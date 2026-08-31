#' Translate coordinates in Cartesian space
#'
#' @description
#' Moves the origin: to a fixed offset, or onto a member of the frame's
#' identity — a keypoint, an animal, or whatever level the frame declares.
#' Translating onto a member is how coordinates are made relative to the
#' subject rather than the arena.
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @param to A value of `level` to place at the origin. All other coordinates
#'   become relative to it.
#' @param level The identity variable `to` is a member of. Defaults to the
#'   frame's only one; a frame declaring several has to be told.
#' @param by Named numeric giving a fixed offset per axis role, e.g.
#'   `c(x = 100, y = 50)`. The offset moves the *origin*, as `to` does, so the
#'   coordinates shift by the negative of it: `c(x = 100)` moves every point
#'   100 to the left. Mutually exclusive with `to`.
#'
#' @return An aniframe with translated coordinates.
#' @family coordinate transforms
#' @examples
#' af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)
#'
#' # Everything becomes relative to the head, which sits at the origin
#' translate_coords(af, to = "head", level = "keypoint")
#'
#' # Or move the origin by a fixed amount, which shifts the coordinates the
#' # other way: x becomes x - 100
#' translate_coords(af, by = c(x = 100, y = 50))
#'
#' @export
translate_coords <- function(data, to = NULL, level = NULL, by = NULL) {
  anicore::ensure_is_aniframe(data)
  anicore::ensure_is_cartesian(data)

  if (!is.null(to) && !is.null(by)) {
    cli::cli_abort(c(
      "Give either {.arg to} or {.arg by}, not both.",
      "i" = "{.arg to} moves the origin onto a member; {.arg by} shifts it a fixed amount."
    ))
  }
  if (is.null(to) && is.null(by)) {
    cli::cli_abort(c(
      "Nothing to translate to.",
      "i" = "Give {.arg to} to centre on a member, or {.arg by} for a fixed offset."
    ))
  }

  axes <- cartesian_columns(data)

  if (!is.null(by)) {
    return(translate_by_offset(data, axes, by))
  }

  if (!is.character(to) || length(to) != 1L) {
    cli::cli_abort("{.arg to} must be a single value of {.arg level}.")
  }
  level <- resolve_level(data, level)
  ensure_members(data, to, level, "to")

  translate_onto_member(data, axes, to, level)
}


#' Shift every coordinate by a fixed offset
#'
#' @param data An aniframe.
#' @param axes Named character vector, axis role to column.
#' @param by Named numeric offset per axis role.
#'
#' @return `data`, translated.
#' @keywords internal
translate_by_offset <- function(data, axes, by, call = rlang::caller_env()) {
  if (!is.numeric(by) || is.null(names(by))) {
    cli::cli_abort(
      c(
        "{.arg by} must name an axis role for every offset.",
        "i" = "For example {.code c(x = 100, y = 50)}."
      ),
      call = call
    )
  }
  unknown <- setdiff(names(by), names(axes))
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "{.val {unknown}} {?is/are} not {?an/} axis{?/es} of this aniframe.",
        "i" = "It has {.val {names(axes)}}."
      ),
      call = call
    )
  }

  for (role in names(by)) {
    column <- axes[[role]]
    data[[column]] <- data[[column]] - by[[role]]
  }
  data
}


#' Move the origin onto one member of an identity level
#'
#' The reference is looked up per group -- everything the frame is identified
#' and positioned by, except the level the member belongs to -- so each
#' subject at each moment is centred on its own reference rather than on the
#' first one found (#20).
#'
#' @param data An aniframe.
#' @param axes Named character vector, axis role to column.
#' @param to The member to place at the origin.
#' @param level The identity variable it belongs to.
#'
#' @return `data`, translated.
#' @keywords internal
translate_onto_member <- function(data, axes, to, level, sign = -1) {
  columns <- unname(axes)
  offsets <- member_offsets(data, axes, to, level)

  shifted <- dplyr::ungroup(dplyr::as_tibble(data))
  for (column in columns) {
    shifted[[column]] <- shifted[[column]] + sign * offsets[[column]]
  }
  redeclare_like(shifted, data)
}


#' Where the reference member sits, for every row
#'
#' One offset per row, looked up in the row's own group, so each subject at
#' each moment is measured against its own reference rather than the first
#' one found (#20).
#'
#' @param data An aniframe.
#' @param axes Named character vector, axis role to column.
#' @param to The reference member.
#' @param level The identity variable it belongs to.
#'
#' @return A data frame of coordinate columns, aligned to `data`'s rows.
#' @keywords internal
member_offsets <- function(data, axes, to, level) {
  columns <- unname(axes)
  grouping <- transform_grouping(data, level)

  bare <- dplyr::ungroup(dplyr::as_tibble(data))
  reference <- bare |>
    dplyr::filter(as.character(.data[[level]]) == to) |>
    dplyr::select(dplyr::all_of(c(grouping, columns)))

  joined <- suppressMessages(dplyr::left_join(
    bare[grouping],
    reference,
    by = grouping
  ))
  joined[columns]
}
