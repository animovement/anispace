# Reading the frame instead of naming its columns (#20)
#
# Nothing in this package consulted the metadata: identity was `individual`
# and `keypoint`, the index was `time`, and coordinates were `x`/`y`/`z`.
# All three are declarations a frame may carry under any names, so they have
# to be looked up.

#' The columns a transform must hold constant
#'
#' A reference point is looked up per group, and the groups are everything
#' the frame is identified and positioned by, minus the level the reference
#' belongs to. Getting this wrong is what made `rotate_coords()` join a
#' trial's angle onto every other trial's rows.
#'
#' @param data An aniframe.
#' @param level The identity variable the reference is a member of.
#'
#' @return Character vector of column names.
#' @keywords internal
transform_grouping <- function(data, level) {
  unique(c(
    setdiff(anicore::get_variables_what(data), level),
    anicore::get_variables_when(data),
    anicore::get_index(data)
  ))
}


#' The identity variable a reference point belongs to
#'
#' Not guessed. `variables_what` is documented coarse to fine, but nothing
#' enforces it and attributes like sex or treatment do not nest at all
#' (animovement/anicore#140, animovement/anicore#141), so a frame declaring
#' more than one has to be told which level `to` or `align` name members of.
#'
#' @param data An aniframe.
#' @param level The caller's choice, or `NULL`.
#'
#' @return Length-one character vector naming the column.
#' @keywords internal
resolve_level <- function(data, level = NULL, call = rlang::caller_env()) {
  what <- anicore::get_variables_what(data)

  if (length(what) == 0L) {
    cli::cli_abort(
      c(
        "This aniframe declares no identity variables.",
        "i" = "A reference point is a member of one; see {.fn anicore::set_variables_what}."
      ),
      call = call
    )
  }

  if (is.null(level)) {
    if (length(what) == 1L) {
      return(what)
    }
    cli::cli_abort(
      c(
        "This aniframe declares {length(what)} identity variables, so {.arg level} has to say which the reference belongs to.",
        "i" = "It declares {.val {what}}.",
        "i" = "For example {.code level = {.str {what[[length(what)]]}}}."
      ),
      call = call
    )
  }

  if (!is.character(level) || length(level) != 1L) {
    cli::cli_abort("{.arg level} must be a single column name.", call = call)
  }
  if (!level %in% what) {
    cli::cli_abort(
      c(
        "{.val {level}} is not an identity variable of this aniframe.",
        "i" = "It declares {.val {what}}."
      ),
      call = call
    )
  }
  level
}


#' Are these members of the level?
#'
#' @param data An aniframe.
#' @param members Values the caller named.
#' @param level The identity variable they should belong to.
#' @param arg Name of the argument they came from.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_members <- function(
  data,
  members,
  level,
  arg,
  call = rlang::caller_env()
) {
  present <- unique(as.character(data[[level]]))
  unknown <- setdiff(as.character(members), present)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "{.val {unknown}} {?is/are} not {?a/} value{?s} of {.field {level}}.",
        "i" = "It has {.val {present}}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}


#' The columns carrying the Cartesian axes, in order
#'
#' By role rather than by name, so a frame whose coordinates are called
#' anything works (animovement/anicore#109).
#'
#' @param data An aniframe.
#'
#' @return Named character vector, axis role to column.
#' @keywords internal
cartesian_columns <- function(data, call = rlang::caller_env()) {
  axes <- anicore::get_axes(data)
  roles <- intersect(c("x", "y", "z"), names(axes))

  if (length(roles) < 2L) {
    cli::cli_abort(
      c(
        "A Cartesian transform needs at least two axes, and this frame declares {length(roles)}.",
        "i" = "{.field coordinate_system} is {.val {anicore::get_coordinate_system(data)}}."
      ),
      call = call
    )
  }
  axes[roles]
}


#' Re-declare a transformed frame the way its source was declared
#'
#' A transform changes coordinates, never the declaration, so letting
#' `as_aniframe()` re-detect risks it inventing an identity column and
#' replacing the metadata. The rest of the source's metadata comes with it.
#'
#' @param transformed A plain data frame derived from `source`.
#' @param source The aniframe it came from.
#'
#' @return `transformed` as an aniframe, declared as `source` was.
#' @keywords internal
redeclare_like <- function(transformed, source) {
  out <- anicore::as_aniframe(
    transformed,
    variables_what = anicore::get_variables_what(source),
    variables_when = anicore::get_variables_when(source),
    variables_where = anicore::get_axes(source),
    index = anicore::get_index(source)
  )
  anicore::set_metadata(out, metadata = anicore::get_metadata(source))
}
