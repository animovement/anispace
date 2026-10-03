#' Transform coordinates to an egocentric reference frame
#'
#' @description
#' Places the subject at the centre of its own coordinate system: translating
#' onto a reference member, and then, if asked, rotating it to face a fixed
#' direction. Positions then describe the subject's own geometry rather than
#' where it happened to be in the arena, which is what makes poses comparable
#' across moments and individuals.
#'
#' The rotation comes from one of two places. Alignment points are members
#' whose positions define the axes. Alternatively, `align = "orientation"`
#' uses the subject's declared orientation (`where$orientation`), for data
#' that has one -- FicTrac, rigid-body motion capture, a centroid with a
#' heading -- whether or not it also has keypoints to align on.
#'
#' Translation alone re-centres without changing orientation. Rotation alone
#' is [rotate_coords()], which turns the frame about the coordinate origin
#' rather than about the subject. A declared orientation is turned with the
#' positions, as [rotate_coords()] describes.
#'
#' @section Aligning by orientation:
#' Each subject at each moment is turned by the inverse of its own
#' orientation, so that it faces `+x`. In 2D that is a rotation by `-yaw`,
#' after which its `yaw` is 0. In 3D it is the inverse of its quaternion,
#' after which its body axes lie along the coordinate axes and its
#' orientation is the identity, `(1, 0, 0, 0)`. With `align_perpendicular`,
#' it faces across instead, as for alignment points.
#'
#' Orientation is declared per row, so the subject's is read from its `to`
#' member: the member it is centred on is the one it is turned to face with.
#' Any other member's orientation is turned with it, and so ends up relative
#' to the `to` member's. To centre on one member and face by another's
#' orientation, align on the second and then [translate_coords()] onto the
#' first; translating changes no orientation.
#'
#' A moment whose `to` member has no orientation cannot be aligned, and its
#' positions and orientation come back `NA` rather than unrotated.
#'
#' A single value can never be alignment points, which come in twos and
#' threes, so `"orientation"` is unambiguous even when `level` has a member
#' of that name: `c("orientation", "head")` aligns on that member.
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @param to A value of `level` to place at the origin.
#' @param align Optionally, how to rotate. Two or three values of `level`
#'   define the axes: two give a direction, and in 3D a third fixes the roll
#'   about it. `r lifecycle::badge("experimental")` `"orientation"` turns each
#'   subject to face `+x` by its declared orientation; see below. This option
#'   is experimental, and may change without a deprecation cycle. Omitted, the
#'   frame is re-centred and left as it was oriented.
#' @param level The identity variable `to` and `align` name members of.
#'   Defaults to the frame's only one; a frame declaring several has to be
#'   told.
#' @param align_perpendicular Put the primary axis across the target rather
#'   than along it.
#'
#' @return An aniframe centred on `to`, with `reference_frame` set to
#'   `"egocentric"`.
#' @family coordinate transforms
#' @seealso [translate_coords()] and [rotate_coords()], which this combines.
#' @examples
#' af <- anicore::example_anipoint(n_obs = 3, n_individuals = 1, n_keypoints = 3)
#'
#' # The head becomes the origin, and the head-neck axis points forward
#' transform_to_egocentric(
#'   af,
#'   to = "head",
#'   align = c("head", "neck"),
#'   level = "keypoint"
#' )
#'
#' # Re-centre without reorienting
#' transform_to_egocentric(af, to = "head", level = "keypoint")
#'
#' # Face the way the declared heading says, whatever the keypoints do
#' heading <- anicore::as_anipoint(data.frame(
#'   time = rep(1:2, each = 2),
#'   keypoint = c("centroid", "head"),
#'   x = c(0, 1, 5, 5),
#'   y = c(0, 1, 5, 6),
#'   yaw = rep(c(pi / 4, pi / 2), each = 2)
#' )) |>
#'   anicore::set_variables(where = list(
#'     position = c(x = "x", y = "y"),
#'     orientation = c(yaw = "yaw")
#'   ))
#' transform_to_egocentric(heading, to = "centroid", align = "orientation")
#'
#' @export
transform_to_egocentric <- function(
  data,
  to,
  align = NULL,
  level = NULL,
  align_perpendicular = FALSE
) {
  anicore::ensure_is_anipoint(data)
  anicore::ensure_is_cartesian(data)

  level <- resolve_level(data, level)
  by_orientation <- identical(align, "orientation")
  if (!is.null(align) && !by_orientation && length(align) < 2L) {
    cli::cli_abort(c(
      "{.arg align} must be {.val orientation}, or name two or three values of {.field {level}}.",
      "i" = "One member defines no direction."
    ))
  }

  out <- translate_coords(data, to = to, level = level)

  # Rotating about the origin is correct here precisely because the
  # translation has already put the subject there.
  if (by_orientation) {
    ensure_orientation_of(out, to, level)
    out <- rotate_onto_orientation(out, to, level, align_perpendicular)
  } else if (!is.null(align)) {
    out <- rotate_coords(
      out,
      align = align,
      level = level,
      align_perpendicular = align_perpendicular
    )
  }

  anicore::set_metadata(out, reference_frame = "egocentric")
}


#' Can the frame be aligned by a member's orientation?
#'
#' It needs a declared orientation, and the member has to carry it at least
#' once: a frame recording orientation on one member only, aligned by
#' another's, would otherwise come back entirely `NA`.
#'
#' @param data An aniframe.
#' @param member The member whose orientation is used, known to be one.
#' @param level The identity variable it belongs to.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_orientation_of <- function(
  data,
  member,
  level,
  call = rlang::caller_env()
) {
  orientation <- anicore::get_variables(data, "where", "orientation")
  if (length(orientation) == 0L) {
    cli::cli_abort(
      c(
        "{.code align = \"orientation\"} needs a declared orientation, and this frame has none.",
        "i" = "Declare one with {.code anicore::set_variables(data, where = list(orientation = ))}, or give {.arg align} two or three values of {.field {level}}."
      ),
      call = call
    )
  }
  bare <- dplyr::ungroup(dplyr::as_tibble(data))
  carried <- bare[as.character(bare[[level]]) %in% member, unname(orientation)]
  if (all(is.na(carried))) {
    cli::cli_abort(
      c(
        "{.val {member}} has no orientation at any moment, so nothing could be aligned.",
        "i" = "Centre on the member that carries the orientation, then use {.fn translate_coords} to move onto {.val {member}}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}


#' Turn every subject to face the way its declared orientation says
#'
#' @param data An aniframe, already centred on `member`.
#' @param member The member whose orientation gives the subject's.
#' @param level The identity variable it belongs to.
#' @param align_perpendicular Face across `+x` rather than along it.
#'
#' @return `data`, rotated.
#' @keywords internal
rotate_onto_orientation <- function(data, member, level, align_perpendicular) {
  axes <- cartesian_columns(data)
  grouping <- transform_grouping(data, level)
  target <- rotation_targets(
    length(axes),
    align_perpendicular,
    anicore::get_angle_direction(data)
  )

  rotations <- orientation_rotations(
    data,
    axes,
    member,
    level,
    grouping,
    target
  )
  apply_rotations(data, axes, rotations, grouping)
}


#' The rotation for each subject at each moment, from its orientation
#'
#' The inverse of the member's orientation turns the body's own axes onto the
#' coordinate axes; the same rotation the alignment points would use then
#' takes them to `target`, which is nothing unless facing across. The
#' orientation's turn is composed exactly, rather than read back off the
#' matrix, so the member's `yaw` comes out 0, not a rounding error either
#' side of it.
#'
#' @param data An aniframe.
#' @param axes Named character vector, axis role to column.
#' @param member The member whose orientation gives the subject's.
#' @param level The identity variable it belongs to.
#' @param grouping The columns a rotation is held constant within.
#' @param target Where the body's axes should end up; see
#'   [rotation_targets()].
#'
#' @return As [alignment_rotations()], with `NA` rotations where the member's
#'   orientation is missing.
#' @keywords internal
orientation_rotations <- function(
  data,
  axes,
  member,
  level,
  grouping,
  target
) {
  orientation <- anicore::get_variables(data, "where", "orientation")
  bare <- dplyr::ungroup(dplyr::as_tibble(data))
  groups <- dplyr::distinct(bare[grouping])

  rows <- dplyr::filter(bare, as.character(.data[[level]]) == member)
  found <- suppressMessages(dplyr::left_join(
    groups,
    rows[c(grouping, unname(orientation))],
    by = grouping
  ))

  three_d <- length(axes) >= 3L
  facing <- rotation_for(
    c(1, 0, 0),
    if (three_d) c(0, 1, 0) else NULL,
    target
  )
  facing <- turn_of(facing, length(axes))

  if (three_d) {
    q <- as.matrix(found[unname(orientation[c("qw", "qx", "qy", "qz")])])
    turns <- quat_multiply(facing, quat_conjugate(quat_normalise(q)))
    turns <- lapply(seq_len(nrow(turns)), \(i) turns[i, ])
    groups$.rot <- lapply(turns, \(turn) quat_to_matrix(turn)[,, 1])
  } else {
    yaw <- anicore::angle_to_rad(found[[orientation[["yaw"]]]], data)
    turns <- as.list(-yaw + facing)
    groups$.rot <- lapply(turns, rotation_from_axis_angle, axis = c(0, 0, 1))
  }

  groups$.turn <- turns
  groups
}
