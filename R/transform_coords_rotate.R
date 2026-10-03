#' Rotate coordinates in Cartesian space
#'
#' @description
#' Rotates each subject's coordinates so that chosen members of its identity
#' define the axes. Two members give a direction; in three dimensions a third
#' fixes the roll about it, which two cannot.
#'
#' A declared orientation (`where$orientation`) turns with the coordinates,
#' by the same rotation, so the two keep describing the same body: in 2D the
#' rotation's angle is added to `yaw`, in the frame's `unit_angle` and wrapped
#' to the range the input used (signed if any value is negative); in 3D the
#' quaternion is pre-multiplied by the rotation's, `quat_multiply(r, q)`
#' (see [quaternions]). The centre of rotation moves positions but not
#' orientation. A moment whose rotation is undefined, because an alignment
#' point is missing, is left as it was, orientation included.
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @param align Two or three values of `level`. The first two define the
#'   primary axis. A third, in 3D, defines the plane and so the orientation
#'   outright.
#' @param level The identity variable `align` names members of. Defaults to
#'   the frame's only one; a frame declaring several has to be told.
#' @param about Centre of rotation: a value of `level` to rotate around, or a
#'   named numeric such as `c(x = 500, y = 500)`. Defaults to the coordinate
#'   origin, which is what to rotate about once the frame has been translated
#'   onto its subject.
#' @param align_perpendicular Put the primary axis across the target rather
#'   than along it.
#'
#' @return An aniframe with rotated coordinates, and orientation if declared.
#' @family coordinate transforms
#' @examples
#' af <- anicore::example_anipoint(n_obs = 3, n_individuals = 1, n_keypoints = 3)
#'
#' # Align the head-neck axis with x, rotating about the origin
#' rotate_coords(af, align = c("head", "neck"), level = "keypoint")
#'
#' # Rotate each animal about its own head instead
#' rotate_coords(af, align = c("head", "neck"), level = "keypoint", about = "head")
#'
#' @export
rotate_coords <- function(
  data,
  align,
  level = NULL,
  about = NULL,
  align_perpendicular = FALSE
) {
  anicore::ensure_is_anipoint(data)
  anicore::ensure_is_cartesian(data)

  axes <- cartesian_columns(data)
  level <- resolve_level(data, level)

  if (!is.character(align) || !length(align) %in% c(2L, 3L)) {
    cli::cli_abort(c(
      "{.arg align} must name two or three values of {.field {level}}.",
      "i" = "Two give a direction; in 3D a third fixes the roll about it."
    ))
  }
  if (length(align) == 3L && length(axes) < 3L) {
    cli::cli_abort(c(
      "A third alignment point only means something in three dimensions.",
      "i" = "This frame declares {.val {names(axes)}}."
    ))
  }
  ensure_members(data, align, level, "align")

  # Rotation is about the origin, so rotating about anything else means
  # bringing it there first and putting the frame back afterwards. A centre
  # that is left where it was is what "rotate about it" means.
  if (is.null(about)) {
    return(rotate_about_origin(data, axes, align, level, align_perpendicular))
  }

  if (is.character(about)) {
    ensure_members(data, about, level, "about")
    centred <- translate_onto_member(data, axes, about, level)
    rotated <- rotate_about_origin(
      centred,
      axes,
      align,
      level,
      align_perpendicular
    )
    return(translate_onto_member_back(rotated, data, axes, about, level))
  }

  centred <- translate_by_offset(data, axes, about)
  rotated <- rotate_about_origin(
    centred,
    axes,
    align,
    level,
    align_perpendicular
  )
  translate_by_offset(rotated, axes, -about)
}


#' Put a frame back where its reference member was
#'
#' The offsets come from the frame as it was before centring, since the
#' member sits at the origin afterwards and no longer knows where it came
#' from.
#'
#' @param rotated The frame after rotation.
#' @param original The frame before centring.
#' @param axes Named character vector, axis role to column.
#' @param about The member it was centred on.
#' @param level The identity variable it belongs to.
#'
#' @return `rotated`, shifted back.
#' @keywords internal
translate_onto_member_back <- function(rotated, original, axes, about, level) {
  columns <- unname(axes)
  offsets <- member_offsets(original, axes, about, level)

  out <- dplyr::ungroup(dplyr::as_tibble(rotated))
  for (column in columns) {
    out[[column]] <- out[[column]] + offsets[[column]]
  }
  redeclare_like(out, original)
}


#' Rotate every subject's coordinates about the origin
#'
#' The rotation is worked out per group -- everything the frame is identified
#' and positioned by, except the level the alignment points belong to -- so
#' each subject at each moment gets its own. Reading the grouping from the
#' frame rather than assuming `individual` and `time` is what stops a second
#' trial's angle being applied to the first's rows (#20).
#'
#' @param data An aniframe.
#' @param axes Named character vector, axis role to column.
#' @param align Values of `level` defining the axes.
#' @param level The identity variable they belong to.
#' @param align_perpendicular Put the primary axis across the target.
#'
#' @return `data`, rotated.
#' @keywords internal
rotate_about_origin <- function(
  data,
  axes,
  align,
  level,
  align_perpendicular = FALSE
) {
  grouping <- transform_grouping(data, level)

  # Putting a stored vector onto stored +x is the same operation whichever
  # way the frame says its angles run. What the sense does decide is which
  # quarter turn "perpendicular" means (#29).
  target <- rotation_targets(
    length(axes),
    align_perpendicular,
    anicore::get_angle_direction(data)
  )

  rotations <- alignment_rotations(data, axes, align, level, grouping, target)
  apply_rotations(data, axes, rotations, grouping)
}


#' The rotation for each subject at each moment, from its alignment points
#'
#' Each alignment point is looked up in every group by joining on the
#' grouping, not by position, so a member missing from a moment leaves that
#' moment without a rotation rather than pairing the points of different
#' moments.
#'
#' @param data An aniframe.
#' @param axes Named character vector, axis role to column.
#' @param align Values of `level` defining the axes.
#' @param level The identity variable they belong to.
#' @param grouping The columns a rotation is held constant within.
#' @param target Where the alignment axes should end up; see
#'   [rotation_targets()].
#'
#' @return A tibble with one row per group: the `grouping` columns; `.rot`,
#'   a list of 3x3 rotation matrices, `NULL` where the rotation is undefined;
#'   and `.turn`, the same rotations as [turn_of()] gives them.
#' @keywords internal
alignment_rotations <- function(data, axes, align, level, grouping, target) {
  columns <- unname(axes)
  bare <- dplyr::ungroup(dplyr::as_tibble(data))
  groups <- dplyr::distinct(bare[grouping])

  point <- function(member) {
    rows <- dplyr::filter(bare, as.character(.data[[level]]) == member)
    found <- suppressMessages(dplyr::left_join(
      groups,
      rows[c(grouping, columns)],
      by = grouping
    ))
    as.matrix(found[columns])
  }
  points <- lapply(align, point)
  vectors <- lapply(points[-1], \(p) p - points[[1]])

  rotations <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    primary <- pad3(vectors[[1]][i, ], length(axes))
    secondary <- if (length(vectors) > 1) {
      pad3(vectors[[2]][i, ], length(axes))
    } else {
      NULL
    }
    # `[<-` with `list()` keeps a `NULL`; `[[<-` would drop the element and
    # shift every later rotation onto the wrong moment.
    rotations[i] <- list(rotation_for(primary, secondary, target))
  }

  groups$.rot <- rotations
  groups$.turn <- lapply(rotations, turn_of, n_axes = length(axes))
  groups
}


#' A rotation as it turns an orientation
#'
#' @param rotation A 3x3 rotation matrix, or `NULL`.
#' @param n_axes How many spatial axes the frame has.
#'
#' @return `NULL` for `NULL`. In 2D, the angle in radians it turns about `z`;
#'   in 3D, its quaternion.
#' @keywords internal
turn_of <- function(rotation, n_axes) {
  if (is.null(rotation)) {
    return(NULL)
  }
  if (n_axes < 3L) {
    return(atan2(rotation[2, 1], rotation[1, 1]))
  }
  quat_from_matrix(rotation)[1, ]
}


#' Apply a rotation per group to the coordinates and orientation
#'
#' @param data An aniframe.
#' @param axes Named character vector, axis role to column.
#' @param rotations One row per group, as from [alignment_rotations()]:
#'   `.rot` turns the positions and `.turn` the orientation, and the two must
#'   be the same rotation. A rotation of `NA`s makes the rows it applies to
#'   `NA`.
#' @param grouping The columns to join `rotations` on.
#'
#' @return `data`, rotated.
#' @keywords internal
apply_rotations <- function(data, axes, rotations, grouping) {
  columns <- unname(axes)
  bare <- dplyr::ungroup(dplyr::as_tibble(data))
  joined <- suppressMessages(dplyr::left_join(bare, rotations, by = grouping))

  coords <- as.matrix(joined[columns])
  out <- coords
  for (i in seq_len(nrow(coords))) {
    rotation <- joined$.rot[[i]]
    if (!is.null(rotation)) {
      out[i, ] <- (rotation %*% pad3(coords[i, ], length(axes)))[seq_along(
        columns
      )]
    }
  }
  joined[columns] <- out
  joined <- rotate_orientation(joined, joined$.turn, data)

  joined |>
    dplyr::select(-c(".rot", ".turn")) |>
    redeclare_like(data)
}


#' Turn a declared orientation by the rotation applied to the positions
#'
#' In 2D, `yaw` is measured from `x` toward `y`, the sense a rotation about
#' `z` turns in, so the rotation's angle is added to it. In 3D, the
#' quaternion expresses the body's axes in the frame's coordinates, so a
#' rotation of the frame's coordinates pre-multiplies it,
#' `quat_multiply(r, q)`. The centre of rotation plays no part; orientation
#' is a direction, not a place.
#'
#' @param rows A data frame holding the orientation columns.
#' @param turns A list, one element per row of `rows`: the rotation as
#'   [turn_of()] gives it, or `NULL` to leave the row as it is.
#' @param data The aniframe the orientation is declared on.
#'
#' @return `rows`, with the orientation columns turned.
#' @keywords internal
rotate_orientation <- function(rows, turns, data) {
  orientation <- anicore::get_variables(data, "where", "orientation")
  turned <- !vapply(turns, is.null, logical(1))
  if (length(orientation) == 0L || !any(turned)) {
    return(rows)
  }

  if ("yaw" %in% names(orientation)) {
    column <- orientation[["yaw"]]
    yaw <- rows[[column]]
    radians <- anicore::angle_to_rad(yaw[turned], data) + unlist(turns[turned])
    signed <- any(yaw < 0, na.rm = TRUE)
    rows[[column]][turned] <- anicore::angle_from_rad(
      wrap_like(radians, signed),
      data
    )
    return(rows)
  }

  columns <- unname(orientation[c("qw", "qx", "qy", "qz")])
  q <- as.matrix(rows[turned, columns])
  r <- do.call(rbind, turns[turned])
  turned_q <- quat_normalise(quat_multiply(r, q))
  for (i in seq_along(columns)) {
    rows[[columns[[i]]]][turned] <- turned_q[, i]
  }
  rows
}


#' Wrap angles to the range their source used
#'
#' Signed, `(-pi, pi]`, or unsigned, `[0, 2 pi)`, as `anicore::reflect_axis()`
#' decides it. A value within a rounding error of where the range wraps can
#' land exactly on the end it excludes -- `-1e-17` wraps to `2 pi` -- so
#' that end is folded onto the other, which is the same angle.
#'
#' @param radians Numeric vector of angles in radians.
#' @param signed Wrap to `(-pi, pi]` rather than `[0, 2 pi)`.
#'
#' @return `radians`, wrapped.
#' @keywords internal
wrap_like <- function(radians, signed) {
  if (signed) {
    wrapped <- anicore::wrap_angle(radians, modulo = "pi")
    wrapped[!is.na(wrapped) & wrapped <= -pi] <- pi
  } else {
    wrapped <- anicore::wrap_angle(radians, modulo = "2pi")
    wrapped[!is.na(wrapped) & wrapped >= 2 * pi] <- 0
  }
  wrapped
}


#' Where the alignment axes should end up
#'
#' The primary axis goes onto x. Put across it instead, it goes onto y -- or
#' onto -y on a frame whose angles run clockwise, since a quarter turn there
#' goes the other way round.
#'
#' @param n_axes How many spatial axes the frame has.
#' @param align_perpendicular Put the primary axis across the target.
#' @param sense The frame's `angle_direction`.
#'
#' @return A list of two length-3 target vectors.
#' @keywords internal
rotation_targets <- function(n_axes, align_perpendicular, sense = "unknown") {
  x <- c(1, 0, 0)
  y <- c(0, 1, 0)
  z <- c(0, 0, 1)

  if (!align_perpendicular) {
    return(list(primary = x, secondary = y))
  }

  quarter_turn <- if (identical(sense, "clockwise")) -y else y
  list(primary = quarter_turn, secondary = if (n_axes >= 3) z else x)
}


#' The rotation for one subject at one moment
#'
#' @param primary The vector between the first two alignment points.
#' @param secondary The vector to the third, or `NULL`.
#' @param target Where they should end up.
#'
#' @return A 3x3 rotation matrix.
#' @keywords internal
rotation_for <- function(primary, secondary, target) {
  if (anyNA(primary)) {
    return(NULL)
  }

  if (is.null(secondary) || anyNA(secondary)) {
    rotation <- rotation_onto(primary, target$primary)
  } else {
    rotation <- rotation_onto_basis(
      primary,
      secondary,
      target$primary,
      target$secondary
    )
  }

  rotation
}


#' Pad a coordinate to three dimensions
#'
#' 2D is the case where the third component is zero and the rotation axis is
#' fixed to z, so both dimensionalities go through the same matrices.
#'
#' @param v A numeric vector.
#' @param n How many dimensions it came from.
#'
#' @return A numeric vector of length 3.
#' @keywords internal
pad3 <- function(v, n) {
  if (n >= 3) as.numeric(v[1:3]) else c(as.numeric(v[1:2]), 0)
}
