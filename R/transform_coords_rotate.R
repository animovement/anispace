#' Rotate coordinates in Cartesian space
#'
#' @description
#' Rotates each subject's coordinates so that chosen members of its identity
#' define the axes. Two members give a direction; in three dimensions a third
#' fixes the roll about it, which two cannot.
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
#' @return An aniframe with rotated coordinates.
#' @family coordinate transforms
#' @examples
#' af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)
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
  anicore::ensure_is_aniframe(data)
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
  columns <- unname(axes)
  grouping <- transform_grouping(data, level)
  bare <- dplyr::ungroup(dplyr::as_tibble(data))

  # Putting a stored vector onto stored +x is the same operation whichever
  # way the frame says its angles run. What the sense does decide is which
  # quarter turn "perpendicular" means (#29).
  target <- rotation_targets(
    length(axes),
    align_perpendicular,
    anicore::get_angle_direction(data)
  )

  point <- function(member) {
    rows <- bare[as.character(bare[[level]]) == member, , drop = FALSE]
    rows[c(grouping, columns)]
  }
  points <- lapply(align, point)

  reference <- points[[1]][grouping]
  vectors <- lapply(points[-1], \(p) {
    as.matrix(p[columns]) - as.matrix(points[[1]][columns])
  })

  rotations <- vector("list", nrow(reference))
  for (i in seq_len(nrow(reference))) {
    primary <- pad3(vectors[[1]][i, ], length(axes))
    secondary <- if (length(vectors) > 1) {
      pad3(vectors[[2]][i, ], length(axes))
    } else {
      NULL
    }
    rotations[[i]] <- rotation_for(primary, secondary, target)
  }

  reference$.rot <- rotations
  joined <- suppressMessages(dplyr::left_join(bare, reference, by = grouping))

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

  joined |>
    dplyr::select(-".rot") |>
    redeclare_like(data)
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
