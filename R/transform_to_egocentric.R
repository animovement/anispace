#' Transform coordinates to an egocentric reference frame
#'
#' @description
#' Places the subject at the centre of its own coordinate system: translating
#' onto a reference member, and then, if alignment points are given, rotating
#' so they define the axes. Positions then describe the subject's own geometry
#' rather than where it happened to be in the arena, which is what makes poses
#' comparable across moments and individuals.
#'
#' Translation alone re-centres without changing orientation. Rotation alone
#' is [rotate_coords()], which turns the frame about the coordinate origin
#' rather than about the subject.
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @param to A value of `level` to place at the origin.
#' @param align Optionally, two or three values of `level` defining the axes.
#'   Two give a direction; in 3D a third fixes the roll about it. Omitted, the
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
#' af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)
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
#' @export
transform_to_egocentric <- function(
  data,
  to,
  align = NULL,
  level = NULL,
  align_perpendicular = FALSE
) {
  anicore::ensure_is_aniframe(data)
  anicore::ensure_is_cartesian(data)

  level <- resolve_level(data, level)
  out <- translate_coords(data, to = to, level = level)

  if (!is.null(align)) {
    # Rotating about the origin is correct here precisely because the
    # translation has already put the subject there.
    out <- rotate_coords(
      out,
      align = align,
      level = level,
      align_perpendicular = align_perpendicular
    )
  }

  anicore::set_metadata(out, reference_frame = "egocentric")
}
