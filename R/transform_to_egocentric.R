#' Transform coordinates to an egocentric reference frame
#'
#' Places the animal at the centre of its own coordinate system, by translating
#' all coordinates onto a reference keypoint and then rotating so that two
#' chosen keypoints define the forward axis. Positions then describe the
#' animal's own geometry rather than where it happened to be in the arena,
#' which is what makes poses comparable across frames and individuals.
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @param to_keypoint A character string naming the keypoint to place at the
#'   origin.
#' @param alignment_points A character vector of length 2 naming the keypoints
#'   that define the axis.
#' @param align_perpendicular A logical value (default `FALSE`) determining the
#'   axis of alignment. `FALSE` aligns `alignment_points` with the forward axis;
#'   `TRUE` rotates them perpendicular to it.
#' @return An aniframe with translated and rotated coordinates, in which
#'   `to_keypoint` sits at the origin.
#' @family coordinate transforms
#' @seealso [translate_coords()] and [rotate_coords()], which this combines.
#' @examples
#' af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)
#'
#' # The head becomes the origin, and the head-neck axis points forward
#' transform_to_egocentric(
#'   af,
#'   to_keypoint = "head",
#'   alignment_points = c("head", "neck")
#' )
#'
#' # Aligning perpendicular instead puts that axis across the forward direction
#' transform_to_egocentric(
#'   af,
#'   to_keypoint = "head",
#'   alignment_points = c("head", "neck"),
#'   align_perpendicular = TRUE
#' )
#' @export
transform_to_egocentric <- function(
  data,
  to_keypoint, # Reference point for translation
  alignment_points, # Two keypoint names to use for alignment
  align_perpendicular = FALSE # If TRUE, alignment_points will be made perpendicular to 0°
) {
  # First translate
  translated_data <- translate_coords(data, to_keypoint = to_keypoint)

  # Then rotate
  transformed_data <- rotate_coords(
    translated_data,
    alignment_points,
    align_perpendicular
  )

  transformed_data <- transformed_data |>
    anicore::as_aniframe() |>
    anicore::set_metadata(
      reference_frame = "egocentric"
    )

  transformed_data
}
