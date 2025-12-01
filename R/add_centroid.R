#' Add Centroid to Movement Data
#'
#' @description
#' Calculates and adds a centroid point to movement tracking data. The centroid
#' represents the mean position of selected keypoints at each time point for
#' each individual (and trial/session if present).
#'
#' @param data An aniframe object containing movement tracking data. Must have
#'   Cartesian coordinates (x, y, and/or z columns).
#' @param include_keypoints Optional character vector specifying which keypoints
#'   to use for centroid calculation. If NULL (default), all keypoints are used
#'   unless `exclude_keypoints` is specified. Cannot be used simultaneously with
#'   `exclude_keypoints`.
#' @param exclude_keypoints Optional character vector specifying which keypoints
#'   to exclude from centroid calculation. If NULL (default), no keypoints are
#'   excluded unless `include_keypoints` is specified. Cannot be used simultaneously
#'   with `include_keypoints`.
#' @param centroid_name Character string specifying the name for the centroid
#'   keypoint. Default is "centroid".
#'
#' @return An aniframe object with the same structure as the input, but with an
#'   additional keypoint level representing the centroid. The centroid is calculated
#'   for each combination of grouping variables (individual, time, and trial/session
#'   if present). Coordinate values (x, y, z) are the mean of the selected keypoints,
#'   and the confidence value is set to NA. If a coordinate dimension is not present
#'   in the input data, it will be NA in the centroid. If all values for a coordinate
#'   are NA at a given time point, the centroid coordinate will also be NA.
#'
#' @details
#' The function calculates the centroid as the mean position of the selected
#' keypoints at each time point, respecting all grouping variables in the aniframe
#' (individual, trial, session, etc.). The function handles:
#' - Missing coordinate dimensions (x, y, z) - only calculates means for present dimensions
#' - NA values - uses `na.rm = TRUE` when calculating means
#' - All-NA cases - returns NA for that coordinate
#' - 1D, 2D, or 3D data automatically
#'
#' Keypoints can be selected either by specifying which ones to include
#' (`include_keypoints`) or which ones to exclude (`exclude_keypoints`), but not both.
#' The resulting centroid is added as a new keypoint level to the data.
#'
#' @examples
#' \dontrun{
#' # Add centroid using all keypoints
#' data_with_centroid <- add_centroid(movement_data)
#'
#' # Calculate centroid using only specific keypoints
#' data_with_centroid <- add_centroid(
#'   movement_data,
#'   include_keypoints = c("head", "thorax", "abdomen")
#' )
#'
#' # Calculate centroid excluding certain keypoints
#' data_with_centroid <- add_centroid(
#'   movement_data,
#'   exclude_keypoints = c("antenna_left", "antenna_right"),
#'   centroid_name = "body_centroid"
#' )
#' }
#'
#' @keywords internal
add_centroid <- function(
  data,
  include_keypoints = NULL,
  exclude_keypoints = NULL,
  centroid_name = "centroid"
) {
  # Data validation
  aniframe::ensure_is_aniframe(data)
  aniframe::ensure_is_cartesian(data)

  # Check that centroid isn't there
  # Check that it's a movement data frame
  # To be optimised with collapse later on
  if (!is.null(include_keypoints)) {
    df_centroid <- data |>
      dplyr::filter(.data$keypoint %in% include_keypoints)
  } else if (!is.null(exclude_keypoints)) {
    df_centroid <- data |>
      dplyr::filter(!.data$keypoint %in% exclude_keypoints)
  } else {
    df_centroid <- data
  }

  has_z <- "z" %in% names(data)

  df_centroid <- df_centroid |>
    dplyr::ungroup(.data$keypoint) |>
    dplyr::group_by(.data$time, .add = TRUE) |>
    dplyr::summarise(
      x = if ("x" %in% names(dplyr::pick(dplyr::everything()))) {
        mean(.data$x, na.rm = TRUE)
      } else {
        NA_real_
      },
      y = if ("y" %in% names(dplyr::pick(dplyr::everything()))) {
        mean(.data$y, na.rm = TRUE)
      } else {
        NA_real_
      },
      z = if ("z" %in% names(dplyr::pick(dplyr::everything()))) {
        mean(.data$z, na.rm = TRUE)
      } else {
        NA_real_
      },
      confidence = NA
    ) |>
    dplyr::mutate(keypoint = factor(as.character(centroid_name))) |>
    aniframe::convert_nan_to_na() |>
    aniframe::as_aniframe() |>
    suppressWarnings() |>
    suppressMessages()

  md <- get_metadata(data)
  data <- dplyr::bind_rows(data, df_centroid) |>
    aniframe::as_aniframe() |>
    aniframe::set_metadata(metadata = md)

  if (!has_z) {
    data <- data |>
      dplyr::select(-"z")
  }

  return(data)
}
