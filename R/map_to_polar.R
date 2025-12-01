#' Map from Cartesian to polar coordinates
#'
#' @param data movement data frame with Cartesian coordinates
#'
#' @return movement data frame with polar coordinates
#' @export
map_to_polar <- function(data) {
  aniframe::ensure_is_aniframe(data)
  aniframe::ensure_is_cartesian(data)
  data <- data |>
    dplyr::mutate(
      rho = cartesian_to_rho(.data$x, .data$y),
      phi = cartesian_to_phi(.data$x, .data$y)
    ) |>
    dplyr::select(-c("x", "y")) |>
    dplyr::relocate("rho", .after = "keypoint") |>
    dplyr::relocate("phi", .after = "rho")

  aniframe::ensure_is_polar(data)
  aniframe::as_aniframe(data)
}
