#' Map from Cartesian to cylindrical coordinates
#'
#' @param data movement data frame with Cartesian coordinates
#'
#' @return movement data frame with cylindrical coordinates
#' @export
map_to_cylindrical <- function(data) {
  aniframe::ensure_is_aniframe(data)
  aniframe::ensure_is_cartesian(data)

  data <- data |>
    dplyr::mutate(
      rho = cartesian_to_rho(.data$x, .data$y),
      phi = cartesian_to_phi(.data$x, .data$y)
    ) |>
    dplyr::select(-c("x", "y")) |>
    dplyr::relocate("rho", .after = "keypoint") |>
    dplyr::relocate("phi", .after = "rho") |>
    dplyr::relocate("z", .after = "phi")

  aniframe::ensure_is_cylindrical(data)
  aniframe::as_aniframe(data)
}
