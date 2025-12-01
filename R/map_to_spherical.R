#' Map from Cartesian to spherical coordinates
#'
#' @param data A data frame/tibble containing columns `x`, `y`, `z`
#' @return Same data frame with columns `rho`, `theta`, `phi`
#' @export
map_to_spherical <- function(data) {
  aniframe::ensure_is_aniframe(data) # your existing sanity check
  aniframe::ensure_is_cartesian(data) # makes sure x, y, z exist

  data <- data |>
    dplyr::mutate(
      rho = cartesian_to_rho(.data$x, .data$y), # distance in xy‑plane
      phi = cartesian_to_phi(.data$x, .data$y), # azimuth (same as cylindrical)
      theta = cartesian_to_theta(.data$x, .data$y, .data$z) # polar angle
    ) |>
    dplyr::select(-c("x", "y", "z")) |>
    dplyr::relocate("rho", .after = "keypoint") |>
    dplyr::relocate("theta", .after = "rho") |>
    dplyr::relocate("phi", .after = "theta")

  aniframe::ensure_is_spherical(data) # optional validator (see below)
  aniframe::as_aniframe(data)
}
