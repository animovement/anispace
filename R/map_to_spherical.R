#' Map from Cartesian to spherical coordinates
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @return An aniframe with `rho`, `phi` and `theta` in place of `x`, `y` and
#'   `z`. `rho` is the radial distance from the origin, `theta` the
#'   inclination from the positive z-axis, and `phi` the azimuth — the
#'   convention of ISO 80000-2.
#' @family coordinate systems
#' @examples
#' af <- anicore::example_aniframe(
#'   n_obs = 5, n_individuals = 1, n_keypoints = 1, n_dims = 3
#' )
#' map_to_spherical(af)
#' @export
map_to_spherical <- function(data) {
  anicore::ensure_is_aniframe(data) # your existing sanity check
  anicore::ensure_is_cartesian(data) # makes sure x, y, z exist

  data <- data |>
    dplyr::mutate(
      rho = cartesian_to_rho(.data$x, .data$y, .data$z), # radial distance
      phi = cartesian_to_phi(.data$x, .data$y), # azimuth (same as cylindrical)
      theta = cartesian_to_theta(.data$x, .data$y, .data$z) # polar angle
    ) |>
    dplyr::select(-c("x", "y", "z")) |>
    anicore::set_variables_where(c("rho", "phi", "theta"))

  anicore::ensure_is_spherical(data) # optional validator (see below)
  data
}
