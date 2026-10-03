#' Map from Cartesian to spherical coordinates
#'
#' The Cartesian columns are read from the frame's declared axes, so they can
#' have any name. `phi` and `theta` are written in the frame's `unit_angle`.
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @return An aniframe with `rho`, `phi` and `theta` in place of `x`, `y` and
#'   `z`. `rho` is the radial distance from the origin, `theta` the
#'   inclination from the positive z-axis, and `phi` the azimuth — the
#'   convention of ISO 80000-2.
#' @family coordinate systems
#' @examples
#' af <- anicore::example_anipoint(
#'   n_obs = 5, n_individuals = 1, n_keypoints = 1, n_dims = 3
#' )
#' map_to_spherical(af)
#' @export
map_to_spherical <- function(data) {
  anicore::ensure_is_anipoint(data)
  anicore::ensure_is_cartesian(data)
  axes <- anicore::get_axes(data)
  unit <- anicore::get_metadata(data, "unit_angle")
  x <- axes[["x"]]
  y <- axes[["y"]]
  z <- axes[["z"]]

  data <- data |>
    dplyr::mutate(
      rho = cartesian_to_rho(.data[[x]], .data[[y]], .data[[z]]),
      phi = anicore::angle_from_rad(
        cartesian_to_phi(.data[[x]], .data[[y]]),
        unit
      ),
      theta = anicore::angle_from_rad(
        cartesian_to_theta(.data[[x]], .data[[y]], .data[[z]]),
        unit
      )
    ) |>
    dplyr::select(-dplyr::all_of(c(x, y, z))) |>
    anicore::set_variables(where = c(rho = "rho", phi = "phi", theta = "theta"))

  anicore::ensure_is_spherical(data)
  data
}
