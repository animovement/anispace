#' Map from Cartesian to polar coordinates
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @return An aniframe with `rho` and `phi` in place of `x` and `y`.
#' @family coordinate systems
#' @examples
#' af <- anicore::example_anipoint(
#'   n_obs = 5, n_individuals = 1, n_keypoints = 1
#' )
#' map_to_polar(af)
#' @export
map_to_polar <- function(data) {
  anicore::ensure_is_anipoint(data)
  anicore::ensure_is_cartesian(data)
  data <- data |>
    dplyr::mutate(
      rho = cartesian_to_rho(.data$x, .data$y),
      phi = cartesian_to_phi(.data$x, .data$y)
    ) |>
    dplyr::select(-c("x", "y")) |>
    anicore::set_variables(where = c(rho = "rho", phi = "phi"))

  anicore::ensure_is_polar(data)
  data
}
