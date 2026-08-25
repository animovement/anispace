#' Map to Cartesian coordinates
#'
#' Converts an aniframe back to Cartesian coordinates, detecting whether it is
#' currently polar, cylindrical or spherical.
#'
#' @param data An aniframe in a polar, cylindrical or spherical coordinate
#'   system.
#' @return An aniframe with `x` and `y` (and `z`, where the input was
#'   three-dimensional) in place of the polar columns.
#' @family coordinate systems
#' @examples
#' af <- aniframe::example_aniframe(n_obs = 5, n_individuals = 1, n_keypoints = 1)
#'
#' # Round-trips back to the coordinates it started from
#' map_to_cartesian(map_to_polar(af))
#' @export
map_to_cartesian <- function(data) {
  aniframe::ensure_is_aniframe(data)
  if (aniframe::is_polar(data)) {
    data <- map_to_cartesian_polar(data)
  } else if (aniframe::is_cylindrical(data)) {
    data <- map_to_cartesian_cylindrical(data)
  } else if (aniframe::is_spherical(data)) {
    data <- map_to_cartesian_spherical(data)
  } else {
    cli::cli_abort("Data is neither polar, cylindrical or spherical.")
  }

  aniframe::as_aniframe(data)
}

#' @keywords internal
map_to_cartesian_polar <- function(data) {
  aniframe::ensure_is_polar(data)
  data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$phi),
      y = polar_to_y(.data$rho, .data$phi)
    ) |>
    dplyr::select(-c("rho", "phi"))
}

#' @keywords internal
map_to_cartesian_cylindrical <- function(data) {
  aniframe::ensure_is_cylindrical(data)
  data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$phi),
      y = polar_to_y(.data$rho, .data$phi),
      z = .data$z
    ) |>
    dplyr::select(-c("rho", "phi"))
}

#' @keywords internal
map_to_cartesian_spherical <- function(data) {
  aniframe::ensure_is_spherical(data)
  data |>
    dplyr::mutate(
      # `rho` is the radial distance, so the projection onto the xy-plane —
      # which is what the polar helpers expect — is rho * sin(theta).
      x = polar_to_x(.data$rho * sin(.data$theta), .data$phi),
      y = polar_to_y(.data$rho * sin(.data$theta), .data$phi),
      z = spherical_to_z(.data$rho, .data$theta)
    ) |>
    dplyr::select(-c("rho", "phi", "theta"))
}
