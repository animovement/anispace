#' Map to Cartesian coordinates
#'
#' Converts an aniframe back to Cartesian coordinates, detecting whether it is
#' currently polar, cylindrical or spherical.
#'
#' The polar columns are read from the frame's declared axes, so they can
#' have any name, and the angles in the frame's `unit_angle`.
#'
#' @param data An aniframe in a polar, cylindrical or spherical coordinate
#'   system.
#' @return An aniframe with `x` and `y` (and `z`, where the input was
#'   three-dimensional) in place of the polar columns.
#' @family coordinate systems
#' @examples
#' af <- anicore::example_anipoint(n_obs = 5, n_individuals = 1, n_keypoints = 1)
#'
#' # Round-trips back to the coordinates it started from
#' map_to_cartesian(map_to_polar(af))
#' @export
map_to_cartesian <- function(data) {
  anicore::ensure_is_anipoint(data)
  if (anicore::is_polar(data)) {
    data <- map_to_cartesian_polar(data)
  } else if (anicore::is_cylindrical(data)) {
    data <- map_to_cartesian_cylindrical(data)
  } else if (anicore::is_spherical(data)) {
    data <- map_to_cartesian_spherical(data)
  } else {
    cli::cli_abort("Data is neither polar, cylindrical or spherical.")
  }

  anicore::as_anipoint(data)
}

#' @keywords internal
map_to_cartesian_polar <- function(data) {
  anicore::ensure_is_polar(data)
  axes <- anicore::get_axes(data)
  unit <- anicore::get_metadata(data, "unit_angle")

  data |>
    dplyr::mutate(
      .phi = anicore::angle_to_rad(.data[[axes[["phi"]]]], unit),
      x = polar_to_x(.data[[axes[["rho"]]]], .data$.phi),
      y = polar_to_y(.data[[axes[["rho"]]]], .data$.phi)
    ) |>
    dplyr::select(-dplyr::all_of(c(".phi", unname(axes[c("rho", "phi")])))) |>
    anicore::set_variables(where = c(x = "x", y = "y"))
}

#' @keywords internal
map_to_cartesian_cylindrical <- function(data) {
  anicore::ensure_is_cylindrical(data)
  axes <- anicore::get_axes(data)
  unit <- anicore::get_metadata(data, "unit_angle")

  data |>
    dplyr::mutate(
      .phi = anicore::angle_to_rad(.data[[axes[["phi"]]]], unit),
      x = polar_to_x(.data[[axes[["rho"]]]], .data$.phi),
      y = polar_to_y(.data[[axes[["rho"]]]], .data$.phi)
    ) |>
    dplyr::select(-dplyr::all_of(c(".phi", unname(axes[c("rho", "phi")])))) |>
    anicore::set_variables(where = c(x = "x", y = "y", z = axes[["z"]]))
}

#' @keywords internal
map_to_cartesian_spherical <- function(data) {
  anicore::ensure_is_spherical(data)
  axes <- anicore::get_axes(data)
  unit <- anicore::get_metadata(data, "unit_angle")

  data |>
    dplyr::mutate(
      .rho = .data[[axes[["rho"]]]],
      .phi = anicore::angle_to_rad(.data[[axes[["phi"]]]], unit),
      .theta = anicore::angle_to_rad(.data[[axes[["theta"]]]], unit),
      # `rho` is the radial distance, so the projection onto the xy-plane —
      # which is what the polar helpers expect — is rho * sin(theta).
      x = polar_to_x(.data$.rho * sin(.data$.theta), .data$.phi),
      y = polar_to_y(.data$.rho * sin(.data$.theta), .data$.phi),
      z = spherical_to_z(.data$.rho, .data$.theta)
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        ".rho",
        ".phi",
        ".theta",
        unname(axes[c("rho", "phi", "theta")])
      ))
    ) |>
    anicore::set_variables(where = c(x = "x", y = "y", z = "z"))
}
