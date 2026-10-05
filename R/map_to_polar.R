#' Map from Cartesian to polar coordinates
#'
#' The Cartesian columns are read from the frame's declared axes, so they can
#' have any name. `phi` is written in the frame's `unit_angle`, in `(-pi, pi]`
#' or `(-180, 180]` (see [cartesian_to_phi()]).
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
  axes <- anicore::get_axes(data)
  unit <- anicore::get_metadata(data, "unit_angle")

  data <- data |>
    dplyr::mutate(
      rho = cartesian_to_rho(.data[[axes[["x"]]]], .data[[axes[["y"]]]]),
      phi = anicore::angle_from_rad(
        cartesian_to_phi(.data[[axes[["x"]]]], .data[[axes[["y"]]]]),
        unit
      )
    ) |>
    dplyr::select(-dplyr::all_of(unname(axes[c("x", "y")]))) |>
    anicore::set_variables(where = c(rho = "rho", phi = "phi"))

  anicore::ensure_is_polar(data)
  data
}
