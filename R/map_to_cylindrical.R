#' Map from Cartesian to cylindrical coordinates
#'
#' @param data An aniframe in a Cartesian coordinate system.
#' @return An aniframe with `rho` and `phi` in place of `x` and `y`, keeping `z`.
#' @family coordinate systems
#' @examples
#' af <- anicore::example_aniframe(
#'   n_obs = 5, n_individuals = 1, n_keypoints = 1, n_dims = 3
#' )
#' map_to_cylindrical(af)
#' @export
map_to_cylindrical <- function(data) {
  anicore::ensure_is_aniframe(data)
  anicore::ensure_is_cartesian(data)

  data <- data |>
    dplyr::mutate(
      rho = cartesian_to_rho(.data$x, .data$y),
      phi = cartesian_to_phi(.data$x, .data$y)
    ) |>
    dplyr::select(-c("x", "y")) |>
    dplyr::relocate("rho", .after = "keypoint") |>
    dplyr::relocate("phi", .after = "rho") |>
    dplyr::relocate("z", .after = "phi")

  anicore::ensure_is_cylindrical(data)
  anicore::as_aniframe(data)
}
