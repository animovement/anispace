# -------------------------------------------------------------
# tests/testthat/test-map_to_cartesian.R
# -------------------------------------------------------------

# Testing:
# - map_to_cartesian_polar() drops polar columns and creates x/y
# - map_to_cartesian_cylindrical() retains original z
# - map_to_cartesian_spherical() correctly computes z via spherical_to_z()
# - map_to_cartesian() dispatches to the correct helper (polar)
# - map_to_cartesian() dispatches to the correct helper (cylindrical)
# - map_to_cartesian() dispatches to the correct helper (spherical)
# - map_to_cartesian() aborts with a clear message for unknown systems
# - map_to_cartesian_*() works with empty data frames (zero rows)
# - map_to_cartesian_*() preserves additional non-coordinate columns
# - Cartesian results from the three systems are mutually consistent when the inputs represent the same point

get_coordinate_system <- function(df) {
  aniframe::get_metadata(df, "coordinate_system")
}

make_polar_df <- function() {
  dplyr::tibble(
    keypoint = 1:3,
    time = seq(1:3),
    rho = c(1, sqrt(2), 0),
    phi = c(0, pi / 4, pi / 2)
  ) |>
    aniframe::as_aniframe()
}

make_cylindrical_df <- function() {
  dplyr::tibble(
    keypoint = 1:3,
    time = seq(1:3),
    rho = c(1, sqrt(2), 0),
    phi = c(0, pi / 4, pi / 2),
    z = c(0, 5, -3)
  ) |>
    aniframe::as_aniframe()
}

make_spherical_df <- function() {
  dplyr::tibble(
    keypoint = 1:3,
    time = seq(1:3),
    rho = c(1, sqrt(2), 0),
    phi = c(0, pi / 4, pi / 2),
    theta = c(pi / 3, pi / 2, 0)
  ) |>
    aniframe::as_aniframe()
}

test_that("map_to_cartesian_polar() drops polar columns and creates x/y", {
  df_in <- make_polar_df()
  df_out <- map_to_cartesian_polar(df_in) |> aniframe::as_aniframe()

  expect_equal(df_out$x, polar_to_x(df_in$rho, df_in$phi))
  expect_equal(df_out$y, polar_to_y(df_in$rho, df_in$phi))

  expect_false(any(c("rho", "phi") %in% colnames(df_out)))

  expect_equal(get_coordinate_system(df_out) |> as.character(), "cartesian_2d")
})

test_that("map_to_cartesian_cylindrical() retains original z", {
  df_in <- make_cylindrical_df()
  df_out <- map_to_cartesian_cylindrical(df_in) |> aniframe::as_aniframe()

  expect_equal(df_out$x, polar_to_x(df_in$rho, df_in$phi))
  expect_equal(df_out$y, polar_to_y(df_in$rho, df_in$phi))
  expect_equal(df_out$z, df_in$z)

  expect_false(any(c("rho", "phi") %in% colnames(df_out)))
  expect_equal(get_coordinate_system(df_out) |> as.character(), "cartesian_3d")
})

test_that("map_to_cartesian_spherical() correctly computes z via spherical_to_z()", {
  df_in <- make_spherical_df()
  df_out <- map_to_cartesian_spherical(df_in) |> aniframe::as_aniframe()

  expect_equal(df_out$x, polar_to_x(df_in$rho, df_in$phi))
  expect_equal(df_out$y, polar_to_y(df_in$rho, df_in$phi))
  expect_equal(df_out$z, spherical_to_z(df_in$rho, df_in$theta))

  expect_false(any(c("rho", "phi", "theta") %in% colnames(df_out)))
  expect_equal(get_coordinate_system(df_out) |> as.character(), "cartesian_3d")
})

test_that("map_to_cartesian() dispatches to the correct helper (polar)", {
  df_in <- make_polar_df()
  df_out <- map_to_cartesian(df_in)
  df_correct <- map_to_cartesian_polar(df_in) |> aniframe::as_aniframe()

  expect_identical(df_out, df_correct)
})

test_that("map_to_cartesian() dispatches to the correct helper (cylindrical)", {
  df_in <- make_cylindrical_df()
  df_out <- map_to_cartesian(df_in)
  df_correct <- map_to_cartesian_cylindrical(df_in) |> aniframe::as_aniframe()

  expect_identical(df_out, df_correct)
})

test_that("map_to_cartesian() dispatches to the correct helper (spherical)", {
  df_in <- make_spherical_df()
  df_out <- map_to_cartesian(df_in)
  df_correct <- map_to_cartesian_spherical(df_in) |> aniframe::as_aniframe()

  expect_identical(df_out, df_correct)
})

test_that("map_to_cartesian() aborts with a clear message for unknown systems", {
  bad_df <- dplyr::tibble(keypoint = 1, a = 1, b = 2)

  expect_error(
    map_to_cartesian(bad_df)
  )
})

test_that("map_to_cartesian_*() works with empty data frames (zero rows)", {
  empty_polar <- dplyr::tibble(
    keypoint = integer(),
    time = numeric(),
    rho = numeric(),
    phi = numeric()
  ) |>
    aniframe::as_aniframe()
  empty_cyl <- dplyr::tibble(
    keypoint = integer(),
    time = numeric(),
    rho = numeric(),
    phi = numeric(),
    z = numeric()
  ) |>
    aniframe::as_aniframe()
  empty_sph <- dplyr::tibble(
    keypoint = integer(),
    time = as.numeric(),
    rho = numeric(),
    phi = numeric(),
    theta = numeric()
  ) |>
    aniframe::as_aniframe()

  expect_equal(nrow(map_to_cartesian_polar(empty_polar)), 0L)
  expect_equal(nrow(map_to_cartesian_cylindrical(empty_cyl)), 0L)
  expect_equal(nrow(map_to_cartesian_spherical(empty_sph)), 0L)

  expect_equal(nrow(map_to_cartesian(empty_polar)), 0L)
  expect_equal(nrow(map_to_cartesian(empty_cyl)), 0L)
  expect_equal(nrow(map_to_cartesian(empty_sph)), 0L)
})

test_that("map_to_cartesian_*() preserves additional non-coordinate columns", {
  df_extra <- make_polar_df() |>
    aniframe::as_aniframe() |>
    dplyr::mutate(label = letters[1], weight = c(10))

  out <- map_to_cartesian(df_extra)

  expect_equal(out$label, df_extra$label)
  expect_equal(out$weight, df_extra$weight)

  expect_equal(get_coordinate_system(out) |> as.character(), "cartesian_2d")
})

test_that("Cartesian results from the three systems are mutually consistent when the inputs represent the same point", {
  x <- 1
  y <- 1
  z <- 2
  rho_xy <- sqrt(x^2 + y^2)
  phi_xy <- atan2(y, x) %% (2 * pi)
  r_total <- sqrt(x^2 + y^2 + z^2)
  theta_sp <- acos(z / sqrt(x^2 + y^2 + z^2))

  df_pol <- dplyr::tibble(keypoint = 1, time = 1, rho = rho_xy, phi = phi_xy) |>
    aniframe::as_aniframe()
  df_cyl <- dplyr::tibble(
    keypoint = 1,
    time = 1,
    rho = rho_xy,
    phi = phi_xy,
    z = z
  ) |>
    aniframe::as_aniframe()
  df_sph <- dplyr::tibble(
    keypoint = 1,
    time = 1,
    rho = rho_xy,
    phi = phi_xy,
    theta = theta_sp
  ) |>
    aniframe::as_aniframe()

  cart_pol <- map_to_cartesian(df_pol)
  cart_cyl <- map_to_cartesian(df_cyl)
  cart_sph <- map_to_cartesian(df_sph)

  expect_equal(cart_pol$x, x, tolerance = 1e-8)
  expect_equal(cart_pol$y, y, tolerance = 1e-8)

  expect_equal(cart_cyl$x, x, tolerance = 1e-8)
  expect_equal(cart_cyl$y, y, tolerance = 1e-8)
  expect_equal(cart_cyl$z, z, tolerance = 1e-8)

  expect_equal(cart_sph$x, x, tolerance = 1e-8)
  expect_equal(cart_sph$y, y, tolerance = 1e-8)
  expect_equal(cart_sph$z, z, tolerance = 1e-8)
})
