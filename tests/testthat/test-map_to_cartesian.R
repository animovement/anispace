# -------------------------------------------------------------
# tests/testthat/test-map_to_cartesian.R
# -------------------------------------------------------------

# -----------------------------------------------------------------
# Helper to read the coordinate‑system metadata that `set_metadata()`
# writes. Adjust if your package uses a different accessor.
# -----------------------------------------------------------------
get_coordinate_system <- function(df) {
  get_metadata(df, "coordinate_system")
}

# -----------------------------------------------------------------
# 1️⃣  Minimal synthetic data frames for each coordinate system
# -----------------------------------------------------------------
make_polar_df <- function() {
  dplyr::tibble(
    keypoint = 1:3,
    time = seq(1:3),
    rho = c(1, sqrt(2), 0),
    phi = c(0, pi / 4, pi / 2)
  ) |>
    as_aniframe()
}

make_cylindrical_df <- function() {
  dplyr::tibble(
    keypoint = 1:3,
    time = seq(1:3),
    rho = c(1, sqrt(2), 0),
    phi = c(0, pi / 4, pi / 2),
    z = c(0, 5, -3)
  ) |>
    as_aniframe()
}

make_spherical_df <- function() {
  dplyr::tibble(
    keypoint = 1:3,
    time = seq(1:3),
    rho = c(1, sqrt(2), 0),
    phi = c(0, pi / 4, pi / 2),
    theta = c(pi / 3, pi / 2, 0) # various polar angles
  ) |>
    as_aniframe()
}

# -----------------------------------------------------------------
# 2️⃣  Tests for the *internal* helpers
# -----------------------------------------------------------------
test_that("map_to_cartesian_polar() drops polar columns and creates x/y", {
  df_in <- make_polar_df()
  df_out <- map_to_cartesian_polar(df_in) |> as_aniframe()

  # Expected Cartesian values (use the same primitives for consistency)
  expect_equal(df_out$x, polar_to_x(df_in$rho, df_in$phi))
  expect_equal(df_out$y, polar_to_y(df_in$rho, df_in$phi))

  # Original polar columns must be gone
  expect_false(any(c("rho", "phi") %in% colnames(df_out)))

  # Metadata should indicate cartesian
  expect_equal(get_coordinate_system(df_out) |> as.character(), "cartesian")
})

test_that("map_to_cartesian_cylindrical() retains original z", {
  df_in <- make_cylindrical_df()
  df_out <- map_to_cartesian_cylindrical(df_in) |> as_aniframe()

  expect_equal(df_out$x, polar_to_x(df_in$rho, df_in$phi))
  expect_equal(df_out$y, polar_to_y(df_in$rho, df_in$phi))
  expect_equal(df_out$z, df_in$z) # z unchanged

  expect_false(any(c("rho", "phi") %in% colnames(df_out)))
  expect_equal(get_coordinate_system(df_out) |> as.character(), "cartesian")
})

test_that("map_to_cartesian_spherical() correctly computes z via spherical_to_z()", {
  df_in <- make_spherical_df()
  df_out <- map_to_cartesian_spherical(df_in) |> as_aniframe()

  expect_equal(df_out$x, polar_to_x(df_in$rho, df_in$phi))
  expect_equal(df_out$y, polar_to_y(df_in$rho, df_in$phi))
  expect_equal(df_out$z, spherical_to_z(df_in$rho, df_in$theta))

  expect_false(any(c("rho", "phi", "theta") %in% colnames(df_out)))
  expect_equal(get_coordinate_system(df_out) |> as.character(), "cartesian")
})

# -----------------------------------------------------------------
# 3️⃣  Tests for the public dispatcher `map_to_cartesian()`
# -----------------------------------------------------------------
test_that("map_to_cartesian() dispatches to the correct helper (polar)", {
  df_in <- make_polar_df()
  df_out <- map_to_cartesian(df_in)
  df_correct <- map_to_cartesian_polar(df_in) |> as_aniframe()

  # Should be identical to the direct call
  expect_identical(df_out, df_correct)
})

test_that("map_to_cartesian() dispatches to the correct helper (cylindrical)", {
  df_in <- make_cylindrical_df()
  df_out <- map_to_cartesian(df_in)
  df_correct <- map_to_cartesian_cylindrical(df_in) |> as_aniframe()

  expect_identical(df_out, df_correct)
})

test_that("map_to_cartesian() dispatches to the correct helper (spherical)", {
  df_in <- make_spherical_df()
  df_out <- map_to_cartesian(df_in)
  df_correct <- map_to_cartesian_spherical(df_in) |> as_aniframe()

  expect_identical(df_out, df_correct)
})

test_that("map_to_cartesian() aborts with a clear message for unknown systems", {
  # Create a data frame that lacks any of the recognised signatures
  bad_df <- dplyr::tibble(keypoint = 1, a = 1, b = 2)

  expect_error(
    map_to_cartesian(bad_df)
  )
})

# -----------------------------------------------------------------
# 4️⃣  Edge‑case handling
# -----------------------------------------------------------------
test_that("map_to_cartesian_*() works with empty data frames (zero rows)", {
  empty_polar <- dplyr::tibble(
    keypoint = integer(),
    time = numeric(),
    rho = numeric(),
    phi = numeric()
  ) |>
    as_aniframe()
  empty_cyl <- dplyr::tibble(
    keypoint = integer(),
    time = numeric(),
    rho = numeric(),
    phi = numeric(),
    z = numeric()
  ) |>
    as_aniframe()
  empty_sph <- dplyr::tibble(
    keypoint = integer(),
    time = as.numeric(),
    rho = numeric(),
    phi = numeric(),
    theta = numeric()
  ) |>
    as_aniframe()

  expect_equal(nrow(map_to_cartesian_polar(empty_polar)), 0L)
  expect_equal(nrow(map_to_cartesian_cylindrical(empty_cyl)), 0L)
  expect_equal(nrow(map_to_cartesian_spherical(empty_sph)), 0L)

  # Dispatcher should also return an empty cartesian aniframe
  expect_equal(nrow(map_to_cartesian(empty_polar)), 0L)
  expect_equal(nrow(map_to_cartesian(empty_cyl)), 0L)
  expect_equal(nrow(map_to_cartesian(empty_sph)), 0L)
})

test_that("map_to_cartesian_*() preserves additional non‑coordinate columns", {
  df_extra <- make_polar_df() |>
    as_aniframe() |>
    dplyr::mutate(label = letters[1], weight = c(10))

  out <- map_to_cartesian(df_extra)

  # The extra columns should survive untouched
  expect_equal(out$label, df_extra$label)
  expect_equal(out$weight, df_extra$weight)

  # Still get the cartesian metadata
  expect_equal(get_coordinate_system(out) |> as.character(), "cartesian")
})

# -----------------------------------------------------------------
# 5️⃣  Consistency check across the three systems
# -----------------------------------------------------------------
test_that("Cartesian results from the three systems are mutually consistent when the inputs represent the same point", {
  # Build three representations of the same physical point (x = 1, y = 1, z = 2)
  x <- 1
  y <- 1
  z <- 2
  rho_xy <- sqrt(x^2 + y^2) # cylindrical/polar radius
  phi_xy <- atan2(y, x) %% (2 * pi) # azimuth
  r_total <- sqrt(x^2 + y^2 + z^2) # spherical radius (not needed here)
  theta_sp <- acos(z / sqrt(x^2 + y^2 + z^2)) # polar angle from +z

  # Polar (no z)
  df_pol <- dplyr::tibble(keypoint = 1, time = 1, rho = rho_xy, phi = phi_xy) |>
    as_aniframe()
  # Cylindrical (adds the known z)
  df_cyl <- dplyr::tibble(
    keypoint = 1,
    time = 1,
    rho = rho_xy,
    phi = phi_xy,
    z = z
  ) |>
    as_aniframe()
  # Spherical (uses theta)
  df_sph <- dplyr::tibble(
    keypoint = 1,
    time = 1,
    rho = rho_xy,
    phi = phi_xy,
    theta = theta_sp
  ) |>
    as_aniframe()

  cart_pol <- map_to_cartesian(df_pol)
  cart_cyl <- map_to_cartesian(df_cyl)
  cart_sph <- map_to_cartesian(df_sph)

  # All three should give the same (x, y, z) triple (within tolerance)
  expect_equal(cart_pol$x, x, tolerance = 1e-8)
  expect_equal(cart_pol$y, y, tolerance = 1e-8)

  expect_equal(cart_cyl$x, x, tolerance = 1e-8)
  expect_equal(cart_cyl$y, y, tolerance = 1e-8)
  expect_equal(cart_cyl$z, z, tolerance = 1e-8)

  expect_equal(cart_sph$x, x, tolerance = 1e-8)
  expect_equal(cart_sph$y, y, tolerance = 1e-8)
  expect_equal(cart_sph$z, z, tolerance = 1e-8)
})
