test_that("cartesian_to_rho() computes Euclidean distance correctly", {
  expect_equal(cartesian_to_rho(3, 4), 5)
  expect_equal(cartesian_to_rho(0, 0), 0)
  expect_equal(cartesian_to_rho(-3, -4), 5)
  expect_equal(cartesian_to_rho(1, 0), 1)
})

test_that("cartesian_to_phi() is atan2() in every quadrant", {
  # Signed, (-pi, pi], the range the suite uses for every direction
  # (animovement/anicore#181)
  x <- c(1, -1, -1, 1, 3, -0.2, -5, 0.1)
  y <- c(1, 1, -1, -1, 0.5, 4, -0.3, -2)

  phi <- cartesian_to_phi(x, y)

  expect_identical(phi, atan2(y, x))
  expect_equal(phi[1:4], c(1, 3, -3, -1) * pi / 4)
})

test_that("cartesian_to_phi() puts the axes at 0, pi / 2, pi and -pi / 2", {
  expect_identical(
    cartesian_to_phi(c(1, 0, -1, 0), c(0, 1, 0, -1)),
    c(0, pi / 2, pi, -pi / 2)
  )
})

test_that("cartesian_to_phi() gives pi, not -pi, on the negative x-axis", {
  # atan2() gives -pi when y is a negative zero; it is the same direction
  expect_identical(atan2(-0, -1), -pi)
  expect_identical(cartesian_to_phi(-1, 0), pi)
  expect_identical(cartesian_to_phi(-1, -0), pi)
  expect_identical(cartesian_to_phi(c(-2, -0.5), c(-0, 0)), c(pi, pi))
})

test_that("cartesian_to_phi() stays in (-pi, pi]", {
  grid <- expand.grid(x = seq(-3, 3, by = 0.25), y = seq(-3, 3, by = 0.25))
  x <- c(grid$x, -1, 0)
  y <- c(grid$y, -0, -0)

  phi <- cartesian_to_phi(x, y)

  expect_true(all(phi > -pi & phi <= pi))
})

test_that("cartesian_to_phi() keeps missing values", {
  expect_identical(
    cartesian_to_phi(c(-1, NA, 1, NaN), c(-0, 1, NA, 1)),
    c(pi, NA, NA, NaN)
  )
})

test_that("cartesian_to_phi(centered) is deprecated", {
  x <- c(1, -1, -1, 1, -1)
  y <- c(1, 1, -1, -1, -0)

  expect_warning(
    signed <- cartesian_to_phi(x, y, centered = TRUE),
    class = "lifecycle_warning_deprecated"
  )
  expect_identical(signed, cartesian_to_phi(x, y))

  # FALSE keeps the [0, 2pi) it used to give, until the argument goes
  expect_warning(
    unsigned <- cartesian_to_phi(x, y, centered = FALSE),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(unsigned, c(1, 3, 5, 7, 4) * pi / 4)
  expect_equal(unsigned, anicore::wrap_angle(cartesian_to_phi(x, y), "2pi"))
})

test_that("polar_to_x() and polar_to_y() correctly invert Cartesian coordinates", {
  rho <- sqrt(2)
  phi <- pi / 4
  expect_equal(polar_to_x(rho, phi), 1, tolerance = 1e-8)
  expect_equal(polar_to_y(rho, phi), 1, tolerance = 1e-8)
})

test_that("polar_to_x() and polar_to_y() accept phi in any range", {
  rho <- c(1, 2, 3, 4)
  signed <- c(3 * pi / 4, -3 * pi / 4, -pi / 4, pi)
  unsigned <- c(3 * pi / 4, 5 * pi / 4, 7 * pi / 4, pi)
  unwrapped <- signed + c(2, -2, 4, -4) * pi
  x <- c(-1, -2, 3, -4 * sqrt(2)) / sqrt(2)
  y <- c(1, -2, -3, 0) / sqrt(2)

  for (phi in list(signed, unsigned, unwrapped)) {
    expect_equal(polar_to_x(rho, phi), x, tolerance = 1e-12)
    expect_equal(polar_to_y(rho, phi), y, tolerance = 1e-12)
  }
})

test_that("polar_to_x() and polar_to_y() handle zero radius correctly", {
  expect_equal(polar_to_x(0, 1), 0)
  expect_equal(polar_to_y(0, 2), 0)
})

# -------------------------------------------------------------
# Tests for spherical_to_z()
# -------------------------------------------------------------
# `rho` is the radial distance from the origin, so z = rho * cos(theta).
# These previously encoded z = rho / tan(theta), which is the cylindrical
# formulation and only correct when rho is the xy-plane radius (#19).

tol <- 1e-8

test_that("spherical_to_z() returns correct z for generic angles", {
  rho_vals <- c(1, 2, 5, 10)
  theta_vals <- c(pi / 6, pi / 4, pi / 3, pi / 2)

  exp_z <- rho_vals * cos(theta_vals)

  expect_equal(spherical_to_z(rho_vals, theta_vals), exp_z, tolerance = tol)
})

test_that("spherical_to_z() recovers full height on the +z axis", {
  # The case the cylindrical formulation could not express: on the axis the
  # xy-plane radius is 0, so the height was unrecoverable and returned as 0.
  # With the radial distance it is simply rho.
  expect_equal(spherical_to_z(c(1, 5, 10), c(0, 0, 0)), c(1, 5, 10))
})

test_that("spherical_to_z() recovers full height on the -z axis", {
  expect_equal(spherical_to_z(c(1, 5, 10), rep(pi, 3)), c(-1, -5, -10))
})

test_that("spherical_to_z() is zero in the xy-plane", {
  expect_equal(
    spherical_to_z(c(3, 7), c(pi / 2, pi / 2)),
    c(0, 0),
    tolerance = tol
  )
})

test_that("spherical_to_z() works element-wise on mixed vectors", {
  rho_vals <- c(3, 1, 4, 2)
  theta_vals <- c(pi / 4, 0, pi, pi / 2)

  exp_z <- rho_vals * cos(theta_vals)

  expect_equal(spherical_to_z(rho_vals, theta_vals), exp_z, tolerance = tol)
})

test_that("spherical_to_z() propagates NA / NaN values", {
  rho_vals <- c(1, NA, 2, NaN)
  theta_vals <- c(pi / 3, pi / 4, NA, pi / 6)

  got_z <- spherical_to_z(rho_vals, theta_vals)

  expect_true(is.na(got_z[2]))
  expect_true(is.na(got_z[3]))
  expect_true(is.na(got_z[4]))
  expect_false(is.na(got_z[1]))
})

test_that("spherical_to_z() handles negative rho gracefully", {
  rho_vals <- c(-3, -5)
  theta_vals <- c(pi / 4, pi / 3)

  expect_equal(
    spherical_to_z(rho_vals, theta_vals),
    rho_vals * cos(theta_vals),
    tolerance = tol
  )
})
