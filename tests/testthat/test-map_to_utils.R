test_that("cartesian_to_rho() computes Euclidean distance correctly", {
  expect_equal(cartesian_to_rho(3, 4), 5)
  expect_equal(cartesian_to_rho(0, 0), 0)
  expect_equal(cartesian_to_rho(-3, -4), 5)
  expect_equal(cartesian_to_rho(1, 0), 1)
})

test_that("cartesian_to_phi() behaves consistently with atan2()", {
  # helper to compare with true atan2
  truth <- atan2(1, 1)
  expect_equal(
    cartesian_to_phi(1, 1),
    anicore::wrap_angle(truth),
    tolerance = 1e-8
  )

  # Test all quadrants
  xy <- list(
    Q1 = c(1, 1),
    Q2 = c(-1, 1),
    Q3 = c(-1, -1),
    Q4 = c(1, -1)
  )
  for (q in xy) {
    expect_equal(
      cartesian_to_phi(q[1], q[2]),
      anicore::wrap_angle(atan2(q[2], q[1])),
      tolerance = 1e-8
    )
  }
})

test_that("cartesian_to_phi() centers correctly when centered = TRUE", {
  phi <- cartesian_to_phi(1, -1, centered = TRUE)
  expect_true(phi >= -pi && phi <= pi)
})

test_that("polar_to_x() and polar_to_y() correctly invert Cartesian coordinates", {
  rho <- sqrt(2)
  phi <- pi / 4
  expect_equal(polar_to_x(rho, phi), 1, tolerance = 1e-8)
  expect_equal(polar_to_y(rho, phi), 1, tolerance = 1e-8)
})

test_that("polar_to_x() and polar_to_y() handle zero radius correctly", {
  expect_equal(polar_to_x(0, 1), 0)
  expect_equal(polar_to_y(0, 2), 0)
})

# -------------------------------------------------------------------------
# 🚨 Extra diagnostic tests (for updated cartesian_to_phi)
# -------------------------------------------------------------------------

test_that("cartesian_to_phi() should match atan2() for key reference points", {
  ## True values using atan2(y, x)
  expected_angles <- c(
    atan2(1, 0), # (x = 0, y = 1) → π/2
    atan2(0, 1), # (x = 1, y = 0) → 0
    atan2(-1, 0), # (x = 0, y = -1) → -π/2
    atan2(0, -1) # (x = -1, y = 0) → π (or -π)
  )

  ## Test points as a list of (x, y) pairs
  test_points <- list(
    c(0, 1), # straight up
    c(1, 0), # right
    c(0, -1), # down
    c(-1, 0) # left
  )

  ## Compute the angles from cartesian_to_phi for each point
  ## sapply returns a numeric vector (same as map_dbl)
  results <- sapply(test_points, function(pt) cartesian_to_phi(pt[1], pt[2]))

  ## Constrain the reference angles and compare
  expected_constrained <- sapply(expected_angles, anicore::wrap_angle)

  expect_equal(results, expected_constrained, tolerance = 1e-8)
})

test_that("cartesian_to_phi() handles axes and quadrants correctly", {
  # The current implementation will incorrectly swap x/y
  # This test will fail until cartesian_to_phi() uses atan2(y, x)

  # Expect roughly 0 radians at (x>0, y=0)
  expect_true(abs(cartesian_to_phi(1, 0) - 0) < 1e-8)

  # Expect roughly pi/2 radians at (x=0, y>0)
  expect_true(abs(cartesian_to_phi(0, 1) - pi / 2) < 1e-8)

  # Expect roughly pi radians at (x<0, y=0)
  expect_true(abs(abs(cartesian_to_phi(-1, 0)) - pi) < 1e-8)

  # Expect roughly -pi/2 radians at (x=0, y<0)
  expect_true(
    calculate_angular_difference(
      abs(cartesian_to_phi(0, -1) + pi / 2),
      0
    ) <
      1e-8
  )
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
