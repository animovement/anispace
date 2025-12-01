test_that("cartesian_to_rho() computes Euclidean distance correctly", {
  expect_equal(cartesian_to_rho(3, 4), 5)
  expect_equal(cartesian_to_rho(0, 0), 0)
  expect_equal(cartesian_to_rho(-3, -4), 5)
  expect_equal(cartesian_to_rho(1, 0), 1)
})

test_that("cartesian_to_phi() behaves consistently with atan2()", {
  skip_if_not(exists("wrap_angle"))

  # helper to compare with true atan2
  truth <- atan2(1, 1)
  expect_equal(
    cartesian_to_phi(1, 1),
    wrap_angle(truth),
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
      wrap_angle(atan2(q[2], q[1])),
      tolerance = 1e-8
    )
  }
})

test_that("cartesian_to_phi() centers correctly when centered = TRUE", {
  skip_if_not(exists("wrap_angle"))
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
  skip_if_not(exists("wrap_angle"))

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
  expected_constrained <- sapply(expected_angles, wrap_angle)

  expect_equal(results, expected_constrained, tolerance = 1e-8)
})

test_that("cartesian_to_phi() handles axes and quadrants correctly", {
  skip_if_not(exists("wrap_angle"))

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

# -----------------------------------------------------------------
# Helper: tiny tolerance for floating‑point comparisons
# -----------------------------------------------------------------
tol <- 1e-8

# -----------------------------------------------------------------
# 1️⃣  Regular case – sin(theta) != 0  →  z = rho / tan(theta)
# -----------------------------------------------------------------
test_that("spherical_to_z() returns correct z for generic angles", {
  # Choose a set of (rho, theta) pairs where tan(theta) is well‑behaved
  rho_vals <- c(1, 2, 5, 10)
  theta_vals <- c(pi / 6, pi / 4, pi / 3, pi / 2) # 30°,45°,60°,90°

  # Expected values from the analytic formula
  exp_z <- rho_vals / tan(theta_vals)

  got_z <- spherical_to_z(rho_vals, theta_vals)

  expect_equal(got_z, exp_z, tolerance = tol)
})

# -----------------------------------------------------------------
# 2️⃣  Pole handling – theta ≈ 0  (positive z‑axis)
# -----------------------------------------------------------------
test_that("spherical_to_z() treats the +z axis correctly", {
  # On the +z axis rho must be (practically) zero; we test a few tiny values
  rho_vals <- c(0, 0, 0)
  theta_vals <- c(0, 1e-12, 5e-13) # very close to 0

  # By definition the point lies on the +z axis, so z = 0 (or could be NA)
  # Our implementation returns 0 for these cases
  expect_equal(spherical_to_z(rho_vals, theta_vals), rep(0, 3))
})

# -----------------------------------------------------------------
# 3️⃣  Pole handling – theta ≈ π  (negative z‑axis)
# -----------------------------------------------------------------
test_that("spherical_to_z() treats the -z axis correctly", {
  rho_vals <- c(0, 0, 0)
  theta_vals <- c(pi, pi - 1e-12, pi - 5e-13) # just below π

  # Should also return 0 (the radius in the xy‑plane is zero)
  expect_equal(spherical_to_z(rho_vals, theta_vals), rep(0, 3))
})

# -----------------------------------------------------------------
# 4️⃣  Mixed vector input – ensure element‑wise operation
# -----------------------------------------------------------------
test_that("spherical_to_z() works element‑wise on mixed vectors", {
  rho_vals <- c(3, 0, 4, 0)
  theta_vals <- c(pi / 4, 0, pi, pi / 2) # mix of regular and pole angles

  # Manually compute expected results
  exp_z <- numeric(4)
  # 1) regular case
  exp_z[1] <- rho_vals[1] / tan(theta_vals[1])
  # 2) +z axis (rho = 0, theta ≈ 0) → 0
  exp_z[2] <- 0
  # 3) -z axis (rho = 0, theta ≈ π) → 0 (negative side, but still 0)
  exp_z[3] <- 0
  # 4) regular case with theta = π/2 → tan = Inf → 0 (since rho / Inf = 0)
  exp_z[4] <- rho_vals[4] / tan(theta_vals[4])

  expect_equal(spherical_to_z(rho_vals, theta_vals), exp_z, tolerance = tol)
})

# -----------------------------------------------------------------
# 5️⃣  Non‑finite inputs – propagate NA / NaN appropriately
# -----------------------------------------------------------------
test_that("spherical_to_z() propagates NA / NaN values", {
  rho_vals <- c(1, NA, 2, NaN)
  theta_vals <- c(pi / 3, pi / 4, NA, pi / 6)

  got_z <- spherical_to_z(rho_vals, theta_vals)

  expect_true(is.na(got_z[2])) # NA in rho → NA result
  expect_true(is.na(got_z[3])) # NA in theta → NA result
  expect_true(is.na(got_z[4])) # NaN propagates
  # First element should be a valid numeric value
  expect_false(is.na(got_z[1]))
  expect_false(is.na(got_z[1]))
})

# -----------------------------------------------------------------
# 6️⃣  Negative rho (physically meaningless but mathematically allowed)
# -----------------------------------------------------------------
test_that("spherical_to_z() handles negative rho gracefully", {
  rho_vals <- c(-3, -5)
  theta_vals <- c(pi / 4, pi / 3)

  # Formula still applies: z = rho / tan(theta)
  exp_z <- rho_vals / tan(theta_vals)

  expect_equal(spherical_to_z(rho_vals, theta_vals), exp_z, tolerance = tol)
})
