# tests/testthat/test-angle-helpers.R

test_that("wrap_angle() wraps angles to [0, 2pi)", {
  expect_equal(wrap_angle(0), 0)
  expect_equal(wrap_angle(2 * pi), 0)
  expect_equal(wrap_angle(-pi / 2), 3 * pi / 2)
  expect_equal(wrap_angle(3 * pi), pi)
  expect_equal(wrap_angle(4 * pi), 0)
  expect_equal(wrap_angle(5 * pi / 2), pi / 2)
})

test_that("wrap_angle() is vectorised", {
  input <- c(-pi / 2, 0, pi / 2, pi, 3 * pi / 2, 2 * pi)
  expected <- c(3 * pi / 2, 0, pi / 2, pi, 3 * pi / 2, 0)
  expect_equal(wrap_angle(input), expected)
})

test_that("calculate_angular_difference() returns expected signed differences", {
  # Simple same-angle case
  expect_equal(calculate_angular_difference(0, 0), 0)

  # Small positive/negative differences
  expect_equal(calculate_angular_difference(pi / 4, 0), -pi / 4)
  expect_equal(calculate_angular_difference(0, pi / 4), pi / 4)

  # Wrap-around at 2*pi boundary
  expect_equal(
    calculate_angular_difference(0, 2 * pi - 0.1),
    -0.1,
    tolerance = 1e-10
  )
  expect_equal(
    calculate_angular_difference(2 * pi - 0.1, 0),
    0.1,
    tolerance = 1e-10
  )

  # Large differences should wrap correctly
  expect_equal(
    calculate_angular_difference(0, 3 * pi / 2),
    -pi / 2,
    tolerance = 1e-10
  )
  expect_equal(
    calculate_angular_difference(3 * pi / 2, 0),
    pi / 2,
    tolerance = 1e-10
  )

  # Difference exactly equal to pi stays as pi, not wrapped
  expect_equal(calculate_angular_difference(pi, 0), pi)
  expect_equal(calculate_angular_difference(0, pi), pi)
})

test_that("calculate_angular_difference() is vectorised", {
  from <- c(0, pi / 2, pi)
  to <- c(pi / 2, pi, 0)
  result <- calculate_angular_difference(from, to)
  expected <- c(pi / 2, pi / 2, pi)
  expect_equal(result, expected)
})


# ------------------------------------------------------------------
# Helper to expose internal objects (adjust the package name as needed)
# ------------------------------------------------------------------
calc_ang_diff <- calculate_angular_difference

# ------------------------------------------------------------------
# Simple wrapper for the angular difference used in expectations
# ------------------------------------------------------------------
expected_diff <- function(x, lag = 1L) {
  # Mimic the same logic as diff_angle but without the NA padding
  if (length(x) <= lag) {
    return(numeric(0))
  }
  from <- x[seq_len(length(x) - lag)]
  to <- x[(lag + 1):length(x)]
  mapply(calc_ang_diff, from_angle = from, to_angle = to)
}

# ------------------------------------------------------------------
# 1. Input validation ------------------------------------------------
# ------------------------------------------------------------------
test_that("`x` must be numeric", {
  expect_error(
    diff_angle("not numeric", lag = 1L),
    "`x` must be a numeric vector of angles"
  )
})

test_that("`lag` must be a positive integer", {
  expect_error(diff_angle(1:5, lag = 0L), "`lag` must be a positive integer")
  expect_error(diff_angle(1:5, lag = -2L), "`lag` must be a positive integer")
  # Non‑integer numeric should also trigger the same check
  expect_error(diff_angle(1:5, lag = 1.5), "`lag` must be a positive integer")
})

# ------------------------------------------------------------------
# 2. Length ≤ lag returns empty numeric vector ----------------------
# ------------------------------------------------------------------
test_that("returns numeric(0) when length(x) <= lag", {
  expect_identical(diff_angle(numeric(0), lag = 1L), numeric(0))
  expect_identical(diff_angle(c(0.1), lag = 1L), numeric(0))
  expect_identical(diff_angle(c(0.2, 0.3), lag = 2L), numeric(0))
})

# ------------------------------------------------------------------
# 3. Correct angular differences + NA padding -----------------------
# ------------------------------------------------------------------
test_that("computes angular differences and pads with NA", {
  set.seed(123)
  angles <- runif(10, 0, 2 * pi) # random radian angles

  # lag = 1 (default)
  res1 <- diff_angle(angles, lag = 1L)
  expect_length(res1, length(angles))
  expect_true(all(is.na(res1[1:1]))) # first lag positions are NA
  expect_equal(res1[-(1:1)], expected_diff(angles, lag = 1L))

  # lag = 3
  lag_val <- 3L
  res3 <- diff_angle(angles, lag = lag_val)
  expect_length(res3, length(angles))
  expect_true(all(is.na(res3[1:lag_val])))
  expect_equal(res3[-(1:lag_val)], expected_diff(angles, lag = lag_val))
})

# ------------------------------------------------------------------
# 4. Handles vectors containing NA values ---------------------------
# ------------------------------------------------------------------
test_that("propagates NA values correctly", {
  vec <- c(0, pi / 2, NA, pi, 3 * pi / 2)

  # lag = 1
  out <- diff_angle(vec, lag = 1L)
  # First element is NA (padding)
  expect_true(is.na(out[1]))
  # Subsequent elements where either side is NA should be NA
  expect_true(is.na(out[3])) # diff between pi/2 and NA
  expect_true(is.na(out[4])) # diff between NA and pi
  # Non‑NA diffs should match the helper
  expect_equal(out[2], calc_ang_diff(vec[1], vec[2]))
  expect_equal(out[5], calc_ang_diff(vec[4], vec[5]))
})

# ------------------------------------------------------------------
# 5. Edge case: single‑element vector --------------------------------
# ------------------------------------------------------------------
test_that("single‑element vector returns numeric(0)", {
  expect_identical(diff_angle(c(pi / 4), lag = 1L), numeric(0))
})

# ------------------------------------------------------------------
# 6. Large lag relative to vector length -----------------------------
# ------------------------------------------------------------------
test_that("large lag greater than length returns numeric(0)", {
  v <- runif(5, 0, 2 * pi)
  expect_identical(diff_angle(v, lag = 10L), numeric(0))
})

# ------------------------------------------------------------------
# 7. Consistency with base::diff for linear (non‑wrapped) values ---
# ------------------------------------------------------------------
test_that("behaves like base::diff when angles are monotonic and within [-π, π]", {
  lin_angles <- seq(-pi / 2, pi / 2, length.out = 7) # monotonic, no wrap needed
  expect_equal(
    diff_angle(lin_angles, lag = 1L)[-1],
    base::diff(lin_angles, lag = 1L)
  )
})
