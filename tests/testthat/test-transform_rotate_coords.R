# test-transform_rotate_coords.R

# ==== Setup helpers ==========================================================

make_point <- function(t, ind, kp, x, y, z = NA) {
  dplyr::tibble(time = t, individual = ind, keypoint = kp, x = x, y = y, z = z)
}

make_pair <- function(name1, x1, y1, z1, name2, x2, y2, z2, n = 3, ind = "A") {
  t <- seq_len(n)
  dplyr::bind_rows(
    make_point(t, ind, name1, x1, y1, z1),
    make_point(t, ind, name2, x2, y2, z2)
  ) |>
    as_aniframe()
}

angle_2d <- function(p1, p2) atan2(p2$y - p1$y, p2$x - p1$x)

get_angle <- function(data, kp1, kp2, t = 1, ind = "A") {
  p1 <- dplyr::filter(data, individual == ind, keypoint == kp1, time == t)
  p2 <- dplyr::filter(data, individual == ind, keypoint == kp2, time == t)
  angle_2d(p1, p2)
}

# Test data setup ----
create_rotation_test_data <- function() {
  # Simple test case: points aligned along x-axis
  dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "head",
    x = c(0, 0, 0),
    y = c(0, 0, 0)
  ) |>
    as_aniframe()
}

create_horizontal_alignment_data <- function() {
  # Two keypoints aligned horizontally (along x-axis)
  data_head <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "head",
    x = c(0, 0, 0),
    y = c(0, 0, 0)
  )

  data_tail <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "tail",
    x = c(2, 2, 2),
    y = c(0, 0, 0)
  )

  dplyr::bind_rows(data_head, data_tail) |>
    as_aniframe()
}

create_vertical_alignment_data <- function() {
  # Two keypoints aligned vertically (along y-axis)
  data_head <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "head",
    x = c(0, 0, 0),
    y = c(0, 0, 0)
  )

  data_tail <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "tail",
    x = c(0, 0, 0),
    y = c(2, 2, 2)
  )

  dplyr::bind_rows(data_head, data_tail) |>
    as_aniframe()
}

create_diagonal_alignment_data <- function() {
  # Two keypoints at 45-degree angle
  data_head <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "head",
    x = c(0, 0, 0),
    y = c(0, 0, 0)
  )

  data_tail <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "tail",
    x = c(1, 1, 1),
    y = c(1, 1, 1)
  )

  dplyr::bind_rows(data_head, data_tail) |>
    as_aniframe()
}

create_three_keypoint_data <- function() {
  # Three keypoints for testing that all points rotate together
  data_head <- dplyr::tibble(
    time = 1:2,
    individual = "A",
    keypoint = "head",
    x = c(0, 0),
    y = c(0, 0)
  )

  data_body <- dplyr::tibble(
    time = 1:2,
    individual = "A",
    keypoint = "body",
    x = c(1, 1),
    y = c(0.5, 0.5)
  )

  data_tail <- dplyr::tibble(
    time = 1:2,
    individual = "A",
    keypoint = "tail",
    x = c(2, 2),
    y = c(0, 0)
  )

  dplyr::bind_rows(data_head, data_body, data_tail) |>
    as_aniframe()
}

create_multi_individual_data <- function() {
  # Individual A aligned horizontally
  data_a_head <- dplyr::tibble(
    time = 1:2,
    individual = "A",
    keypoint = "head",
    x = c(0, 0),
    y = c(0, 0)
  )

  data_a_tail <- dplyr::tibble(
    time = 1:2,
    individual = "A",
    keypoint = "tail",
    x = c(2, 2),
    y = c(0, 0)
  )

  # Individual B aligned vertically
  data_b_head <- dplyr::tibble(
    time = 1:2,
    individual = "B",
    keypoint = "head",
    x = c(0, 0),
    y = c(0, 0)
  )

  data_b_tail <- dplyr::tibble(
    time = 1:2,
    individual = "B",
    keypoint = "tail",
    x = c(0, 0),
    y = c(2, 2)
  )

  dplyr::bind_rows(data_a_head, data_a_tail, data_b_head, data_b_tail) |>
    as_aniframe()
}

create_varying_angle_data <- function() {
  # Alignment points at different angles across time
  data_head <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "head",
    x = c(0, 0, 0),
    y = c(0, 0, 0)
  )

  data_tail <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "tail",
    x = c(1, 0, 1), # Different positions across time
    y = c(0, 1, 1) # Creates different angles
  )

  data_body <- dplyr::tibble(
    time = 1:3,
    individual = "A",
    keypoint = "body",
    x = c(0.5, 0.5, 0.5),
    y = c(0.5, 0.5, 0.5)
  )

  dplyr::bind_rows(data_head, data_tail, data_body) |>
    as_aniframe()
}

# Helper functions ----
calculate_angle <- function(x1, y1, x2, y2) {
  atan2(y2 - y1, x2 - x1)
}

get_vector_angle <- function(data, kp1, kp2, ind = "A", t = 1) {
  p1 <- data |>
    dplyr::filter(individual == ind, keypoint == kp1, time == t)
  p2 <- data |>
    dplyr::filter(individual == ind, keypoint == kp2, time == t)
  calculate_angle(p1$x, p1$y, p2$x, p2$y)
}

# ==== 2D tests (existing) ====================================================

# Tests for basic rotation functionality ----
test_that("rotate_coords preserves horizontal alignment at 0 degrees", {
  data <- create_horizontal_alignment_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # Points already aligned horizontally, should remain unchanged
  expect_equal(result$x, data$x, tolerance = 1e-10)
  expect_equal(result$y, data$y, tolerance = 1e-10)
})

test_that("rotate_coords rotates vertical line to horizontal", {
  data <- create_vertical_alignment_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # After rotation, alignment points should be on x-axis (y should be ~0)
  head_coords <- result |> dplyr::filter(keypoint == "head")
  tail_coords <- result |> dplyr::filter(keypoint == "tail")

  expect_equal(head_coords$y, rep(0, 3), tolerance = 1e-10)
  expect_equal(tail_coords$y, rep(0, 3), tolerance = 1e-10)

  # x-coordinates should differ (aligned horizontally)
  expect_true(all(tail_coords$x > head_coords$x))
})

test_that("rotate_coords rotates diagonal line to horizontal", {
  data <- create_diagonal_alignment_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # After rotation, alignment points should be on x-axis
  head_coords <- result |> dplyr::filter(keypoint == "head")
  tail_coords <- result |> dplyr::filter(keypoint == "tail")

  expect_equal(head_coords$y, rep(0, 3), tolerance = 1e-10)
  expect_equal(tail_coords$y, rep(0, 3), tolerance = 1e-10)
})

test_that("rotate_coords with align_perpendicular makes horizontal line vertical", {
  data <- create_horizontal_alignment_data()

  result <- rotate_coords(
    data,
    alignment_points = c("head", "tail"),
    align_perpendicular = TRUE
  )

  # After rotation, alignment points should be on y-axis (x should be ~0)
  head_coords <- result |> dplyr::filter(keypoint == "head")
  tail_coords <- result |> dplyr::filter(keypoint == "tail")

  expect_equal(head_coords$x, rep(0, 3), tolerance = 1e-10)
  expect_equal(tail_coords$x, rep(0, 3), tolerance = 1e-10)

  # y-coordinates should differ (aligned vertically)
  expect_true(all(tail_coords$y != head_coords$y))
})

test_that("rotate_coords with align_perpendicular makes vertical line horizontal", {
  data <- create_vertical_alignment_data()

  result <- rotate_coords(
    data,
    alignment_points = c("head", "tail"),
    align_perpendicular = TRUE
  )

  # After rotation, alignment points should remain on y-axis
  head_coords <- result |> dplyr::filter(keypoint == "head")
  tail_coords <- result |> dplyr::filter(keypoint == "tail")

  # Should be perpendicular, so x should be ~0
  expect_equal(head_coords$x, rep(0, 3), tolerance = 1e-10)
  expect_equal(tail_coords$x, rep(0, 3), tolerance = 1e-10)
})

# Tests for distance preservation ----
test_that("rotate_coords preserves distances between points", {
  data <- create_three_keypoint_data()

  # Calculate original distances
  orig_dist_head_tail <- sqrt(
    (data$x[data$keypoint == "head"][1] -
      data$x[data$keypoint == "tail"][1])^2 +
      (data$y[data$keypoint == "head"][1] -
        data$y[data$keypoint == "tail"][1])^2
  )

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # Calculate rotated distances
  rot_dist_head_tail <- sqrt(
    (result$x[result$keypoint == "head"][1] -
      result$x[result$keypoint == "tail"][1])^2 +
      (result$y[result$keypoint == "head"][1] -
        result$y[result$keypoint == "tail"][1])^2
  )

  expect_equal(orig_dist_head_tail, rot_dist_head_tail, tolerance = 1e-10)
})

test_that("rotate_coords rotates all keypoints together", {
  data <- create_three_keypoint_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # All points should be rotated, not just alignment points
  expect_equal(nrow(result), nrow(data))
  expect_true("body" %in% result$keypoint)
})

# Tests for multiple individuals ----
test_that("rotate_coords processes each individual independently", {
  data <- create_multi_individual_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # Check that both individuals are in result
  expect_true(all(c("A", "B") %in% unique(result$individual)))

  # Individual A should remain horizontal (already aligned)
  ind_a <- result |> dplyr::filter(individual == "A")
  expect_equal(
    ind_a$y[ind_a$keypoint == "head"],
    ind_a$y[ind_a$keypoint == "tail"],
    tolerance = 1e-10
  )

  # Individual B should be rotated to horizontal
  ind_b <- result |> dplyr::filter(individual == "B")
  expect_equal(
    ind_b$y[ind_b$keypoint == "head"],
    ind_b$y[ind_b$keypoint == "tail"],
    tolerance = 1e-10
  )
})

test_that("rotate_coords maintains individual separation", {
  data <- create_multi_individual_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # Check row counts per individual
  expect_equal(
    nrow(result |> dplyr::filter(individual == "A")),
    nrow(data |> dplyr::filter(individual == "A"))
  )
  expect_equal(
    nrow(result |> dplyr::filter(individual == "B")),
    nrow(data |> dplyr::filter(individual == "B"))
  )
})

# Tests for time-varying rotations ----
test_that("rotate_coords handles different angles at different times", {
  data <- create_varying_angle_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # At each time point, alignment should be horizontal
  for (t in 1:3) {
    time_data <- result |> dplyr::filter(time == t)
    head_y <- time_data$y[time_data$keypoint == "head"]
    tail_y <- time_data$y[time_data$keypoint == "tail"]
    expect_equal(head_y, tail_y, tolerance = 1e-10)
  }
})

test_that("rotate_coords applies different rotations per time point", {
  data <- create_varying_angle_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # Body position should differ across time (different rotations applied)
  body_coords <- result |> dplyr::filter(keypoint == "body")

  # Not all body coordinates should be identical (different rotations)
  expect_false(all(body_coords$x == body_coords$x[1]))
})

# Tests for data structure preservation ----
test_that("rotate_coords preserves data structure", {
  data <- create_horizontal_alignment_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  expect_equal(nrow(result), nrow(data))
  expect_equal(names(result), names(data))
  expect_equal(result$time, data$time)
  expect_equal(result$individual, data$individual)
  expect_equal(result$keypoint, data$keypoint)
})

test_that("rotate_coords returns aniframe object", {
  data <- create_horizontal_alignment_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  expect_true("aniframe" %in% class(result))
})

test_that("rotate_coords preserves all keypoints", {
  data <- create_three_keypoint_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  expect_setequal(unique(result$keypoint), unique(data$keypoint))
})

# Tests for error handling ----
test_that("rotate_coords errors with wrong number of alignment points", {
  data <- create_horizontal_alignment_data()

  expect_error(
    rotate_coords(data, alignment_points = c("head")),
    "exactly 2 keypoint names"
  )

  expect_error(
    rotate_coords(data, alignment_points = c("head", "body", "tail")),
    "exactly 2 keypoint names"
  )
})

test_that("rotate_coords errors with non-existent keypoints", {
  data <- create_horizontal_alignment_data()

  expect_error(
    rotate_coords(data, alignment_points = c("head", "nonexistent")),
    "not found in data"
  )
})

test_that("rotate_coords errors with missing keypoints", {
  data <- create_horizontal_alignment_data()

  expect_error(
    rotate_coords(data, alignment_points = c("nose", "ear")),
    "not found in data"
  )
})

# Tests for edge cases ----
test_that("rotate_coords handles single time point", {
  data <- create_horizontal_alignment_data() |>
    dplyr::filter(time == 1)

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  expect_equal(nrow(result), nrow(data))
})

test_that("rotate_coords handles points at origin", {
  data_head <- dplyr::tibble(
    time = 1,
    individual = "A",
    keypoint = "head",
    x = 0,
    y = 0
  )

  data_tail <- dplyr::tibble(
    time = 1,
    individual = "A",
    keypoint = "tail",
    x = 1,
    y = 0
  )

  data <- dplyr::bind_rows(data_head, data_tail) |> as_aniframe()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  expect_equal(nrow(result), nrow(data))
})

test_that("rotate_coords handles negative coordinates", {
  data_head <- dplyr::tibble(
    time = 1,
    individual = "A",
    keypoint = "head",
    x = -1,
    y = -1
  )

  data_tail <- dplyr::tibble(
    time = 1,
    individual = "A",
    keypoint = "tail",
    x = 1,
    y = 1
  )

  data <- dplyr::bind_rows(data_head, data_tail) |> as_aniframe()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  # Should align to x-axis
  head_coords <- result |> dplyr::filter(keypoint == "head")
  tail_coords <- result |> dplyr::filter(keypoint == "tail")

  expect_equal(head_coords$y, tail_coords$y, tolerance = 1e-10)
})

# Tests for rotation angle calculations ----
test_that("rotate_coords produces correct angle for 90-degree rotation", {
  data <- create_vertical_alignment_data()

  # Vertical line (90 degrees) should rotate to 0 degrees (horizontal)
  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  angle_after <- get_vector_angle(result, "head", "tail", "A", 1)
  expect_equal(angle_after, 0, tolerance = 1e-10)
})

test_that("rotate_coords produces correct angle for 45-degree rotation", {
  data <- create_diagonal_alignment_data()

  result <- rotate_coords(data, alignment_points = c("head", "tail"))

  angle_after <- get_vector_angle(result, "head", "tail", "A", 1)
  expect_equal(angle_after, 0, tolerance = 1e-10)
})

test_that("rotate_coords perpendicular creates 90-degree angle", {
  data <- create_horizontal_alignment_data()

  result <- rotate_coords(
    data,
    alignment_points = c("head", "tail"),
    align_perpendicular = TRUE
  )

  angle_after <- get_vector_angle(result, "head", "tail", "A", 1)
  expect_equal(angle_after, pi / 2, tolerance = 1e-10)
})

# ==== 3D test data ===========================================================

create_horizontal_3d <- function() {
  make_pair("head", 0, 0, 0, "tail", 2, 0, 0)
}

create_vertical_3d <- function() {
  make_pair("head", 0, 0, 0, "tail", 0, 2, 0)
}

create_depth_3d <- function() {
  make_pair("head", 0, 0, 0, "tail", 0, 0, 2)
}

create_diagonal_3d <- function() {
  make_pair("head", 0, 0, 0, "tail", 1, 1, 1)
}

create_three_keypoint_3d <- function() {
  t <- 1
  dplyr::bind_rows(
    make_point(t, "A", "head", 0, 0, 0),
    make_point(t, "A", "body", 1, 0.5, 0.5),
    make_point(t, "A", "tail", 2, 0, 1)
  ) |>
    as_aniframe()
}

# ==== 3D rotation tests ======================================================

# test_that("rotate_coords correctly aligns a diagonal 3D vector to x-axis", {
#   data <- create_diagonal_3d()
#   result <- rotate_coords(data, alignment_points = c("head", "tail"))
#
#   # The vector from head to tail should lie on x-axis → y,z ≈ 0
#   head <- dplyr::filter(result, keypoint == "head")
#   tail <- dplyr::filter(result, keypoint == "tail")
#   expect_equal(tail$y - head$y, 0, tolerance = 1e-10)
#   expect_equal(tail$z - head$z, 0, tolerance = 1e-10)
#   expect_true(tail$x > head$x)
# })
#
# test_that("rotate_coords aligns vertical 3D vector to x-axis", {
#   data <- create_vertical_3d()
#   result <- rotate_coords(data, alignment_points = c("head", "tail"))
#   head <- dplyr::filter(result, keypoint == "head")
#   tail <- dplyr::filter(result, keypoint == "tail")
#   expect_equal(tail$y - head$y, 0, tolerance = 1e-10)
#   expect_equal(tail$z - head$z, 0, tolerance = 1e-10)
# })
#
# test_that("rotate_coords aligns depth 3D vector (z-axis) to x-axis", {
#   data <- create_depth_3d()
#   result <- rotate_coords(data, alignment_points = c("head", "tail"))
#   head <- dplyr::filter(result, keypoint == "head")
#   tail <- dplyr::filter(result, keypoint == "tail")
#   expect_equal(tail$y - head$y, 0, tolerance = 1e-10)
#   expect_equal(tail$z - head$z, 0, tolerance = 1e-10)
# })
#
# test_that("rotate_coords_3d with align_perpendicular rotates to y-axis", {
#   data <- create_horizontal_3d()
#   result <- rotate_coords(data, alignment_points = c("head", "tail"), align_perpendicular = TRUE)
#   head <- dplyr::filter(result, keypoint == "head")
#   tail <- dplyr::filter(result, keypoint == "tail")
#   expect_equal(tail$x - head$x, 0, tolerance = 1e-10)
#   expect_true(abs(tail$y - head$y) > 0)
# })
#
# test_that("rotate_coords_3d preserves distances between keypoints", {
#   data <- create_three_keypoint_3d()
#   d_before <- sqrt(sum((dplyr::filter(data, keypoint == "head")[, c("x", "y", "z")] -
#                           dplyr::filter(data, keypoint == "tail")[, c("x", "y", "z")])^2))
#   result <- rotate_coords(data, alignment_points = c("head", "tail"))
#   d_after <- sqrt(sum((dplyr::filter(result, keypoint == "head")[, c("x", "y", "z")] -
#                          dplyr::filter(result, keypoint == "tail")[, c("x", "y", "z")])^2))
#   expect_equal(d_before, d_after, tolerance = 1e-10)
# })
#
# test_that("rotate_coords_3d rotates all keypoints consistently", {
#   data <- create_three_keypoint_3d()
#   result <- rotate_coords(data, alignment_points = c("head", "tail"))
#   expect_equal(nrow(result), nrow(data))
#   expect_setequal(unique(result$keypoint), unique(data$keypoint))
# })
#
# test_that("rotate_coords_3d preserves data structure and class", {
#   data <- create_diagonal_3d()
#   result <- rotate_coords(data, alignment_points = c("head", "tail"))
#   expect_true("aniframe" %in% class(result))
#   expect_equal(names(result), names(data))
#   expect_equal(nrow(result), nrow(data))
# })
