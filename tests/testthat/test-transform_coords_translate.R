# test-translate_coords_translate.R

# Test data setup ----
create_test_data <- function() {
  dplyr::tibble(
    time = seq(1:12),
    individual = "A",
    keypoint = "nose",
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    y = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
  ) |>
    aniframe::as_aniframe()
}

create_test_data_3d <- function() {
  data <- create_test_data()
  data$z <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200)
  aniframe::as_aniframe(data)
}

# Tests for translate_coords_vector ----
test_that("translate_coords_vector translates by single point", {
  data <- create_test_data()

  result <- translate_coords_vector(data, to_x = 1, to_y = 10)

  expect_equal(result$x, data$x - 1)
  expect_equal(result$y, data$y - 10)
  expect_equal(nrow(result), nrow(data))
})

test_that("translate_coords_vector translates by vector", {
  data <- create_test_data()
  to_x_vec <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  to_y_vec <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)

  result <- translate_coords_vector(data, to_x = to_x_vec, to_y = to_y_vec)

  # Each time point should be translated by corresponding vector element
  expected_x <- rep(0, 12)
  expected_y <- rep(0, 12)

  expect_equal(result$x, expected_x)
  expect_equal(result$y, expected_y)
})

test_that("translate_coords_vector handles 3D coordinates", {
  data <- create_test_data_3d()

  result <- translate_coords_vector(data, to_x = 1, to_y = 10, to_z = 100)

  expect_equal(result$x, data$x - 1)
  expect_equal(result$y, data$y - 10)
  expect_equal(result$z, data$z - 100)
})

test_that("translate_coords_vector preserves data structure", {
  data <- create_test_data()

  result <- translate_coords_vector(data, to_x = 0, to_y = 0)

  expect_equal(names(result), names(data))
  expect_equal(result$time, data$time)
  expect_equal(result$individual, data$individual)
  expect_equal(result$keypoint, data$keypoint)
})

# Tests for translate_coords_keypoint ----
test_that("translate_coords_keypoint makes coordinates relative to keypoint", {
  data <- create_test_data()

  result <- translate_coords_keypoint(data, to_keypoint = "nose")

  # Nose keypoints should be at origin for each individual at each time
  nose_coords <- result |> dplyr::filter(keypoint == "nose")
  expect_true(all(nose_coords$x == 0))
  expect_true(all(nose_coords$y == 0))
})

# test_that("translate_coords_keypoint preserves relative distances", {
#   data_a <- create_test_data()
#   data_b <- create_test_data() |> mutate(individual = "b")
#   data <- bind_rows(data_a, data_b)
#
#   # Original distance between nose and tail for individual A at time 1
#   original_dist_x <- 2 - 1  # tail_x - nose_x
#   original_dist_y <- 20 - 10  # tail_y - nose_y
#
#   result <- translate_coords_keypoint(data, to_keypoint = "nose")
#
#   # Distance should be preserved after translation
#   ind_a_time1 <- result |>
#     dplyr::filter(individual == "A", time == 1)
#   tail_coords <- ind_a_time1 |> dplyr::filter(keypoint == "tail")
#
#   expect_equal(tail_coords$x, original_dist_x)
#   expect_equal(tail_coords$y, original_dist_y)
# })

test_that("translate_coords_keypoint handles multiple individuals", {
  data_nose <- create_test_data()
  data_centroid <- create_test_data() |> dplyr::mutate(keypoint = "centroid")
  data <- dplyr::bind_rows(data_centroid, data_nose)

  result <- translate_coords_keypoint(data, to_keypoint = "nose")

  # Check both individuals have nose at origin
  nose_coords <- result |>
    dplyr::filter(keypoint == "nose")
  expect_true(all(nose_coords$x == 0))
  expect_true(all(nose_coords$y == 0))
})

test_that("translate_coords_keypoint preserves row count", {
  data <- create_test_data()

  result <- translate_coords_keypoint(data, to_keypoint = "nose")

  expect_equal(nrow(result), nrow(data))
})

# Tests for translate_coords (main function) ----
test_that("translate_coords works with single point", {
  data <- create_test_data()

  result <- translate_coords(data, to_x = 5, to_y = 50)

  expect_equal(result$x, data$x - 5)
  expect_equal(result$y, data$y - 50)
})

# test_that("translate_coords works with time-length vector", {
#   data <- create_test_data()
#   to_x_vec <- c(1, 2, 3)
#   to_y_vec <- c(10, 20, 30)
#
#   result <- translate_coords(data, to_x = to_x_vec, to_y = to_y_vec)
#
#   expected_x <- c(0, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9)
#   expect_equal(result$x, expected_x)
# })

test_that("translate_coords works with keypoint", {
  data <- create_test_data()

  result <- translate_coords(data, to_keypoint = "nose")

  nose_coords <- result |> dplyr::filter(keypoint == "nose")
  expect_true(all(nose_coords$x == 0))
  expect_true(all(nose_coords$y == 0))
})

test_that("translate_coords handles default parameters", {
  data <- create_test_data()

  result <- translate_coords(data)

  expect_equal(result$x, data$x)
  expect_equal(result$y, data$y)
})

test_that("translate_coords errors with multiple keypoints", {
  data <- create_test_data()

  expect_error(
    translate_coords(data, to_keypoint = c("nose", "tail")),
    "Only 1 keypoint can be supplied"
  )
})

test_that("translate_coords errors with non-existent keypoint", {
  data <- create_test_data()

  expect_error(
    translate_coords(data, to_keypoint = "head"),
    "Keypoint head is not among the keypoints"
  )
})

test_that("translate_coords errors with non-numeric coordinates", {
  data <- create_test_data()

  expect_error(
    translate_coords(data, to_x = "five", to_y = 10),
    "must be numeric"
  )
})

test_that("translate_coords returns same structure as input", {
  data <- create_test_data()

  result <- translate_coords(data, to_x = 1, to_y = 1)

  expect_equal(names(result), names(data))
  expect_equal(class(result), class(data))
  expect_equal(nrow(result), nrow(data))
})

test_that("translate_coords with 3D coordinates", {
  data <- create_test_data_3d()

  result <- translate_coords(data, to_x = 1, to_y = 10, to_z = 100)

  expect_equal(result$z, data$z - 100)
})

# Edge cases ----
test_that("translate_coords handles zero translation", {
  data <- create_test_data()

  result <- translate_coords(data, to_x = 0, to_y = 0)

  expect_equal(result$x, data$x)
  expect_equal(result$y, data$y)
})

test_that("translate_coords handles negative translations", {
  data <- create_test_data()

  result <- translate_coords(data, to_x = -5, to_y = -10)

  expect_equal(result$x, data$x + 5)
  expect_equal(result$y, data$y + 10)
})

test_that("translate_coords_keypoint handles single time point", {
  data <- create_test_data() |> dplyr::filter(time == 1)

  result <- translate_coords_keypoint(data, to_keypoint = "nose")

  expect_equal(nrow(result), nrow(data))
  nose_coords <- result |> dplyr::filter(keypoint == "nose")
  expect_true(all(nose_coords$x == 0))
})
