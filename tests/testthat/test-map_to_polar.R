test_that("map_to_polar() correctly converts simple Cartesian data", {
  df <- data.frame(
    time = seq(1:4),
    keypoint = "nose",
    x = c(1, 0, -1, 0),
    y = c(0, 1, 0, -1)
  ) |>
    aniframe::as_aniframe()

  pol <- map_to_polar(df)

  expect_true(aniframe::is_polar(pol))
  expect_equal(pol$rho, c(1, 1, 1, 1), tolerance = 1e-8)
  expect_equal(pol$phi, c(0, pi / 2, pi, 3 * pi / 2), tolerance = 1e-8)
})

test_that("map_to_polar() drops the Cartesian columns", {
  df <- data.frame(time = 1:2, keypoint = "nose", x = c(3, 0), y = c(4, 1)) |>
    aniframe::as_aniframe()

  pol <- map_to_polar(df)

  expect_false(any(c("x", "y") %in% names(pol)))
  expect_true(all(c("rho", "phi") %in% names(pol)))
})

test_that("map_to_polar() round-trips through map_to_cartesian()", {
  df <- data.frame(
    time = 1:3,
    keypoint = "nose",
    x = c(1, 2, -3),
    y = c(4, -5, 6)
  ) |>
    aniframe::as_aniframe()

  back <- map_to_cartesian(map_to_polar(df))

  expect_equal(back$x, df$x, tolerance = 1e-8)
  expect_equal(back$y, df$y, tolerance = 1e-8)
})

test_that("map_to_polar() rejects data that is not already Cartesian", {
  df <- data.frame(time = 1:2, keypoint = "nose", x = c(1, 2), y = c(3, 4)) |>
    aniframe::as_aniframe()

  expect_error(map_to_polar(map_to_polar(df)))
})
