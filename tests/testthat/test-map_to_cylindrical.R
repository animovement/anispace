test_that("map_to_cylindrical() correctly converts simple Cartesian data", {
  df <- data.frame(
    time = seq(1:4),
    keypoint = "nose",
    x = c(1, 0, -1, 0),
    y = c(0, 1, 0, -1),
    z = c(0, 1, 2, 3)
  ) |>
    as_aniframe()

  cyl <- map_to_cylindrical(df)

  expect_true(aniframe::is_cylindrical(cyl))
  expect_equal(cyl$rho, c(1, 1, 1, 1), tolerance = 1e-8)
  expect_equal(cyl$phi, c(0, pi / 2, pi, 3 * pi / 2), tolerance = 1e-8)
  expect_equal(cyl$z, df$z)
})
