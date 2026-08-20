test_that("map_to_spherical() correctly converts simple Cartesian data", {
  df <- data.frame(
    time = seq(1:4),
    keypoint = "nose",
    x = c(1, 0, -1, 0),
    y = c(0, 1, 0, -1),
    z = c(0, 0, 0, 0)
  ) |>
    aniframe::as_aniframe()

  sph <- map_to_spherical(df)

  expect_true(aniframe::is_spherical(sph))
  expect_equal(sph$rho, c(1, 1, 1, 1), tolerance = 1e-8)
  expect_equal(sph$phi, c(0, pi / 2, pi, 3 * pi / 2), tolerance = 1e-8)
  # With z = 0 every point lies in the xy-plane, a quarter turn from +z
  expect_equal(sph$theta, rep(pi / 2, 4), tolerance = 1e-8)
})

test_that("map_to_spherical() places a point on the +z axis at theta = 0", {
  df <- data.frame(time = 1, keypoint = "nose", x = 0, y = 0, z = 5) |>
    aniframe::as_aniframe()

  sph <- map_to_spherical(df)

  expect_equal(sph$theta, 0, tolerance = 1e-8)
  # rho is currently the cylindrical radius, so a point on the axis has rho = 0
  expect_equal(sph$rho, 0, tolerance = 1e-8)
})

test_that("map_to_spherical() currently reports rho as the cylindrical radius", {
  # NOTE: this pins current behaviour, which is *not* the standard convention.
  # (3, 4, 12) is 13 from the origin but 5 from the z-axis, and rho is 5.
  # ISO 80000-2 spherical coordinates use the radial distance, so this should
  # become 13 -- see issue #19. When that is fixed, this test should fail and
  # be updated rather than deleted.
  df <- data.frame(time = 1, keypoint = "nose", x = 3, y = 4, z = 12) |>
    aniframe::as_aniframe()

  expect_equal(map_to_spherical(df)$rho, 5, tolerance = 1e-8)
})

test_that("map_to_spherical() round-trips through map_to_cartesian()", {
  df <- data.frame(
    time = 1:2, keypoint = "nose",
    x = c(3, 1), y = c(4, -2), z = c(12, 2)
  ) |>
    aniframe::as_aniframe()

  back <- map_to_cartesian(map_to_spherical(df))

  expect_equal(back$x, df$x, tolerance = 1e-6)
  expect_equal(back$y, df$y, tolerance = 1e-6)
  expect_equal(back$z, df$z, tolerance = 1e-6)
})

test_that("map_to_spherical() drops the Cartesian columns", {
  df <- data.frame(time = 1:2, keypoint = "nose", x = c(1, 2), y = c(3, 4), z = c(5, 6)) |>
    aniframe::as_aniframe()

  sph <- map_to_spherical(df)

  expect_false(any(c("x", "y", "z") %in% names(sph)))
  expect_true(all(c("rho", "phi", "theta") %in% names(sph)))
})
