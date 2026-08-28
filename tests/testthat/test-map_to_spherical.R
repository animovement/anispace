test_that("map_to_spherical() correctly converts simple Cartesian data", {
  df <- data.frame(
    time = seq(1:4),
    keypoint = "nose",
    x = c(1, 0, -1, 0),
    y = c(0, 1, 0, -1),
    z = c(0, 0, 0, 0)
  ) |>
    anicore::as_aniframe()

  sph <- map_to_spherical(df)

  expect_true(anicore::is_spherical(sph))
  expect_equal(sph$rho, c(1, 1, 1, 1), tolerance = 1e-8)
  expect_equal(sph$phi, c(0, pi / 2, pi, 3 * pi / 2), tolerance = 1e-8)
  # With z = 0 every point lies in the xy-plane, a quarter turn from +z
  expect_equal(sph$theta, rep(pi / 2, 4), tolerance = 1e-8)
})

test_that("map_to_spherical() places a point on the +z axis at theta = 0", {
  df <- data.frame(time = 1, keypoint = "nose", x = 0, y = 0, z = 5) |>
    anicore::as_aniframe()

  sph <- map_to_spherical(df)

  expect_equal(sph$theta, 0, tolerance = 1e-8)
  # rho is the radial distance, so a point on the axis carries its height (#19)
  expect_equal(sph$rho, 5, tolerance = 1e-8)
})

test_that("map_to_spherical() reports rho as the radial distance", {
  # (3, 4, 12) is 13 from the origin and 5 from the z-axis. ISO 80000-2
  # spherical coordinates use the radial distance, so rho is 13; the
  # cylindrical radius of 5 belongs to map_to_cylindrical() (#19).
  df <- data.frame(time = 1, keypoint = "nose", x = 3, y = 4, z = 12) |>
    anicore::as_aniframe()

  expect_equal(map_to_spherical(df)$rho, 13, tolerance = 1e-8)
  expect_equal(map_to_cylindrical(df)$rho, 5, tolerance = 1e-8)
})

test_that("map_to_spherical() round-trips through map_to_cartesian()", {
  df <- data.frame(
    time = 1:2,
    keypoint = "nose",
    x = c(3, 1),
    y = c(4, -2),
    z = c(12, 2)
  ) |>
    anicore::as_aniframe()

  back <- map_to_cartesian(map_to_spherical(df))

  expect_equal(back$x, df$x, tolerance = 1e-6)
  expect_equal(back$y, df$y, tolerance = 1e-6)
  expect_equal(back$z, df$z, tolerance = 1e-6)
})

test_that("map_to_spherical() drops the Cartesian columns", {
  df <- data.frame(
    time = 1:2,
    keypoint = "nose",
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6)
  ) |>
    anicore::as_aniframe()

  sph <- map_to_spherical(df)

  expect_false(any(c("x", "y", "z") %in% names(sph)))
  expect_true(all(c("rho", "phi", "theta") %in% names(sph)))
})

# rho is the radial distance, not the cylindrical radius (#19) ----

test_that("map_to_spherical() returns the distance from the origin as rho", {
  # Previously rho was sqrt(x^2 + y^2) — the distance from the z-axis — while
  # theta already used the full radius, leaving the triple internally
  # inconsistent with the name "spherical".
  af <- anicore::as_aniframe(
    data.frame(time = 1, keypoint = "a", x = 3, y = 4, z = 12)
  )

  result <- map_to_spherical(af)

  expect_equal(result$rho, 13)
  expect_equal(result$theta, acos(12 / 13))
  expect_equal(result$phi, atan2(4, 3))
})

test_that("a point on the z-axis survives the round trip", {
  # The sharp case. With rho as the xy-plane radius, a point on the z-axis
  # has rho = 0 and theta = 0, and its height cannot be recovered — the old
  # code returned z = 0, silently moving the point to the origin.
  af <- anicore::as_aniframe(
    data.frame(time = c(1, 2), keypoint = "a", x = 0, y = 0, z = c(5, -5))
  )

  back <- map_to_cartesian(map_to_spherical(af))

  expect_equal(back$z, c(5, -5))
})

test_that("spherical coordinates round-trip away from the poles", {
  af <- anicore::as_aniframe(data.frame(
    time = 1:3,
    keypoint = "a",
    x = c(3, -2, 1),
    y = c(4, -3, 0),
    z = c(12, 6, 0)
  ))

  back <- map_to_cartesian(map_to_spherical(af))

  expect_equal(back$x, c(3, -2, 1))
  expect_equal(back$y, c(4, -3, 0))
  expect_equal(back$z, c(12, 6, 0))
})

test_that("cylindrical rho keeps its own meaning", {
  # rho means the distance from the z-axis in a cylindrical frame and the
  # distance from the origin in a spherical one. That is the ISO 80000-2
  # convention, and conflating the two is what #19 was.
  af <- anicore::as_aniframe(
    data.frame(time = 1, keypoint = "a", x = 3, y = 4, z = 12)
  )

  expect_equal(map_to_cylindrical(af)$rho, 5)
  expect_equal(map_to_spherical(af)$rho, 13)
})

test_that("spherical_to_z() recovers height at the poles", {
  expect_equal(spherical_to_z(13, acos(12 / 13)), 12)
  expect_equal(spherical_to_z(5, 0), 5)
  expect_equal(spherical_to_z(5, pi), -5)
  expect_equal(spherical_to_z(NA_real_, 0), NA_real_)
  expect_equal(spherical_to_z(5, NA_real_), NA_real_)
})
