# The conversions honour the frame's unit_angle and declared axes (#47).

in_degrees <- function(data) {
  anicore::set_metadata(data, unit_angle = "deg")
}

test_that("map_to_cartesian() reads phi in degrees from a degree frame", {
  polar <- data.frame(time = 0:3, rho = 1, phi = c(0, 90, 180, 270)) |>
    anicore::as_anipoint() |>
    in_degrees()

  result <- map_to_cartesian(polar)

  expect_equal(result$x, c(1, 0, -1, 0), tolerance = 1e-12)
  expect_equal(result$y, c(0, 1, 0, -1), tolerance = 1e-12)
})

test_that("map_to_cartesian() reads phi and theta in degrees for 3D frames", {
  spherical <- data.frame(
    time = 0:1,
    rho = 2,
    phi = c(90, 0),
    theta = c(90, 0)
  ) |>
    anicore::as_anipoint() |>
    in_degrees()
  cylindrical <- data.frame(time = 0, rho = 1, phi = 180, z = 5) |>
    anicore::as_anipoint() |>
    in_degrees()

  sph <- map_to_cartesian(spherical)
  expect_equal(sph$x, c(0, 0), tolerance = 1e-12)
  expect_equal(sph$y, c(2, 0), tolerance = 1e-12)
  expect_equal(sph$z, c(0, 2), tolerance = 1e-12)

  cyl <- map_to_cartesian(cylindrical)
  expect_equal(c(cyl$x, cyl$y, cyl$z), c(-1, 0, 5), tolerance = 1e-12)
})

test_that("map_to_polar(), _cylindrical() and _spherical() write degrees", {
  cartesian <- data.frame(time = 0:1, x = c(0, 1), y = c(1, 1), z = c(0, 0)) |>
    anicore::as_anipoint() |>
    in_degrees()

  expect_equal(map_to_cylindrical(cartesian)$phi, c(90, 45))
  expect_equal(map_to_spherical(cartesian)$phi, c(90, 45))
  expect_equal(map_to_spherical(cartesian)$theta, c(90, 90))

  planar <- data.frame(time = 0:1, x = c(0, 1), y = c(1, 1)) |>
    anicore::as_anipoint() |>
    in_degrees()
  polar <- map_to_polar(planar)
  expect_equal(polar$phi, c(90, 45))
  expect_equal(as.character(anicore::get_metadata(polar, "unit_angle")), "deg")
})

test_that("a degree frame round-trips through every system", {
  planar <- data.frame(time = 1:4, x = c(1, -2, 3, -4), y = c(4, 3, -2, -1)) |>
    anicore::as_anipoint() |>
    in_degrees()
  spatial <- data.frame(
    time = 1:4,
    x = c(1, -2, 3, -4),
    y = c(4, 3, -2, -1),
    z = c(-1, 2, 0.5, 3)
  ) |>
    anicore::as_anipoint() |>
    in_degrees()

  back <- map_to_cartesian(map_to_polar(planar))
  expect_equal(back$x, planar$x, tolerance = 1e-12)
  expect_equal(back$y, planar$y, tolerance = 1e-12)

  for (to in list(map_to_cylindrical, map_to_spherical)) {
    back <- map_to_cartesian(to(spatial))
    expect_equal(back$x, spatial$x, tolerance = 1e-12)
    expect_equal(back$y, spatial$y, tolerance = 1e-12)
    expect_equal(back$z, spatial$z, tolerance = 1e-12)
  }
})

test_that("the conversions read renamed axis columns from the frame", {
  df <- data.frame(
    time = rep(1:3, 2),
    individual = rep(c("a", "b"), each = 3),
    u = c(1, -2, 3, 0.5, 2, -1),
    v = c(2, 1, -1, 3, -2, 1),
    h = c(0, 1, 2, -1, 0.5, 3)
  )
  renamed <- anicore::as_anipoint(
    df,
    variables_where = c(x = "u", y = "v", z = "h")
  )
  standard <- anicore::as_anipoint(
    dplyr::rename(df, x = "u", y = "v", z = "h")
  )

  cyl <- map_to_cylindrical(renamed)
  expect_equal(cyl$phi, map_to_cylindrical(standard)$phi)
  # The z axis keeps its column
  expect_equal(anicore::get_axes(cyl)[["z"]], "h")

  sph <- map_to_spherical(renamed)
  expect_equal(sph$theta, map_to_spherical(standard)$theta)

  polar_renamed <- anicore::as_anipoint(
    data.frame(time = 0:1, r = 1, a = c(0, pi / 2)),
    variables_where = c(rho = "r", phi = "a")
  )
  back <- map_to_cartesian(polar_renamed)
  expect_equal(back$x, c(1, 0), tolerance = 1e-12)
  expect_equal(back$y, c(0, 1), tolerance = 1e-12)
  expect_false(any(c("r", "a") %in% names(back)))
})
