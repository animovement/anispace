test_that("map_to_polar() correctly converts simple Cartesian data", {
  df <- data.frame(
    time = seq(1:4),
    keypoint = "nose",
    x = c(1, 0, -1, 0),
    y = c(0, 1, 0, -1)
  ) |>
    anicore::as_anipoint()

  pol <- map_to_polar(df)

  expect_true(anicore::is_polar(pol))
  expect_equal(pol$rho, c(1, 1, 1, 1), tolerance = 1e-8)
  expect_equal(pol$phi, c(0, pi / 2, pi, -pi / 2), tolerance = 1e-8)
})

test_that("map_to_polar() drops the Cartesian columns", {
  df <- data.frame(time = 1:2, keypoint = "nose", x = c(3, 0), y = c(4, 1)) |>
    anicore::as_anipoint()

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
    anicore::as_anipoint()

  back <- map_to_cartesian(map_to_polar(df))

  expect_equal(back$x, df$x, tolerance = 1e-8)
  expect_equal(back$y, df$y, tolerance = 1e-8)
})

test_that("map_to_polar() rejects data that is not already Cartesian", {
  df <- data.frame(time = 1:2, keypoint = "nose", x = c(1, 2), y = c(3, 4)) |>
    anicore::as_anipoint()

  expect_error(map_to_polar(map_to_polar(df)))
})

# phi is signed, (-pi, pi], like every direction in the suite (#59)

quadrants <- function() {
  # Every quadrant, both halves of each axis, and the negative x-axis
  # approached from below (a negative zero y), where atan2() gives -pi
  data.frame(
    time = 1:10,
    keypoint = "nose",
    x = c(1, -2, -3, 4, 2, 0, -5, 0, -1.5, 0.25),
    y = c(2, 1, -4, -3, 0, 3, 0, -2, -0, 1e-9),
    z = c(0.5, -1, 2, -2, 0, 1, -3, 4, 1, 0)
  )
}

test_that("map_to_polar() writes phi in (-pi, pi], with pi on the negative x-axis", {
  df <- quadrants()
  pol <- map_to_polar(anicore::as_anipoint(df[c("time", "keypoint", "x", "y")]))

  expect_identical(pol$phi[c(5, 6, 7, 8, 9)], c(0, pi / 2, pi, -pi / 2, pi))
  expect_true(all(pol$phi > -pi & pol$phi <= pi))
  expect_true(all(pol$phi[3:4] < 0))
  expect_equal(pol$phi, atan2(df$y, df$x) + c(rep(0, 8), 2 * pi, 0))
})

test_that("map_to_cylindrical() and map_to_spherical() write phi in (-pi, pi]", {
  af <- anicore::as_anipoint(quadrants())

  for (to in list(map_to_cylindrical, map_to_spherical)) {
    phi <- to(af)$phi
    expect_true(all(phi > -pi & phi <= pi))
    expect_identical(phi[c(7, 8, 9)], c(pi, -pi / 2, pi))
  }
})

test_that("a degree frame gets phi in (-180, 180]", {
  df <- quadrants()
  planar <- anicore::as_anipoint(df[c("time", "keypoint", "x", "y")]) |>
    anicore::set_metadata(unit_angle = "deg")
  spatial <- anicore::as_anipoint(df) |>
    anicore::set_metadata(unit_angle = "deg")

  for (phi in list(
    map_to_polar(planar)$phi,
    map_to_cylindrical(spatial)$phi,
    map_to_spherical(spatial)$phi
  )) {
    expect_true(all(phi > -180 & phi <= 180))
    expect_identical(phi[c(5, 6, 7, 8, 9)], c(0, 90, 180, -90, 180))
    expect_equal(phi[1:4], atan2(df$y, df$x)[1:4] * 180 / pi)
  }
})

test_that("every quadrant round-trips exactly, in radians and degrees", {
  df <- quadrants()
  for (unit in c("rad", "deg")) {
    planar <- anicore::as_anipoint(df[c("time", "keypoint", "x", "y")]) |>
      anicore::set_metadata(unit_angle = unit)
    spatial <- anicore::as_anipoint(df) |>
      anicore::set_metadata(unit_angle = unit)

    back <- map_to_cartesian(map_to_polar(planar))
    expect_equal(back$x, df$x, tolerance = 1e-12)
    expect_equal(back$y, df$y, tolerance = 1e-12)

    for (to in list(map_to_cylindrical, map_to_spherical)) {
      back <- map_to_cartesian(to(spatial))
      expect_equal(back$x, df$x, tolerance = 1e-12)
      expect_equal(back$y, df$y, tolerance = 1e-12)
      expect_equal(back$z, df$z, tolerance = 1e-12)
    }
  }
})

test_that("map_to_cartesian() reads phi in [0, 2pi) as well as signed", {
  signed <- c(0, 3 * pi / 4, pi, -3 * pi / 4, -pi / 4)
  unsigned <- anicore::wrap_angle(signed, "2pi")
  extra <- list(
    polar = list(),
    cylindrical = list(z = 1),
    spherical = list(theta = pi / 3)
  )
  frame <- function(phi, unit, system) {
    angle <- function(rad) if (unit == "deg") rad * 180 / pi else rad
    cols <- extra[[system]]
    if (!is.null(cols$theta)) {
      cols$theta <- angle(cols$theta)
    }
    do.call(data.frame, c(list(time = 1:5, rho = 2, phi = angle(phi)), cols)) |>
      anicore::as_anipoint() |>
      anicore::set_metadata(unit_angle = unit)
  }

  for (unit in c("rad", "deg")) {
    for (system in names(extra)) {
      from_signed <- map_to_cartesian(frame(signed, unit, system))
      from_unsigned <- map_to_cartesian(frame(unsigned, unit, system))
      expect_equal(from_unsigned$x, from_signed$x, tolerance = 1e-12)
      expect_equal(from_unsigned$y, from_signed$y, tolerance = 1e-12)
    }
  }
})
