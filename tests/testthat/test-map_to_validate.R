# tests/testthat/test-coordinate-helpers.R

test_that("is_cartesian() correctly identifies Cartesian coordinate systems", {
  expect_true(aniframe::is_cartesian(data.frame(x = 1:3, y = 2:4)))
  expect_true(aniframe::is_cartesian(data.frame(z = 1)))
  expect_false(aniframe::is_cartesian(data.frame(a = 1, b = 2)))
})

test_that("ensure_is_cartesian() aborts when data is not Cartesian", {
  expect_silent(aniframe::ensure_is_cartesian(data.frame(x = 1)))
  expect_error(
    aniframe::ensure_is_cartesian(data.frame(a = 1, b = 2)),
    "This data frame is not in a Cartesian coordinate system"
  )
})

test_that("is_polar() correctly identifies polar coordinate systems", {
  expect_true(aniframe::is_polar(data.frame(rho = 1:3, phi = 2:4)))
  expect_false(aniframe::is_polar(data.frame(rho = 1, theta = 2)))
  expect_false(aniframe::is_polar(data.frame(x = 1, y = 2)))
})

test_that("ensure_is_polar() aborts when data is not polar", {
  expect_silent(aniframe::ensure_is_polar(data.frame(rho = 1, phi = 2)))
  expect_error(
    aniframe::ensure_is_polar(data.frame(x = 1, y = 2)),
    "This data frame is not in a polar coordinate system"
  )
})

test_that("is_cylindrical() correctly identifies cylindrical coordinate systems", {
  expect_true(aniframe::is_cylindrical(data.frame(rho = 1, phi = 2, z = 3)))
  expect_false(aniframe::is_cylindrical(data.frame(rho = 1, phi = 2)))
  expect_false(aniframe::is_cylindrical(data.frame(x = 1, y = 2, z = 3)))
})

test_that("ensure_is_cylindrical() aborts when data is not cylindrical", {
  expect_silent(aniframe::ensure_is_cylindrical(data.frame(
    rho = 1,
    phi = 2,
    z = 3
  )))
  expect_error(
    aniframe::ensure_is_cylindrical(data.frame(rho = 1, phi = 2)),
    "This data frame is not in a cylindrical coordinate system"
  )
})

test_that("is_spherical() correctly identifies spherical coordinate systems", {
  expect_true(aniframe::is_spherical(data.frame(rho = 1, phi = 2, theta = 3)))
  expect_false(aniframe::is_spherical(data.frame(rho = 1, phi = 2)))
  expect_false(aniframe::is_spherical(data.frame(x = 1, y = 2, z = 3)))
})

test_that("ensure_is_spherical() aborts when data is not spherical", {
  expect_silent(aniframe::ensure_is_spherical(data.frame(
    rho = 1,
    phi = 2,
    theta = 3
  )))
  expect_error(
    aniframe::ensure_is_spherical(data.frame(rho = 1, phi = 2)),
    "This data frame is not in a spherical coordinate system"
  )
})
