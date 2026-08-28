# The rotation maths, and the guards on the way in
#
# Degenerate inputs -- parallel vectors, zero-length ones, missing points --
# are the cases that never arise from a well-formed frame and are exactly
# the ones worth pinning down.

test_that("a zero-length axis rotates by nothing", {
  expect_equal(rotation_from_axis_angle(c(0, 0, 0), pi / 2), diag(3))
})

test_that("a vector already on the target is left alone", {
  expect_equal(rotation_onto(c(2, 0, 0), c(1, 0, 0)), diag(3))
})

test_that("an antiparallel vector is turned right round", {
  r <- rotation_onto(c(1, 0, 0), c(-1, 0, 0))

  expect_equal(as.vector(r %*% c(1, 0, 0)), c(-1, 0, 0), tolerance = 1e-8)
  expect_equal(det(r), 1, tolerance = 1e-8)
})

test_that("an antiparallel vector along y is handled too", {
  # The perpendicular is chosen differently when the vector is mostly x.
  r <- rotation_onto(c(0, 1, 0), c(0, -1, 0))

  expect_equal(as.vector(r %*% c(0, 1, 0)), c(0, -1, 0), tolerance = 1e-8)
})

test_that("a zero-length vector gives no rotation", {
  expect_equal(rotation_onto(c(0, 0, 0), c(1, 0, 0)), diag(3))
  expect_equal(rotation_onto(c(1, 0, 0), c(0, 0, 0)), diag(3))
})

test_that("parallel vectors span no plane, so no basis", {
  expect_null(orthonormal_basis(c(1, 0, 0), c(2, 0, 0)))
  expect_null(orthonormal_basis(c(0, 0, 0), c(1, 0, 0)))
})

test_that("a basis rotation falls back when the vectors are parallel", {
  expect_equal(
    rotation_onto_basis(c(1, 0, 0), c(2, 0, 0), c(1, 0, 0), c(0, 1, 0)),
    diag(3)
  )
})

test_that("a rotation matrix is orthogonal with determinant one", {
  r <- rotation_onto_basis(c(0, 0, 1), c(0, 1, 0), c(1, 0, 0), c(0, 1, 0))

  expect_equal(t(r) %*% r, diag(3), tolerance = 1e-8)
  expect_equal(det(r), 1, tolerance = 1e-8)
})

test_that("a missing alignment point leaves the row unrotated", {
  expect_null(rotation_for(c(NA, 0, 0), NULL, list(primary = c(1, 0, 0))))
})


# The guards ----

test_that("a frame declaring no identity is refused", {
  af <- suppressWarnings(anicore::as_aniframe(
    data.frame(time = 1:3, x = 1:3, y = 1:3),
    variables_what = character(0)
  ))

  expect_error(translate_coords(af, to = "head"), "declares no identity")
})

test_that("the level has to be a single name", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

  expect_error(
    translate_coords(af, to = "head", level = c("individual", "keypoint")),
    "single column name"
  )
})

test_that("a frame with fewer than two axes is refused", {
  af <- anicore::as_aniframe(
    data.frame(time = 1:3, keypoint = "head", x = 1:3),
    variables_what = "keypoint",
    variables_where = "x"
  )

  expect_error(translate_coords(af, to = "head"), "at least two axes")
})
