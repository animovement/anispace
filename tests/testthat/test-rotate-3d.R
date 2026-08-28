# Rotating in three dimensions (#4)
#
# One implementation covers both dimensionalities: a rotation is a matrix,
# and 2D is the case where the axis is fixed to z. What differs is how much
# the alignment points determine -- two give a direction and leave the roll
# about it free, three fix the orientation outright.

body_3d <- function() {
  anicore::as_aniframe(
    data.frame(
      time = rep(1:2, each = 3),
      keypoint = rep(c("head", "tail", "ear"), 2),
      x = c(0, 0, 0, 0, 0, 0),
      y = c(0, 0, 1, 0, 2, 0),
      z = c(0, 2, 0, 0, 0, 1)
    ),
    variables_what = "keypoint"
  )
}

member <- function(frame, name, at = 1) {
  d <- as.data.frame(frame)
  d[d$keypoint == name & d$time == at, c("x", "y", "z")]
}


# Two points ----

test_that("two points put the primary axis onto x", {
  out <- rotate_coords(body_3d(), align = c("head", "tail"))
  tail <- member(out, "tail")

  expect_equal(tail$x, 2, tolerance = 1e-8)
  expect_equal(tail$y, 0, tolerance = 1e-8)
  expect_equal(tail$z, 0, tolerance = 1e-8)
})

test_that("the reference point stays where it was", {
  out <- rotate_coords(body_3d(), align = c("head", "tail"))

  expect_equal(
    unname(unlist(member(out, "head"))),
    c(0, 0, 0),
    tolerance = 1e-8
  )
})


# Three points ----

test_that("three points put the second axis into the xy plane", {
  out <- rotate_coords(body_3d(), align = c("head", "tail", "ear"))

  expect_equal(member(out, "tail")$x, 2, tolerance = 1e-8)
  expect_equal(member(out, "ear")$z, 0, tolerance = 1e-8)
})

test_that("a third point is refused on a 2D frame", {
  flat <- anicore::as_aniframe(
    data.frame(
      time = 1,
      keypoint = c("head", "tail", "ear"),
      x = c(0, 1, 0),
      y = c(0, 0, 1)
    ),
    variables_what = "keypoint"
  )

  expect_error(
    rotate_coords(flat, align = c("head", "tail", "ear")),
    "three dimensions"
  )
})


# What a rotation must not change ----

test_that("distances between points survive", {
  before <- body_3d()
  after <- rotate_coords(before, align = c("head", "tail", "ear"))

  at_one <- function(f) {
    d <- as.data.frame(f)
    dist(d[d$time == 1, c("x", "y", "z")])
  }

  expect_equal(
    as.vector(at_one(after)),
    as.vector(at_one(before)),
    tolerance = 1e-8
  )
})

test_that("the row count and declaration survive", {
  before <- body_3d()
  after <- rotate_coords(before, align = c("head", "tail"))

  expect_equal(nrow(after), nrow(before))
  expect_s3_class(after, "aniframe")
  expect_equal(anicore::get_axes(after), anicore::get_axes(before))
})

test_that("each moment is rotated by its own angle", {
  # `tail` points along +z at the first moment and +y at the second; both
  # should land on +x.
  out <- rotate_coords(body_3d(), align = c("head", "tail"))

  expect_equal(member(out, "tail", at = 1)$x, 2, tolerance = 1e-8)
  expect_equal(member(out, "tail", at = 2)$x, 2, tolerance = 1e-8)
})
