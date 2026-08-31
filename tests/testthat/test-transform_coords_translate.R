# Translating coordinates (#20, #15)
#
# Identity, the index and the coordinate columns all come from the frame's
# declaration. The old code named `individual`, `keypoint`, `time`, `x` and
# `y` literally, so a frame declaring anything else failed outright.

simple <- function() {
  anicore::as_aniframe(
    data.frame(
      individual = "a",
      time = rep(1:2, each = 3),
      keypoint = rep(c("head", "neck", "tail"), 2),
      x = c(1, 2, 3, 11, 12, 13),
      y = c(10, 20, 30, 110, 120, 130)
    ),
    variables_what = c("individual", "keypoint")
  )
}


# Translating onto a member ----

test_that("the reference member ends up at the origin", {
  out <- as.data.frame(translate_coords(
    simple(),
    to = "head",
    level = "keypoint"
  ))
  head_rows <- out[out$keypoint == "head", ]

  expect_true(all(head_rows$x == 0))
  expect_true(all(head_rows$y == 0))
})

test_that("everything else becomes relative to it", {
  out <- as.data.frame(
    translate_coords(simple(), to = "head", level = "keypoint")
  )
  first <- out[out$time == 1 & out$keypoint == "neck", ]

  expect_equal(first$x, 1)
  expect_equal(first$y, 10)
})

test_that("each moment gets its own reference", {
  out <- as.data.frame(
    translate_coords(simple(), to = "head", level = "keypoint")
  )

  # `tail` is 2 from `head` at both moments, not 12 at the second.
  expect_equal(out[out$time == 2 & out$keypoint == "tail", ]$x, 2)
})

test_that("the row count and declaration are unchanged", {
  af <- simple()
  out <- translate_coords(af, to = "head", level = "keypoint")

  expect_equal(nrow(out), nrow(af))
  expect_s3_class(out, "aniframe")
  expect_equal(
    anicore::get_variables_what(out),
    anicore::get_variables_what(af)
  )
  expect_equal(anicore::get_index(out), anicore::get_index(af))
})

test_that("each subject is centred on its own reference", {
  af <- anicore::as_aniframe(
    data.frame(
      individual = rep(c("a", "b"), each = 2),
      time = 1,
      keypoint = rep(c("head", "tail"), 2),
      x = c(0, 1, 100, 102),
      y = c(0, 0, 0, 0)
    ),
    variables_what = c("individual", "keypoint")
  )

  out <- as.data.frame(translate_coords(af, to = "head", level = "keypoint"))

  expect_equal(out[out$individual == "a" & out$keypoint == "tail", ]$x, 1)
  expect_equal(out[out$individual == "b" & out$keypoint == "tail", ]$x, 2)
})


# The declarations it used to assume ----

test_that("a frame with no individual column works", {
  # The issue's second reproducer: `.data$individual` did not exist.
  af <- anicore::as_aniframe(
    data.frame(
      time = rep(1:2, each = 3),
      keypoint = rep(c("head", "neck", "tail"), 2),
      x = c(1, 0, -1, 2, 1, 0),
      y = c(0, 0, 0, 1, 1, 1)
    ),
    variables_what = "keypoint"
  )

  out <- as.data.frame(translate_coords(af, to = "head"))

  expect_true(all(out[out$keypoint == "head", ]$x == 0))
})

test_that("a frame indexed by something other than time works", {
  af <- anicore::as_aniframe(
    data.frame(
      frame = rep(1:2, each = 3),
      individual = "a",
      keypoint = rep(c("head", "neck", "tail"), 2),
      x = c(1, 0, -1, 2, 1, 0),
      y = c(0, 0, 0, 1, 1, 1)
    ),
    variables_what = c("individual", "keypoint"),
    index = "frame"
  )

  out <- as.data.frame(translate_coords(af, to = "head", level = "keypoint"))

  expect_equal(nrow(out), 6)
  expect_true(all(out[out$keypoint == "head", ]$x == 0))
})

test_that("coordinates carried by columns of any name work", {
  af <- anicore::as_aniframe(
    data.frame(
      time = rep(1:2, each = 2),
      keypoint = rep(c("head", "tail"), 2),
      u = c(1, 3, 5, 7),
      v = c(0, 4, 0, 4)
    ),
    variables_what = "keypoint",
    variables_where = c(x = "u", y = "v")
  )

  out <- as.data.frame(translate_coords(af, to = "head"))

  expect_equal(out[out$keypoint == "tail", ]$u, c(2, 2))
})


# Translating by a fixed offset ----

test_that("a fixed offset shifts every coordinate", {
  af <- simple()
  out <- as.data.frame(translate_coords(af, by = c(x = 1, y = 10)))
  before <- as.data.frame(af)

  expect_equal(out$x, before$x - 1)
  expect_equal(out$y, before$y - 10)
})

test_that("an offset need not name every axis", {
  af <- simple()
  out <- as.data.frame(translate_coords(af, by = c(x = 1)))
  before <- as.data.frame(af)

  expect_equal(out$x, before$x - 1)
  expect_equal(out$y, before$y)
})

test_that("a zero offset changes nothing", {
  af <- simple()

  expect_equal(
    as.data.frame(translate_coords(af, by = c(x = 0, y = 0)))$x,
    as.data.frame(af)$x
  )
})

test_that("a negative offset moves the other way", {
  af <- simple()
  out <- as.data.frame(translate_coords(af, by = c(x = -5)))

  expect_equal(out$x, as.data.frame(af)$x + 5)
})

test_that("3D coordinates translate too", {
  af <- anicore::as_aniframe(
    data.frame(
      time = 1:2,
      keypoint = "head",
      x = c(1, 2),
      y = c(3, 4),
      z = c(5, 6)
    ),
    variables_what = "keypoint"
  )

  out <- as.data.frame(translate_coords(af, by = c(x = 1, y = 1, z = 1)))

  expect_equal(out$z, c(4, 5))
})


# What it refuses ----

test_that("it needs something to translate to", {
  expect_error(translate_coords(simple()), "Nothing to translate to")
})

test_that("to and by are mutually exclusive", {
  expect_error(
    translate_coords(simple(), to = "head", level = "keypoint", by = c(x = 1)),
    "not both"
  )
})

test_that("the reference has to be a member of the level", {
  expect_error(
    translate_coords(simple(), to = "elbow", level = "keypoint"),
    "not a value of"
  )
})

test_that("only one reference member is accepted", {
  expect_error(
    translate_coords(simple(), to = c("head", "neck"), level = "keypoint"),
    "single value"
  )
})

test_that("a frame with several identity variables has to be told the level", {
  expect_error(translate_coords(simple(), to = "head"), "has to say which")
})

test_that("the level has to be an identity variable", {
  expect_error(
    translate_coords(simple(), to = "head", level = "time"),
    "not an identity variable"
  )
})

test_that("an offset has to name its axes", {
  expect_error(
    translate_coords(simple(), by = c(1, 10)),
    "must name an axis role"
  )
  expect_error(translate_coords(simple(), by = c(w = 1)), "not an axis")
})

test_that("a non-Cartesian frame is refused", {
  polar <- anicore::as_aniframe(
    data.frame(time = 1:3, keypoint = "head", rho = 1:3, phi = c(0, 1, 2)),
    variables_what = "keypoint"
  )

  expect_error(translate_coords(polar, to = "head"), "artesian")
})

test_that("the documented direction is the one implemented", {
  # `by` moves the origin, so the coordinates shift by its negative (#38).
  af <- simple()

  expect_equal(
    as.data.frame(translate_coords(af, by = c(x = 100)))$x,
    as.data.frame(af)$x - 100
  )
})
