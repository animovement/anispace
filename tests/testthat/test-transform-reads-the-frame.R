# The transforms read the frame rather than naming its columns (#20, #15)
#
# The reproducers from the issue, plus the multi-trial case the standing
# TODO warned about, which turned out to be worse than "will likely break".

test_that("two trials are rotated separately, not joined together", {
  # `left_join(angles, by = "time")` matched every trial's angle to every
  # trial's rows, so 12 rows came back as 48 with 36 duplicates.
  af <- anicore::as_aniframe(
    data.frame(
      individual = "a",
      trial = rep(c(1, 2), each = 6),
      time = rep(rep(1:2, each = 3), 2),
      keypoint = rep(c("head", "neck", "tail"), 4),
      x = c(1, 0, -1, 2, 1, 0, 0, 0, 0, 1, 1, 1),
      y = c(0, 0, 0, 1, 1, 1, 1, 0, -1, 2, 1, 0)
    ),
    variables_what = c("individual", "keypoint"),
    variables_when = "trial"
  )

  out <- rotate_coords(af, align = c("head", "neck"), level = "keypoint")
  keys <- as.data.frame(out)[, c("trial", "time", "keypoint")]

  expect_equal(nrow(out), nrow(af))
  expect_equal(sum(duplicated(keys)), 0)
})

test_that("the egocentric transform works without an individual column", {
  # The issue's second reproducer.
  af <- anicore::as_aniframe(
    data.frame(
      time = rep(1:2, each = 3),
      keypoint = rep(c("head", "neck", "tail"), 2),
      x = c(1, 0, -1, 2, 1, 0),
      y = c(0, 0, 0, 1, 1, 1)
    ),
    variables_what = "keypoint"
  )

  out <- transform_to_egocentric(af, to = "head", align = c("head", "neck"))
  d <- as.data.frame(out)

  expect_equal(nrow(out), 6)
  expect_true(all(abs(d[d$keypoint == "head", c("x", "y")]) < 1e-8))
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

  expect_equal(
    nrow(rotate_coords(af, align = c("head", "neck"), level = "keypoint")),
    6
  )
  expect_equal(
    nrow(transform_to_egocentric(
      af,
      to = "head",
      align = c("head", "neck"),
      level = "keypoint"
    )),
    6
  )
})


# Centring the rotation ----

test_that("rotation is about the origin by default", {
  af <- anicore::as_aniframe(
    data.frame(
      time = 1,
      keypoint = c("head", "tail"),
      x = c(1, 1),
      y = c(0, 1)
    ),
    variables_what = "keypoint"
  )

  out <- as.data.frame(rotate_coords(af, align = c("head", "tail")))

  # head->tail points along +y, so a quarter turn puts it on +x -- and the
  # whole frame swings around (0, 0) rather than around the head.
  expect_equal(
    out[out$keypoint == "tail", ]$x - out[out$keypoint == "head", ]$x,
    1,
    tolerance = 1e-8
  )
  expect_false(isTRUE(all.equal(out[out$keypoint == "head", ]$x, 1)))
})

test_that("about names a member to rotate around", {
  af <- anicore::as_aniframe(
    data.frame(
      time = 1,
      keypoint = c("head", "tail"),
      x = c(1, 1),
      y = c(0, 1)
    ),
    variables_what = "keypoint"
  )

  out <- as.data.frame(
    rotate_coords(af, align = c("head", "tail"), about = "head")
  )

  # Rotating about the head leaves the head exactly where it was.
  expect_equal(out[out$keypoint == "head", ]$x, 1, tolerance = 1e-8)
  expect_equal(out[out$keypoint == "head", ]$y, 0, tolerance = 1e-8)
})

test_that("about takes a fixed point too", {
  af <- anicore::as_aniframe(
    data.frame(
      time = 1,
      keypoint = c("head", "tail"),
      x = c(10, 10),
      y = c(10, 11)
    ),
    variables_what = "keypoint"
  )

  out <- as.data.frame(
    rotate_coords(af, align = c("head", "tail"), about = c(x = 10, y = 10))
  )

  expect_equal(out[out$keypoint == "head", ]$x, 10, tolerance = 1e-8)
  expect_equal(out[out$keypoint == "head", ]$y, 10, tolerance = 1e-8)
})


# Egocentric without rotating ----

test_that("omitting align re-centres without reorienting", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

  centred <- transform_to_egocentric(af, to = "head", level = "keypoint")
  translated <- translate_coords(af, to = "head", level = "keypoint")

  expect_equal(as.data.frame(centred)$x, as.data.frame(translated)$x)
  expect_equal(
    as.character(anicore::get_metadata(centred)$reference_frame),
    "egocentric"
  )
})


# Which way a quarter turn goes (#29) ----

test_that("perpendicular follows the frame's declared sense", {
  # A quarter turn from +x lands on +y counter-clockwise and on -y
  # clockwise. Which one a frame means is what its axis directions say.
  base <- anicore::as_aniframe(
    data.frame(
      time = 1,
      keypoint = c("head", "tail"),
      x = c(0, 1),
      y = c(0, 0)
    ),
    variables_what = "keypoint"
  )

  ccw <- anicore::set_axis_directions(base, c(x = "right", y = "up"))
  cw <- anicore::set_axis_directions(base, c(x = "right", y = "down"))

  expect_equal(anicore::get_angle_direction(ccw), "counter_clockwise")
  expect_equal(anicore::get_angle_direction(cw), "clockwise")

  turn <- function(af) {
    d <- as.data.frame(
      rotate_coords(af, align = c("head", "tail"), align_perpendicular = TRUE)
    )
    round(d[d$keypoint == "tail", ]$y, 8)
  }

  expect_equal(turn(ccw), 1)
  expect_equal(turn(cw), -1)
})

test_that("a frame declaring nothing keeps the counter-clockwise convention", {
  af <- anicore::as_aniframe(
    data.frame(
      time = 1,
      keypoint = c("head", "tail"),
      x = c(0, 1),
      y = c(0, 0)
    ),
    variables_what = "keypoint"
  )

  d <- as.data.frame(
    rotate_coords(af, align = c("head", "tail"), align_perpendicular = TRUE)
  )

  expect_equal(round(d[d$keypoint == "tail", ]$y, 8), 1)
})

test_that("a row whose alignment point is missing is left unrotated", {
  af <- anicore::as_aniframe(
    data.frame(
      time = rep(1:2, each = 2),
      keypoint = rep(c("head", "tail"), 2),
      x = c(0, 1, 0, NA),
      y = c(0, 0, 0, NA)
    ),
    variables_what = "keypoint"
  )

  out <- as.data.frame(rotate_coords(af, align = c("head", "tail")))

  expect_equal(nrow(out), 4)
  expect_true(all(is.na(out[
    out$time == 2 & out$keypoint == "tail",
    c("x", "y")
  ])))
})
