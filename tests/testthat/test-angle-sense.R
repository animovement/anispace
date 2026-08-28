# The sense of rotation is carried, not changed (#29)
#
# `phi` is measured from +x toward +y, always. Whether that appears
# clockwise or counter-clockwise is what the frame's axis directions say,
# and mapping to polar re-represents the points without touching either.

sweep <- function(y_dir) {
  # The same physical motion: a point sweeping anticlockwise through 0, 45
  # and 90 degrees as a viewer sees it. A y-down camera stores it mirrored.
  d <- data.frame(
    time = 1:3,
    keypoint = "nose",
    x = c(1, sqrt(0.5), 0),
    y = c(0, sqrt(0.5), 1)
  )
  if (identical(y_dir, "down")) {
    d$y <- -d$y
  }

  anicore::as_aniframe(d, variables_what = "keypoint") |>
    anicore::set_axis_directions(c(x = "right", y = y_dir))
}

degrees <- function(frame) round(as.data.frame(frame)$phi * 180 / pi, 6)


test_that("phi is measured from +x toward +y, whatever the frame declares", {
  expect_equal(degrees(map_to_polar(sweep("up"))), c(0, 45, 90))
  expect_equal(degrees(map_to_polar(sweep("down"))), c(0, 315, 270))
})

test_that("the declared sense survives the transform", {
  # The polar frame has no axes of its own to be handed -- it borrows the
  # sense from the space, which the transform does not change.
  expect_equal(
    anicore::get_angle_direction(map_to_polar(sweep("up"))),
    "counter_clockwise"
  )
  expect_equal(
    anicore::get_angle_direction(map_to_polar(sweep("down"))),
    "clockwise"
  )
})

test_that("the two are reconcilable, which is what makes them comparable", {
  # 315 clockwise and 45 counter-clockwise are the same physical direction.
  # The metadata is what says so, and normalising is the caller's to ask for.
  normalised <- anicore::set_angle_direction(sweep("down"), "counter_clockwise")

  expect_equal(degrees(map_to_polar(normalised)), c(0, 45, 90))
  expect_equal(
    anicore::get_angle_direction(map_to_polar(normalised)),
    "counter_clockwise"
  )
})

test_that("a round trip returns the coordinates it was given", {
  af <- sweep("down")
  rt <- map_to_cartesian(map_to_polar(af))

  expect_equal(as.data.frame(rt)$y, as.data.frame(af)$y, tolerance = 1e-8)
  expect_equal(anicore::get_axis_directions(rt)[["y"]], "down")
})
