make_frame <- function() {
  data.frame(
    time = rep(1:2, each = 3),
    individual = "a",
    keypoint = rep(c("head", "neck", "tail"), times = 2),
    x = c(1, 0, -1, 2, 1, 0),
    y = c(0, 0, 0, 1, 1, 1)
  ) |>
    anicore::as_aniframe()
}

test_that("transform_to_egocentric() puts the reference keypoint at the origin", {
  ego <- transform_to_egocentric(
    make_frame(),
    level = "keypoint",
    to = "head",
    align = c("head", "neck")
  )

  head_rows <- ego[ego$keypoint == "head", ]
  expect_equal(head_rows$x, rep(0, 2), tolerance = 1e-8)
  expect_equal(head_rows$y, rep(0, 2), tolerance = 1e-8)
})

test_that("transform_to_egocentric() aligns the chosen axis", {
  ego <- transform_to_egocentric(
    make_frame(),
    level = "keypoint",
    to = "head",
    align = c("head", "neck")
  )

  # With head at the origin, neck should lie on the x-axis
  neck <- ego[ego$keypoint == "neck", ]
  expect_equal(neck$y, rep(0, 2), tolerance = 1e-8)
})

test_that("align_perpendicular rotates the axis by a quarter turn", {
  along <- transform_to_egocentric(
    make_frame(),
    level = "keypoint",
    to = "head",
    align = c("head", "neck")
  )
  across <- transform_to_egocentric(
    make_frame(),
    level = "keypoint",
    to = "head",
    align = c("head", "neck"),
    align_perpendicular = TRUE
  )

  neck_along <- along[along$keypoint == "neck", ]
  neck_across <- across[across$keypoint == "neck", ]

  # The same distance from the origin, but on the other axis
  expect_equal(
    sqrt(neck_along$x^2 + neck_along$y^2),
    sqrt(neck_across$x^2 + neck_across$y^2),
    tolerance = 1e-8
  )
  expect_equal(neck_across$x, rep(0, 2), tolerance = 1e-8)
})

test_that("transform_to_egocentric() keeps every row and returns an aniframe", {
  df <- make_frame()
  ego <- transform_to_egocentric(
    df,
    level = "keypoint",
    to = "head",
    align = c("head", "neck")
  )

  expect_true(anicore::is_aniframe(ego))
  expect_equal(nrow(ego), nrow(df))
})
