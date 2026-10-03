# Aligning an egocentric frame by the declared orientation (#49)
#
# `align = "orientation"` turns each subject, at each moment, by the inverse
# of its `to` member's orientation, so it faces +x whether or not it has
# keypoints to align on.

# 2D ----

# A centroid and a head, with the head one unit ahead of the centroid along
# the declared heading. The heading differs between the two moments.
heading_frame <- function(yaw = c(pi / 4, 3 * pi / 2), unit_angle = NULL) {
  radians <- if (identical(unit_angle, "deg")) yaw * pi / 180 else yaw
  centre <- cbind(c(3, -2), c(1, 4))
  head <- centre + cbind(cos(radians), sin(radians))

  af <- anicore::as_anipoint(data.frame(
    time = rep(1:2, times = 2),
    keypoint = rep(c("centroid", "head"), each = 2),
    x = c(centre[, 1], head[, 1]),
    y = c(centre[, 2], head[, 2]),
    hd = rep(yaw, times = 2)
  )) |>
    anicore::set_variables(
      where = list(
        position = c(x = "x", y = "y"),
        orientation = c(yaw = "hd")
      )
    )
  if (!is.null(unit_angle)) {
    af <- anicore::set_metadata(af, unit_angle = unit_angle)
  }
  af
}

rows_of <- function(frame, member) {
  d <- as.data.frame(frame)
  d <- d[d$keypoint == member, ]
  d[order(d$time), ]
}

test_that("each subject faces +x, and its yaw becomes 0", {
  ego <- transform_to_egocentric(
    heading_frame(),
    to = "centroid",
    align = "orientation"
  )

  centroid <- rows_of(ego, "centroid")
  head <- rows_of(ego, "head")

  expect_equal(centroid$x, c(0, 0))
  expect_equal(centroid$y, c(0, 0))
  expect_equal(head$x, c(1, 1), tolerance = 1e-12)
  expect_equal(head$y, c(0, 0), tolerance = 1e-12)
  # Composed exactly, so 0 rather than a rounding error either side of it
  expect_identical(as.data.frame(ego)$hd, rep(0, 4))
  expect_identical(
    as.character(anicore::get_metadata(ego, "reference_frame")),
    "egocentric"
  )
})

test_that("degrees are read and written in degrees", {
  ego <- transform_to_egocentric(
    heading_frame(yaw = c(45, 270), unit_angle = "deg"),
    to = "centroid",
    align = "orientation"
  )

  head <- rows_of(ego, "head")
  expect_equal(head$x, c(1, 1), tolerance = 1e-12)
  expect_equal(head$y, c(0, 0), tolerance = 1e-12)
  expect_identical(as.data.frame(ego)$hd, rep(0, 4))
})

test_that("facing across turns a quarter further, the frame's way round", {
  across <- transform_to_egocentric(
    heading_frame(),
    to = "centroid",
    align = "orientation",
    align_perpendicular = TRUE
  )
  head <- rows_of(across, "head")
  expect_equal(head$x, c(0, 0), tolerance = 1e-12)
  expect_equal(head$y, c(1, 1), tolerance = 1e-12)
  expect_equal(as.data.frame(across)$hd, rep(pi / 2, 4))

  # A frame whose angles run clockwise turns the other way
  clockwise <- anicore::set_axis_directions(
    heading_frame(),
    c(x = "right", y = "down")
  )
  across <- transform_to_egocentric(
    clockwise,
    to = "centroid",
    align = "orientation",
    align_perpendicular = TRUE
  )
  expect_equal(rows_of(across, "head")$y, c(-1, -1), tolerance = 1e-12)
  expect_equal(as.data.frame(across)$hd, rep(3 * pi / 2, 4))
})

test_that("another member's orientation ends up relative to the to member's", {
  af <- heading_frame()
  af$hd[af$keypoint == "head"] <- c(pi / 2, 0)

  ego <- transform_to_egocentric(af, to = "centroid", align = "orientation")

  # The head's own heading, less the centroid's, in [0, 2pi) like the input
  expect_equal(rows_of(ego, "head")$hd, c(pi / 4, pi / 2))
})

test_that("a subject can be another individual, each moment in its frame", {
  # A focal animal and a neighbour: where the neighbour is, and which way it
  # faces, as the focal animal sees it
  af <- anicore::as_anipoint(
    data.frame(
      time = rep(1:2, each = 2),
      individual = rep(c("focal", "other"), 2),
      x = c(0, 0, 1, 1),
      y = c(0, 2, 1, 4),
      hd = c(pi / 2, pi, 0, -pi / 2)
    ),
    variables_what = "individual"
  ) |>
    anicore::set_variables(where = list(orientation = c(yaw = "hd")))

  ego <- transform_to_egocentric(af, to = "focal", align = "orientation")
  other <- as.data.frame(ego)
  other <- other[other$individual == "other", ]

  # Ahead at the first moment, to the left at the second
  expect_equal(other$x, c(2, 0), tolerance = 1e-12)
  expect_equal(other$y, c(0, 3), tolerance = 1e-12)
  # Signed input stays signed
  expect_equal(other$hd, c(pi / 2, -pi / 2))
})

test_that("a moment without the to member's orientation comes back NA", {
  af <- heading_frame()
  af$hd[af$keypoint == "centroid" & af$time == 2] <- NA

  ego <- transform_to_egocentric(af, to = "centroid", align = "orientation")
  d <- as.data.frame(ego)

  later <- d[d$time == 2, ]
  expect_true(all(is.na(later[c("x", "y", "hd")])))
  expect_equal(rows_of(ego, "head")$x[1], 1, tolerance = 1e-12)
  expect_identical(d$hd[d$time == 1], c(0, 0))
})

test_that("a moment the to member is absent from comes back NA", {
  af <- anicore::as_anipoint(
    data.frame(
      time = c(1, 1, 2),
      keypoint = c("centroid", "head", "head"),
      x = c(0, 0, 5),
      y = c(0, 1, 5),
      hd = c(pi / 2, pi / 2, 0)
    ),
    variables_what = "keypoint"
  ) |>
    anicore::set_variables(where = list(orientation = c(yaw = "hd")))

  d <- as.data.frame(transform_to_egocentric(
    af,
    to = "centroid",
    align = "orientation"
  ))

  expect_true(all(is.na(d[d$time == 2, c("x", "y", "hd")])))
  expect_equal(d$x[d$time == 1 & d$keypoint == "head"], 1, tolerance = 1e-12)
})


# 3D ----

# A rigid body facing along its own +x, with its left along its own +y,
# placed from its orientation so the two agree by construction.
rigid_body <- function() {
  q <- rbind(
    quat_from_axis_angle(c(1, 2, 3), 1.1),
    quat_from_axis_angle(c(-1, 0.5, 2), 2.5)
  )
  centre <- rbind(c(1, 2, 3), c(4, -1, 0))
  parts <- list(tail = c(0, 0, 0), head = c(2, 0, 0), left = c(0, 1, 0))

  rows <- lapply(names(parts), \(part) {
    at <- centre + quat_rotate(q, parts[[part]])
    data.frame(
      time = 1:2,
      keypoint = part,
      x = at[, 1],
      y = at[, 2],
      z = at[, 3],
      qw = q[, 1],
      qx = q[, 2],
      qy = q[, 3],
      qz = q[, 4]
    )
  })

  anicore::as_anipoint(do.call(rbind, rows), variables_what = "keypoint") |>
    anicore::set_variables(
      where = list(
        position = c(x = "x", y = "y", z = "z"),
        orientation = c(qw = "qw", qx = "qx", qy = "qy", qz = "qz")
      )
    )
}

body_position <- function(frame, part) {
  d <- rows_of(frame, part)
  unname(as.matrix(d[c("x", "y", "z")]))
}

body_quat <- function(frame) {
  as.matrix(as.data.frame(frame)[c("qw", "qx", "qy", "qz")])
}

test_that("in 3D the body frame lands on the axes, and the orientation on the identity", {
  ego <- transform_to_egocentric(
    rigid_body(),
    to = "tail",
    align = "orientation"
  )

  expect_equal(body_position(ego, "tail"), matrix(0, 2, 3), tolerance = 1e-12)
  expect_equal(body_position(ego, "head"), cbind(c(2, 2), 0, 0))
  expect_equal(body_position(ego, "left"), cbind(0, c(1, 1), 0))
  expect_equal(
    unname(body_quat(ego)),
    matrix(c(1, 0, 0, 0), 6, 4, byrow = TRUE)
  )
})

test_that("in 3D, facing across puts forward on +y and left on +z", {
  ego <- transform_to_egocentric(
    rigid_body(),
    to = "tail",
    align = "orientation",
    align_perpendicular = TRUE
  )

  expect_equal(body_position(ego, "head"), cbind(0, c(2, 2), 0))
  expect_equal(body_position(ego, "left"), cbind(0, 0, c(1, 1)))
  # And the orientation still says so
  expect_equal(
    unname(quat_rotate(body_quat(ego), c(1, 0, 0))),
    matrix(c(0, 1, 0), 6, 3, byrow = TRUE)
  )
})

test_that("orientation agrees with aligning on the keypoints that it placed", {
  by_orientation <- transform_to_egocentric(
    rigid_body(),
    to = "tail",
    align = "orientation"
  )
  by_keypoints <- transform_to_egocentric(
    rigid_body(),
    to = "tail",
    align = c("tail", "head", "left")
  )

  for (part in c("tail", "head", "left")) {
    expect_equal(
      body_position(by_orientation, part),
      body_position(by_keypoints, part)
    )
  }
  expect_equal(
    quat_distance(body_quat(by_orientation), body_quat(by_keypoints)),
    rep(0, 6),
    tolerance = 1e-7
  )
})

test_that("in 3D a missing quaternion makes its moment NA", {
  af <- rigid_body()
  missing <- af$keypoint == "tail" & af$time == 1
  af$qw[missing] <- NA
  af$qx[missing] <- NA
  af$qy[missing] <- NA
  af$qz[missing] <- NA

  ego <- transform_to_egocentric(af, to = "tail", align = "orientation")
  d <- as.data.frame(ego)

  expect_true(all(is.na(d[d$time == 1, c("x", "y", "z", "qw", "qx")])))
  expect_equal(body_position(ego, "head")[2, ], c(2, 0, 0))
})


# Choosing between the two ----

test_that("a member called orientation is still an alignment point", {
  af <- anicore::as_anipoint(
    data.frame(
      time = 1,
      keypoint = c("orientation", "head"),
      x = c(0, 0),
      y = c(0, 1),
      hd = c(0, 0)
    ),
    variables_what = "keypoint"
  ) |>
    anicore::set_variables(where = list(orientation = c(yaw = "hd")))

  by_points <- as.data.frame(transform_to_egocentric(
    af,
    to = "orientation",
    align = c("orientation", "head")
  ))
  by_orientation <- as.data.frame(transform_to_egocentric(
    af,
    to = "orientation",
    align = "orientation"
  ))

  # The points put the head on +x; the declared yaw of 0 leaves it on +y
  expect_equal(by_points$x[by_points$keypoint == "head"], 1, tolerance = 1e-12)
  expect_equal(by_orientation$y[by_orientation$keypoint == "head"], 1)
})

test_that("aligning on keypoints is unchanged", {
  af <- heading_frame()
  expected <- rotate_coords(
    translate_coords(af, to = "centroid"),
    align = c("centroid", "head")
  )

  ego <- transform_to_egocentric(
    af,
    to = "centroid",
    align = c("centroid", "head")
  )

  expect_equal(
    as.data.frame(ego)[c("x", "y", "hd")],
    as.data.frame(expected)[c("x", "y", "hd")]
  )
})

test_that("aligning by orientation needs one to align by", {
  plain <- anicore::as_anipoint(
    data.frame(time = 1, keypoint = c("a", "b"), x = 0:1, y = 0),
    variables_what = "keypoint"
  )
  expect_error(
    transform_to_egocentric(plain, to = "a", align = "orientation"),
    "needs a declared orientation"
  )

  af <- heading_frame()
  af$hd[af$keypoint == "head"] <- NA
  expect_error(
    transform_to_egocentric(af, to = "head", align = "orientation"),
    "no orientation at any moment"
  )
})

test_that("one alignment point that is not orientation is refused", {
  expect_error(
    transform_to_egocentric(heading_frame(), to = "centroid", align = "head"),
    "or name two or three values"
  )
})
