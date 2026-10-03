# A declared orientation turns with the coordinates (#49)
#
# Rotating the positions and leaving `yaw` or the quaternion as it was makes
# the two disagree about which way the body faces. The orientation is turned
# by the same rotation, per subject and moment.

# 2D ----

# The issue's example: facing along the tail -> head axis, at pi / 4.
heading_2d <- function(hd = pi / 4, unit_angle = NULL) {
  af <- anicore::as_anipoint(data.frame(
    time = rep(1:2, each = 2),
    keypoint = rep(c("head", "tail"), 2),
    x = c(1, 0, 1, 0),
    y = c(1, 0, 2, 1),
    hd = hd
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

# In the order the rows were written: by time, head before tail
yaw_of <- function(frame) {
  d <- as.data.frame(frame)
  d$hd[order(d$time, d$keypoint)]
}

# How far apart two angles are around the circle, so that a turn landing a
# rounding error either side of 0 compares equal to 0
circular_gap <- function(a, b, period = 2 * pi) {
  gap <- (a - b) %% period
  pmin(gap, period - gap)
}

test_that("aligning on the facing axis turns yaw to 0", {
  ego <- transform_to_egocentric(
    heading_2d(),
    to = "tail",
    align = c("tail", "head"),
    level = "keypoint"
  )
  d <- as.data.frame(ego)

  # The head now lies on +x, and the orientation says so too
  expect_equal(d$y[d$keypoint == "head"], c(0, 0), tolerance = 1e-12)
  expect_equal(circular_gap(d$hd, 0), rep(0, 4), tolerance = 1e-12)
  expect_identical(
    anicore::get_variables(ego, "where", "orientation"),
    c(yaw = "hd")
  )
})

test_that("yaw turns by the rotation's own angle", {
  # Each moment's tail -> head axis is at pi / 4, so each turns by -pi / 4
  out <- rotate_coords(
    heading_2d(hd = c(pi / 2, pi / 2, 1, 1)),
    align = c("tail", "head"),
    level = "keypoint"
  )

  expect_equal(yaw_of(out), c(pi / 4, pi / 4, 1 - pi / 4, 1 - pi / 4))
})

test_that("yaw in degrees is turned in degrees", {
  out <- rotate_coords(
    heading_2d(hd = c(90, 90, 45, 45), unit_angle = "deg"),
    align = c("tail", "head"),
    level = "keypoint"
  )

  expect_equal(
    circular_gap(yaw_of(out), c(45, 45, 0, 0), period = 360),
    rep(0, 4),
    tolerance = 1e-9
  )
  expect_equal(as.character(anicore::get_metadata(out, "unit_angle")), "deg")
})

test_that("a quarter turn across the axis turns yaw by that much more", {
  along <- rotate_coords(
    heading_2d(hd = pi / 2),
    align = c("tail", "head"),
    level = "keypoint"
  )
  across <- rotate_coords(
    heading_2d(hd = pi / 2),
    align = c("tail", "head"),
    level = "keypoint",
    align_perpendicular = TRUE
  )

  expect_equal(yaw_of(across) - yaw_of(along), rep(pi / 2, 4))
})

test_that("unsigned yaw stays in [0, 2pi), signed yaw in (-pi, pi]", {
  # Turning by -pi / 4 takes 0.5 below zero
  unsigned <- rotate_coords(
    heading_2d(hd = c(0.5, 0.5, 3, 3)),
    align = c("tail", "head"),
    level = "keypoint"
  )
  signed <- rotate_coords(
    heading_2d(hd = c(-3, -3, 0.5, 0.5)),
    align = c("tail", "head"),
    level = "keypoint"
  )

  # The input's rows are by time: both of a moment's rows share a yaw
  expect_equal(
    yaw_of(unsigned),
    c(0.5 - pi / 4 + 2 * pi, 0.5 - pi / 4 + 2 * pi, 3 - pi / 4, 3 - pi / 4)
  )
  expect_equal(
    yaw_of(signed),
    c(-3 - pi / 4 + 2 * pi, -3 - pi / 4 + 2 * pi, 0.5 - pi / 4, 0.5 - pi / 4)
  )
  expect_true(all(yaw_of(unsigned) >= 0 & yaw_of(unsigned) < 2 * pi))
  expect_true(all(yaw_of(signed) > -pi & yaw_of(signed) <= pi))
})

test_that("a missing yaw stays missing", {
  out <- rotate_coords(
    heading_2d(hd = c(NA, NA, pi / 4, pi / 4)),
    align = c("tail", "head"),
    level = "keypoint"
  )

  expect_true(all(is.na(yaw_of(out)[1:2])))
  expect_equal(circular_gap(yaw_of(out)[3:4], 0), c(0, 0), tolerance = 1e-12)
})

test_that("a moment without a rotation keeps its yaw, as it keeps its positions", {
  af <- heading_2d()
  af$x[af$keypoint == "head" & af$time == 2] <- NA

  out <- as.data.frame(rotate_coords(
    af,
    align = c("tail", "head"),
    level = "keypoint"
  ))

  later <- out[out$time == 2, ]
  expect_equal(later$hd, c(pi / 4, pi / 4))
  expect_equal(later$y, c(2, 1))
  expect_equal(
    circular_gap(out$hd[out$time == 1], 0),
    c(0, 0),
    tolerance = 1e-12
  )
})

test_that("the centre of rotation moves positions but not orientation", {
  about_origin <- rotate_coords(
    heading_2d(),
    align = c("tail", "head"),
    level = "keypoint"
  )
  about_head <- rotate_coords(
    heading_2d(),
    align = c("tail", "head"),
    level = "keypoint",
    about = "head"
  )
  about_point <- rotate_coords(
    heading_2d(),
    align = c("tail", "head"),
    level = "keypoint",
    about = c(x = 10, y = -5)
  )

  expect_equal(yaw_of(about_head), yaw_of(about_origin))
  expect_equal(yaw_of(about_point), yaw_of(about_origin))
})

test_that("positions do not depend on whether an orientation is declared", {
  with <- heading_2d()
  without <- anicore::as_anipoint(
    as.data.frame(with)[c("time", "keypoint", "x", "y")],
    variables_what = "keypoint"
  )

  rotated_with <- rotate_coords(with, align = c("tail", "head"))
  rotated_without <- rotate_coords(without, align = c("tail", "head"))

  expect_equal(
    as.data.frame(rotated_with)[c("x", "y")],
    as.data.frame(rotated_without)[c("x", "y")]
  )
  expect_length(
    anicore::get_variables(rotated_without, "where", "orientation"),
    0
  )
})

test_that("translating leaves orientation alone", {
  af <- heading_2d(hd = c(0.1, 0.2, 0.3, 0.4))

  onto <- translate_coords(af, to = "tail", level = "keypoint")
  by <- translate_coords(af, by = c(x = 3, y = -2))
  ego <- transform_to_egocentric(af, to = "tail", level = "keypoint")

  expect_equal(yaw_of(onto), yaw_of(af))
  expect_equal(yaw_of(by), yaw_of(af))
  expect_equal(yaw_of(ego), yaw_of(af))
  expect_identical(
    anicore::get_variables(onto, "where", "orientation"),
    c(yaw = "hd")
  )
})


# Wrapping ----

test_that("wrapping never lands on the end of the range it excludes", {
  # Both of these round onto the excluded end in anicore::wrap_angle()
  expect_identical(wrap_like(-1e-17, signed = FALSE), 0)
  expect_identical(wrap_like(pi + 4.5e-16, signed = TRUE), pi)

  expect_equal(wrap_like(c(-pi / 2, NA, 3 * pi), FALSE), c(3 * pi / 2, NA, pi))
  expect_equal(wrap_like(c(3 * pi / 2, NA, -pi), TRUE), c(-pi / 2, NA, pi))
})


# 3D ----

# A rigid body facing along its own +x, with its left along its own +y. The
# keypoints are placed from the orientation, so the two agree by
# construction.
rigid_body_3d <- function() {
  q <- rbind(
    quat_from_axis_angle(c(1, 2, 3), 1.1),
    quat_from_axis_angle(c(-1, 0.5, 2), 2.5)
  )
  centre <- rbind(c(1, 2, 3), c(4, -1, 0))
  parts <- list(
    tail = c(0, 0, 0),
    head = c(2, 0, 0),
    left = c(0, 1, 0)
  )

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

quat_of <- function(frame) {
  d <- as.data.frame(frame)
  as.matrix(d[c("qw", "qx", "qy", "qz")])
}

position_of <- function(frame, part) {
  d <- as.data.frame(frame)
  as.matrix(d[d$keypoint == part, c("x", "y", "z")])
}

test_that("the turned quaternion still points the body along its keypoints", {
  out <- rotate_coords(
    rigid_body_3d(),
    align = c("tail", "head"),
    level = "keypoint"
  )
  d <- as.data.frame(out)
  head <- d$keypoint == "head"

  forward <- position_of(out, "head") - position_of(out, "tail")
  left <- position_of(out, "left") - position_of(out, "tail")
  q <- quat_of(out)[head, ]

  expect_equal(unname(quat_rotate(q, c(2, 0, 0))), unname(forward))
  expect_equal(unname(quat_rotate(q, c(0, 1, 0))), unname(left))
  # Two points put the forward axis onto +x
  expect_equal(unname(forward), cbind(c(2, 2), 0, 0))
  expect_equal(unname(rowSums(quat_of(out)^2)), rep(1, 6))
})

test_that("three points turn the orientation to the identity", {
  ego <- transform_to_egocentric(
    rigid_body_3d(),
    to = "tail",
    align = c("tail", "head", "left"),
    level = "keypoint"
  )

  expect_equal(
    quat_distance(quat_of(ego), c(1, 0, 0, 0)),
    rep(0, 6),
    tolerance = 1e-7
  )
})

test_that("in 3D too, the centre of rotation leaves orientation alone", {
  about_origin <- rotate_coords(
    rigid_body_3d(),
    align = c("tail", "head"),
    level = "keypoint"
  )
  about_point <- rotate_coords(
    rigid_body_3d(),
    align = c("tail", "head"),
    level = "keypoint",
    about = c(x = 5, y = 5, z = -5)
  )

  expect_equal(quat_of(about_point), quat_of(about_origin))
})

test_that("a missing quaternion stays missing, and a moment without a rotation keeps its own", {
  af <- rigid_body_3d()
  af$qw[af$time == 1] <- NA
  af$qx[af$time == 1] <- NA
  af$qy[af$time == 1] <- NA
  af$qz[af$time == 1] <- NA
  af$x[af$keypoint == "head" & af$time == 2] <- NA

  out <- rotate_coords(af, align = c("tail", "head"), level = "keypoint")
  d <- as.data.frame(out)

  expect_true(all(is.na(quat_of(out)[d$time == 1, ])))
  expect_equal(
    quat_of(out)[d$time == 2, ],
    quat_of(af)[as.data.frame(af)$time == 2, ]
  )
})


# Which moment a rotation belongs to ----

test_that("a final moment without a rotation is not given another's", {
  # `[[<-` with NULL dropped the last rotation, so the list was recycled:
  # with two moments the first's rotation turned the second's rows.
  af <- heading_2d()
  af$x[af$keypoint == "head" & af$time == 2] <- NA

  out <- as.data.frame(rotate_coords(
    af,
    align = c("tail", "head"),
    level = "keypoint"
  ))

  expect_equal(
    unlist(out[out$time == 2 & out$keypoint == "tail", c("x", "y")]),
    c(x = 0, y = 1)
  )
})

test_that("more than two moments, the last without a rotation, rotate", {
  af <- anicore::as_anipoint(
    data.frame(
      time = rep(1:3, each = 2),
      keypoint = rep(c("head", "tail"), 3),
      x = c(0, 0, 0, 0, 0, NA),
      y = c(1, 0, -1, 0, 1, NA)
    ),
    variables_what = "keypoint"
  )

  out <- as.data.frame(rotate_coords(af, align = c("tail", "head")))

  expect_equal(out$x[out$keypoint == "head"], c(1, 1, 0), tolerance = 1e-12)
  expect_equal(out$y[out$keypoint == "head"], c(0, 0, 1), tolerance = 1e-12)
})

test_that("a member absent from a moment leaves that moment, not its neighbours, unrotated", {
  # Pairing alignment points by row position failed here with
  # "non-conformable arrays" -- or, with equal counts, paired one moment's
  # head with another moment's tail.
  af <- anicore::as_anipoint(
    data.frame(
      time = c(1, 1, 2, 3, 3),
      keypoint = c("head", "tail", "head", "head", "tail"),
      x = c(0, 0, 5, -1, 0),
      y = c(1, 0, 5, 0, 0)
    ),
    variables_what = "keypoint"
  )

  out <- as.data.frame(rotate_coords(af, align = c("tail", "head")))
  head <- out[out$keypoint == "head", ]

  expect_equal(head$x, c(1, 5, 1), tolerance = 1e-12)
  expect_equal(head$y, c(0, 5, 0), tolerance = 1e-12)
})
