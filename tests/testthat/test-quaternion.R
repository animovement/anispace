quarter_z <- function() quat_from_axis_angle(c(0, 0, 1), pi / 2)

test_that("the Hamilton product composes rotations, right one first", {
  x_then_z <- quat_multiply(
    quarter_z(),
    quat_from_axis_angle(c(1, 0, 0), pi / 2)
  )
  expect_equal(unname(quat_rotate(x_then_z, c(0, 1, 0))[1, ]), c(0, 0, 1))
  expect_equal(
    unname(quat_multiply(quarter_z(), quarter_z())[1, ]),
    unname(quat_from_axis_angle(c(0, 0, 1), pi)[1, ])
  )
})

test_that("rows are paired and a single row recycled", {
  q <- quat_from_axis_angle(c(0, 0, 1), c(0, pi / 2))
  expect_equal(nrow(quat_multiply(q, quarter_z())), 2L)
  v <- quat_rotate(q, c(1, 0, 0))
  expect_equal(unname(v), rbind(c(1, 0, 0), c(0, 1, 0)))
  expect_error(quat_multiply(q, rbind(q, q)), "one row or 4")
})

test_that("conjugate inverts and normalise rescales", {
  q <- quat_from_axis_angle(c(1, 2, 3), 0.7)
  expect_equal(unname(quat_multiply(q, quat_conjugate(q))[1, ]), c(1, 0, 0, 0))
  expect_equal(sqrt(sum(quat_normalise(2 * q)^2)), 1)
  expect_true(all(is.na(quat_normalise(c(0, 0, 0, 0)))))
})

test_that("distance is the angle between rotations, blind to sign", {
  expect_equal(quat_distance(quarter_z(), quat_conjugate(quarter_z())), pi)
  expect_equal(quat_distance(quarter_z(), -quarter_z()), 0)
  q <- quat_from_axis_angle(c(0, 1, 0), 1e-9)
  expect_equal(quat_distance(c(1, 0, 0, 0), q), 1e-9, tolerance = 1e-6)
})

test_that("quaternion input can be a data frame, matched by column name", {
  df <- data.frame(qz = 0, qx = 0, qw = 1, qy = 0)
  expect_equal(unname(quat_normalise(df)[1, ]), c(1, 0, 0, 0))
  df2 <- data.frame(x = 0, y = 0, z = 0, w = 1)
  expect_equal(unname(quat_normalise(df2)[1, ]), c(1, 0, 0, 0))
  expect_error(quat_normalise(c(1, 0, 0)), "length 4")
})

test_that("axis-angle round trips, with the angle in [0, pi]", {
  q <- quat_from_axis_angle(c(0, 0, 2), -pi / 2)
  aa <- quat_to_axis_angle(q)
  expect_equal(aa$angle, pi / 2)
  expect_equal(unname(aa$axis[1, ]), c(0, 0, -1))
  expect_true(all(is.na(quat_to_axis_angle(c(1, 0, 0, 0))$axis)))
  expect_true(all(is.na(quat_from_axis_angle(c(0, 0, 0), 1))))
})

test_that("rotation matrices round trip through every pivot", {
  set.seed(1)
  axes <- matrix(stats::rnorm(60), ncol = 3)
  angles <- c(0, pi, stats::runif(18, -pi, pi))
  axes[2, ] <- c(1, 0, 0)
  q <- quat_from_axis_angle(axes, angles)
  back <- quat_from_matrix(quat_to_matrix(q))
  expect_lt(max(quat_distance(q, back)), 1e-9)
  for (axis in list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))) {
    half <- quat_from_axis_angle(axis, pi)
    expect_lt(
      quat_distance(half, quat_from_matrix(quat_to_matrix(half)[,, 1])),
      1e-9
    )
  }
  expect_error(quat_from_matrix(matrix(1, 2, 2)), "3x3")
})

test_that("Euler angles round trip for all twelve sequences, both conventions", {
  set.seed(2)
  sequences <- c(
    "XYZ",
    "XZY",
    "YXZ",
    "YZX",
    "ZXY",
    "ZYX",
    "XYX",
    "XZX",
    "YXY",
    "YZY",
    "ZXZ",
    "ZYZ"
  )
  for (s in sequences) {
    proper <- substr(s, 1, 1) == substr(s, 3, 3)
    middle <- if (proper) {
      stats::runif(20, 0.1, pi - 0.1)
    } else {
      stats::runif(20, -1.4, 1.4)
    }
    angles <- cbind(stats::runif(20, -3, 3), middle, stats::runif(20, -3, 3))
    for (intrinsic in c(TRUE, FALSE)) {
      back <- quat_to_euler(quat_from_euler(angles, s, intrinsic), s, intrinsic)
      expect_equal(unname(back), unname(angles), tolerance = 1e-9)
    }
  }
})

test_that("intrinsic ZYX is yaw-pitch-roll, and equals extrinsic XYZ reversed", {
  R <- quat_to_matrix(quat_from_euler(c(0.3, 0.2, 0.1), "ZYX", TRUE))[,, 1]
  rz <- function(a) matrix(c(cos(a), sin(a), 0, -sin(a), cos(a), 0, 0, 0, 1), 3)
  ry <- function(a) matrix(c(cos(a), 0, -sin(a), 0, 1, 0, sin(a), 0, cos(a)), 3)
  rx <- function(a) matrix(c(1, 0, 0, 0, cos(a), sin(a), 0, -sin(a), cos(a)), 3)
  expect_equal(R, rz(0.3) %*% ry(0.2) %*% rx(0.1))
  q <- quat_from_euler(c(0.3, 0.2, 0.1), "zyx", TRUE)
  expect_equal(
    unname(quat_to_euler(q, "XYZ", intrinsic = FALSE)[1, ]),
    c(0.1, 0.2, 0.3)
  )
  expect_equal(colnames(quat_to_euler(q, "ZYX", TRUE)), c("z_1", "y_2", "x_3"))
})

test_that("gimbal lock gives a valid decomposition with the third angle 0", {
  for (s in c("ZYX", "ZXZ")) {
    locked <- if (s == "ZYX") c(0.4, pi / 2, 0.3) else c(0.4, 0, 0.3)
    q <- quat_from_euler(locked, s, TRUE)
    back <- quat_to_euler(q, s, TRUE)
    expect_equal(back[1, 3], 0, ignore_attr = TRUE)
    expect_lt(quat_distance(q, quat_from_euler(back, s, TRUE)), 1e-6)
  }
  q <- quat_from_euler(c(0.4, pi, 0.3), "ZXZ", TRUE)
  back <- quat_to_euler(q, "ZXZ", TRUE)
  expect_lt(quat_distance(q, quat_from_euler(back, "ZXZ", TRUE)), 1e-6)
})

test_that("the Euler convention must be stated", {
  expect_error(quat_from_euler(c(0, 0, 0), "ZZY", TRUE), "sequence")
  expect_error(quat_from_euler(c(0, 0, 0), "ABC", TRUE), "sequence")
  expect_error(quat_from_euler(c(0, 0, 0), "ZYX", NA), "intrinsic")
  expect_error(quat_to_euler(c(1, 0, 0, 0), "ZYX", "yes"), "intrinsic")
})

test_that("slerp follows the shorter arc", {
  a <- c(1, 0, 0, 0)
  b <- quarter_z()
  expect_equal(quat_to_axis_angle(quat_slerp(a, b, 0.5))$angle, pi / 4)
  expect_equal(quat_distance(quat_slerp(a, -b, 0.5), quat_slerp(a, b, 0.5)), 0)
  steps <- quat_slerp(a, b, c(0, 1))
  expect_lt(max(quat_distance(steps, rbind(a, b))), 1e-12)
  expect_equal(unname(quat_slerp(a, a, 0.3)[1, ]), a)
  expect_error(quat_slerp(rbind(a, a), rbind(a, a, a), 0.5), "one row or 3")
})

test_that("the mean is unaffected by sign and honours weights", {
  a <- c(1, 0, 0, 0)
  b <- quarter_z()
  expect_equal(quat_to_axis_angle(quat_mean(rbind(a, -b)))$angle, pi / 4)
  heavy <- quat_mean(rbind(a, b), weights = c(0, 1))
  expect_lt(quat_distance(heavy, b), 1e-12)
  expect_equal(nrow(quat_mean(rbind(a, NA_real_))), 1L)
  expect_true(all(is.na(quat_mean(rbind(rep(NA_real_, 4))))))
  expect_error(quat_mean(rbind(a, b), weights = 1), "one per row")
  expect_equal(quat_mean(rbind(-a))[1, 1], 1, ignore_attr = TRUE)
})

test_that("continuity removes sign jumps without changing rotations", {
  q <- quat_from_axis_angle(c(0, 0, 1), c(0, 0.1, 0.2, 0.3))
  jumpy <- q
  jumpy[c(2, 4), ] <- -jumpy[c(2, 4), ]
  jumpy <- rbind(jumpy[1:2, ], NA, jumpy[3:4, ])
  fixed <- quat_continuous(jumpy)
  expect_equal(fixed[-3, ], q, ignore_attr = TRUE)
  expect_true(all(is.na(fixed[3, ])))
})

test_that("angular velocity is the rotation per unit time, per frame", {
  spin <- quat_from_axis_angle(c(0, 0, 1), seq(0, 1, by = 0.25))
  w <- quat_angular_velocity(spin, dt = 0.5)
  expect_true(all(is.na(w[1, ])))
  expect_equal(unname(w[-1, ]), matrix(c(0, 0, 0.5), 4, 3, byrow = TRUE))

  still <- quat_angular_velocity(rbind(c(1, 0, 0, 0), c(1, 0, 0, 0)), dt = 1)
  expect_equal(unname(still[2, ]), c(0, 0, 0))

  tilted <- quat_from_axis_angle(c(1, 0, 0), pi / 2)
  body <- rbind(
    tilted,
    quat_multiply(tilted, quat_from_axis_angle(c(0, 0, 1), 0.1))
  )
  expect_equal(
    unname(quat_angular_velocity(body, 1, frame = "body")[2, ]),
    c(0, 0, 0.1)
  )
  expect_equal(
    unname(quat_angular_velocity(body, 1, frame = "fixed")[2, ]),
    c(0, -0.1, 0)
  )
  expect_true(all(is.na(quat_angular_velocity(c(1, 0, 0, 0), 1))))
})

test_that("Euler columns become a declared quaternion orientation and back", {
  df <- data.frame(
    time = 1:3,
    x = 0,
    y = 0,
    z = 0,
    yaw = c(0, 0.1, 0.2),
    pitch = c(0, 0.05, 0),
    roll = c(0, 0, 0.1)
  )
  af <- anicore::as_anipoint(df) |>
    transform_euler_to_quaternion(c("yaw", "pitch", "roll"), "ZYX", TRUE)
  expect_equal(
    anicore::get_variables(af, "where", "orientation"),
    c(qw = "qw", qx = "qx", qy = "qy", qz = "qz")
  )
  back <- transform_quaternion_to_euler(
    af,
    "ZYX",
    TRUE,
    names = c("a", "b", "c")
  )
  expect_equal(back$a, df$yaw)
  expect_equal(back$b, df$pitch)
  expect_equal(back$c, df$roll)
})

test_that("the frame helpers follow unit_angle", {
  df <- data.frame(time = 1:2, x = 0, y = 0, z = 0, h = c(0, 90), p = 0, r = 0)
  af <- anicore::as_anipoint(df) |>
    anicore::set_metadata(unit_angle = "deg") |>
    transform_euler_to_quaternion(c("h", "p", "r"), "ZYX", TRUE)
  expect_equal(
    unname(
      quat_to_axis_angle(as.data.frame(af)[2, c("qw", "qx", "qy", "qz")])$angle
    ),
    pi / 2
  )
  expect_equal(transform_quaternion_to_euler(af, "ZYX", TRUE)$euler_1, c(0, 90))
})

test_that("the frame helpers refuse what they cannot do", {
  df <- data.frame(
    time = 1:2,
    x = 0,
    y = 0,
    z = 0,
    a = 0,
    b = 0,
    c = 0,
    lbl = "k"
  )
  af <- anicore::as_anipoint(df)
  expect_error(
    transform_euler_to_quaternion(af, c("a", "b"), "ZYX", TRUE),
    "three columns"
  )
  expect_error(
    transform_euler_to_quaternion(af, c("a", "b", "lbl"), "ZYX", TRUE),
    "numeric"
  )
  expect_error(
    transform_euler_to_quaternion(
      af,
      c("a", "b", "c"),
      "ZYX",
      TRUE,
      names = c("a", "q1", "q2", "q3")
    ),
    "already exist"
  )
  expect_error(
    transform_euler_to_quaternion(
      af,
      c("a", "b", "c"),
      "ZYX",
      TRUE,
      names = "q"
    ),
    "4 distinct"
  )
  expect_error(
    transform_quaternion_to_euler(af, "ZYX", TRUE),
    "no quaternion orientation"
  )
  flat <- anicore::as_anipoint(data.frame(
    time = 1:2,
    x = 0,
    y = 0,
    a = 0,
    b = 0,
    c = 0
  ))
  expect_error(
    transform_euler_to_quaternion(flat, c("a", "b", "c"), "ZYX", TRUE),
    "3D frame"
  )
})

test_that("extrinsic gimbal lock also sets the third angle to 0", {
  for (s in c("XYZ", "ZXZ")) {
    locked <- if (s == "XYZ") c(0.4, -pi / 2, 0.3) else c(0.4, pi, 0.3)
    q <- quat_from_euler(locked, s, FALSE)
    back <- quat_to_euler(q, s, FALSE)
    expect_equal(back[1, 3], 0, ignore_attr = TRUE)
    expect_lt(quat_distance(q, quat_from_euler(back, s, FALSE)), 1e-9)
  }
})

test_that("vectors can be given as a data frame", {
  v <- data.frame(x = c(1, 0), y = c(0, 1), z = 0)
  expect_equal(
    unname(quat_rotate(quarter_z(), v)),
    rbind(c(0, 1, 0), c(-1, 0, 0))
  )
})
