#' Quaternions from and to Euler angles
#'
#' @description
#' Three rotations about coordinate axes, applied in `sequence`. There are
#' twelve sequences: six Tait-Bryan (`"ZYX"`, `"XYZ"`, …, all three axes) and
#' six proper Euler (`"ZXZ"`, `"XYX"`, …, first and last axis the same).
#'
#' * **Intrinsic** rotations are about the body's own axes, which move with
#'   each rotation. Intrinsic `"ZYX"` is yaw, pitch, roll.
#' * **Extrinsic** rotations are about the fixed frame's axes. An intrinsic
#'   sequence equals the reversed extrinsic sequence with the angles reversed.
#'
#' Neither `sequence` nor `intrinsic` has a default: an assumed convention is
#' the classic source of Euler-angle bugs, so the call must say which is
#' meant. Euler angles are a report format; store orientation as quaternions.
#'
#' @param angles Angles in radians: a vector of length 3, or three columns,
#'   in `sequence` order.
#' @param q Quaternions; see [quaternions].
#' @param sequence Three axes, e.g. `"ZYX"`. Case is ignored.
#' @param intrinsic `TRUE` for rotations about the moving body axes, `FALSE`
#'   for rotations about the fixed axes.
#'
#' @return `quat_from_euler()`: a quaternion matrix. `quat_to_euler()`: a
#'   matrix with three columns in `sequence` order. The middle angle is in
#'   `[-pi/2, pi/2]` for Tait-Bryan sequences and `[0, pi]` for proper ones;
#'   the others are in `(-pi, pi]`. At gimbal lock the third angle is 0.
#'
#' @examples
#' ypr <- c(pi / 2, 0.1, -0.2)
#' q <- quat_from_euler(ypr, sequence = "ZYX", intrinsic = TRUE)
#' quat_to_euler(q, sequence = "ZYX", intrinsic = TRUE)
#'
#' # The same rotation, described extrinsically
#' quat_to_euler(q, sequence = "XYZ", intrinsic = FALSE)
#' @export
quat_from_euler <- function(angles, sequence, intrinsic) {
  axes <- parse_euler_sequence(sequence)
  ensure_intrinsic_flag(intrinsic)
  angles <- as_rows(angles, 3L, "angles")
  unit <- diag(3)
  parts <- lapply(1:3, function(i) {
    quat_from_axis_angle(unit[axes[[i]], ], angles[, i])
  })
  if (intrinsic) {
    quat_multiply(quat_multiply(parts[[1]], parts[[2]]), parts[[3]])
  } else {
    quat_multiply(quat_multiply(parts[[3]], parts[[2]]), parts[[1]])
  }
}


#' @rdname quat_from_euler
#' @export
quat_to_euler <- function(q, sequence, intrinsic) {
  axes <- parse_euler_sequence(sequence)
  ensure_intrinsic_flag(intrinsic)
  q <- quat_normalise(q)
  if (intrinsic) {
    # Reversed, the intrinsic third angle is the extrinsic first.
    out <- extrinsic_euler(q, rev(axes), zero_first = TRUE)
    out <- out[, 3:1, drop = FALSE]
  } else {
    out <- extrinsic_euler(q, axes)
  }
  dimnames(out) <- list(NULL, paste0(c("x", "y", "z")[axes], "_", 1:3))
  out
}


# Bernardes & Viollet (2022), "Quaternion to Euler angles conversion: a
# direct, general and computationally efficient method", PLoS ONE 17(11).
extrinsic_euler <- function(q, axes, zero_first = FALSE) {
  i <- axes[[1]]
  j <- axes[[2]]
  proper <- i == axes[[3]]
  k <- if (proper) 6L - i - j else axes[[3]]
  sign <- (i - j) * (j - k) * (k - i) / 2

  w <- q[, 1]
  qi <- q[, i + 1L]
  qj <- q[, j + 1L]
  qk <- q[, k + 1L] * sign
  if (proper) {
    a <- w
    b <- qi
    c <- qj
    d <- qk
  } else {
    a <- w - qj
    b <- qi + qk
    c <- qj + w
    d <- qk - qi
  }

  middle <- acos(pmax(
    -1,
    pmin(1, 2 * (a^2 + b^2) / (a^2 + b^2 + c^2 + d^2) - 1)
  ))
  plus <- atan2(b, a)
  minus <- atan2(d, c)
  first <- plus - minus
  third <- sign^(!proper) * (plus + minus)
  locked <- !is.na(middle) & (middle < 1e-7 | pi - middle < 1e-7)
  if (!proper) {
    middle <- middle - pi / 2
  }

  # At gimbal lock only first + third is determined: set one to 0 and solve
  # the other exactly from what the middle rotation leaves.
  if (any(locked)) {
    middle_q <- quat_from_axis_angle(diag(3)[j, ], middle[locked])
    q_locked <- q[locked, , drop = FALSE]
    if (zero_first) {
      rest <- quat_multiply(q_locked, quat_conjugate(middle_q))
      third[locked] <- 2 * atan2(rest[, axes[[3]] + 1L], rest[, 1])
      first[locked] <- 0
    } else {
      rest <- quat_multiply(quat_conjugate(middle_q), q_locked)
      first[locked] <- 2 * atan2(rest[, i + 1L], rest[, 1])
      third[locked] <- 0
    }
  }
  wrap <- function(x) atan2(sin(x), cos(x))
  cbind(wrap(first), middle, wrap(third))
}


#' @keywords internal
parse_euler_sequence <- function(sequence) {
  axes <- if (is.character(sequence) && length(sequence) == 1L) {
    match(strsplit(tolower(sequence), "")[[1]], c("x", "y", "z"))
  }
  if (
    length(axes) != 3L ||
      anyNA(axes) ||
      axes[[1]] == axes[[2]] ||
      axes[[2]] == axes[[3]]
  ) {
    cli::cli_abort(c(
      "{.arg sequence} must be three axes with no axis repeated consecutively.",
      "i" = "For example {.val ZYX} (yaw, pitch, roll) or {.val ZXZ}."
    ))
  }
  axes
}


#' @keywords internal
ensure_intrinsic_flag <- function(intrinsic) {
  if (!is.logical(intrinsic) || length(intrinsic) != 1L || is.na(intrinsic)) {
    cli::cli_abort(
      "{.arg intrinsic} must be {.code TRUE} (body axes) or {.code FALSE} (fixed axes)."
    )
  }
  invisible(TRUE)
}
