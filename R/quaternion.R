# Quaternions are unit, Hamilton, scalar first: one row per rotation, columns
# w, x, y, z (animovement/anicore#154). They express a body's axes in the
# frame's coordinate system.

#' Quaternion algebra
#'
#' @description
#' Row-wise operations on unit quaternions. A quaternion argument is a numeric
#' vector of length 4, or a matrix or data frame with four columns, in the
#' order `w`, `x`, `y`, `z` (Hamilton convention, scalar first). Columns named
#' `w`/`x`/`y`/`z` or `qw`/`qx`/`qy`/`qz` are matched by name. A single row is
#' recycled against the other argument.
#'
#' * `quat_multiply(p, q)`: the Hamilton product `p q`, the rotation `q`
#'   followed by `p`.
#' * `quat_conjugate(q)`: the inverse of a unit quaternion.
#' * `quat_normalise(q)`: scaled to unit norm; a zero quaternion gives `NA`.
#' * `quat_rotate(q, v)`: rotates 3D vectors `v` (length 3, or 3 columns).
#' * `quat_distance(p, q)`: the angle of the rotation between `p` and `q`, in
#'   `[0, pi]`. `q` and `-q` are the same rotation, so their distance is 0.
#'
#' @param p,q Quaternions.
#' @param v 3D vectors.
#'
#' @return A matrix with columns `w`, `x`, `y`, `z` (or `x`, `y`, `z` for
#'   `quat_rotate()`), or a numeric vector for `quat_distance()`.
#'
#' @examples
#' quarter_z <- quat_from_axis_angle(c(0, 0, 1), pi / 2)
#' quat_rotate(quarter_z, c(1, 0, 0))
#' quat_multiply(quarter_z, quarter_z)
#' quat_distance(quarter_z, quat_conjugate(quarter_z))
#'
#' @seealso [quat_from_euler()], [quat_slerp()]
#' @name quaternions
NULL


#' @rdname quaternions
#' @export
quat_multiply <- function(p, q) {
  p <- as_quat(p, "p")
  q <- as_quat(q, "q")
  n <- common_rows(p, q)
  p <- recycle_quat(p, n)
  q <- recycle_quat(q, n)
  quat_matrix(
    p[, 1] * q[, 1] - p[, 2] * q[, 2] - p[, 3] * q[, 3] - p[, 4] * q[, 4],
    p[, 1] * q[, 2] + p[, 2] * q[, 1] + p[, 3] * q[, 4] - p[, 4] * q[, 3],
    p[, 1] * q[, 3] - p[, 2] * q[, 4] + p[, 3] * q[, 1] + p[, 4] * q[, 2],
    p[, 1] * q[, 4] + p[, 2] * q[, 3] - p[, 3] * q[, 2] + p[, 4] * q[, 1]
  )
}


#' @rdname quaternions
#' @export
quat_conjugate <- function(q) {
  q <- as_quat(q)
  q[, 2:4] <- -q[, 2:4]
  q
}


#' @rdname quaternions
#' @export
quat_normalise <- function(q) {
  q <- as_quat(q)
  norm <- sqrt(rowSums(q^2))
  norm[!is.na(norm) & norm == 0] <- NA_real_
  q / norm
}


#' @rdname quaternions
#' @export
quat_rotate <- function(q, v) {
  q <- as_quat(q)
  v <- as_rows(v, 3L, "v")
  n <- common_rows(q, v)
  q <- recycle_quat(q, n)
  v <- recycle_quat(v, n)
  u <- q[, 2:4, drop = FALSE]
  t <- 2 * cross_rows3(u, v)
  out <- v + q[, 1] * t + cross_rows3(u, t)
  colnames(out) <- c("x", "y", "z")
  out
}


#' @rdname quaternions
#' @export
quat_distance <- function(p, q) {
  # atan2 of the relative rotation stays accurate near 0, where acos does not.
  r <- quat_multiply(quat_conjugate(quat_normalise(p)), quat_normalise(q))
  2 * atan2(sqrt(rowSums(r[, 2:4, drop = FALSE]^2)), abs(r[, 1]))
}


#' Quaternions from and to other rotation representations
#'
#' @description
#' * `quat_from_axis_angle()` / `quat_to_axis_angle()`: a rotation of `angle`
#'   radians about `axis`, counter-clockwise by the right-hand rule.
#' * `quat_from_matrix()` / `quat_to_matrix()`: 3x3 rotation matrices, or a
#'   3x3xn array with one matrix per row.
#'
#' @param axis Axis vectors (length 3, or 3 columns); need not be unit length.
#' @param angle Angles in radians.
#' @param q Quaternions; see [quaternions].
#' @param rotation A 3x3 rotation matrix, or a 3x3xn array.
#'
#' @return `quat_from_*()`: a quaternion matrix. `quat_to_axis_angle()`: a
#'   list with `axis` (3 columns) and `angle` in `[0, pi]`; the axis of a zero
#'   rotation is `NA`. `quat_to_matrix()`: a 3x3xn array.
#'
#' @examples
#' q <- quat_from_axis_angle(c(0, 0, 2), pi / 2)
#' quat_to_axis_angle(q)
#' quat_to_matrix(q)[, , 1]
#' quat_from_matrix(quat_to_matrix(q))
#' @export
quat_from_axis_angle <- function(axis, angle) {
  axis <- as_rows(axis, 3L, "axis")
  n <- max(nrow(axis), length(angle))
  axis <- recycle_quat(axis, n)
  angle <- rep_len(angle, n)
  norm <- sqrt(rowSums(axis^2))
  norm[!is.na(norm) & norm == 0] <- NA_real_
  s <- sin(angle / 2) / norm
  w <- ifelse(is.na(norm), NA_real_, cos(angle / 2))
  quat_matrix(w, axis[, 1] * s, axis[, 2] * s, axis[, 3] * s)
}


#' @rdname quat_from_axis_angle
#' @export
quat_to_axis_angle <- function(q) {
  q <- quat_normalise(q)
  # The same rotation, with w >= 0, so the angle falls in [0, pi].
  q[!is.na(q[, 1]) & q[, 1] < 0, ] <- -q[!is.na(q[, 1]) & q[, 1] < 0, ]
  s <- sqrt(rowSums(q[, 2:4, drop = FALSE]^2))
  axis <- q[, 2:4, drop = FALSE] / ifelse(s > 1e-12, s, NA_real_)
  colnames(axis) <- c("x", "y", "z")
  list(axis = axis, angle = 2 * atan2(s, q[, 1]))
}


#' @rdname quat_from_axis_angle
#' @export
quat_from_matrix <- function(rotation) {
  if (!is.array(rotation) || !identical(dim(rotation)[1:2], c(3L, 3L))) {
    cli::cli_abort("{.arg rotation} must be a 3x3 matrix or a 3x3xn array.")
  }
  if (is.matrix(rotation)) {
    rotation <- array(rotation, dim = c(3, 3, 1))
  }
  out <- t(apply(rotation, 3, matrix_to_quat))
  quat_matrix(out[, 1], out[, 2], out[, 3], out[, 4])
}


#' @rdname quat_from_axis_angle
#' @export
quat_to_matrix <- function(q) {
  q <- quat_normalise(q)
  out <- array(NA_real_, dim = c(3, 3, nrow(q)))
  w <- q[, 1]
  x <- q[, 2]
  y <- q[, 3]
  z <- q[, 4]
  out[1, 1, ] <- 1 - 2 * (y^2 + z^2)
  out[1, 2, ] <- 2 * (x * y - w * z)
  out[1, 3, ] <- 2 * (x * z + w * y)
  out[2, 1, ] <- 2 * (x * y + w * z)
  out[2, 2, ] <- 1 - 2 * (x^2 + z^2)
  out[2, 3, ] <- 2 * (y * z - w * x)
  out[3, 1, ] <- 2 * (x * z - w * y)
  out[3, 2, ] <- 2 * (y * z + w * x)
  out[3, 3, ] <- 1 - 2 * (x^2 + y^2)
  out
}


# Shepperd's method: pivot on the largest of w, x, y, z for stability.
matrix_to_quat <- function(r) {
  trace <- r[1, 1] + r[2, 2] + r[3, 3]
  pivot <- which.max(c(trace, r[1, 1], r[2, 2], r[3, 3]))
  q <- switch(
    pivot,
    c(1 + trace, r[3, 2] - r[2, 3], r[1, 3] - r[3, 1], r[2, 1] - r[1, 2]),
    c(
      r[3, 2] - r[2, 3],
      1 + 2 * r[1, 1] - trace,
      r[1, 2] + r[2, 1],
      r[1, 3] + r[3, 1]
    ),
    c(
      r[1, 3] - r[3, 1],
      r[1, 2] + r[2, 1],
      1 + 2 * r[2, 2] - trace,
      r[2, 3] + r[3, 2]
    ),
    c(
      r[2, 1] - r[1, 2],
      r[1, 3] + r[3, 1],
      r[2, 3] + r[3, 2],
      1 + 2 * r[3, 3] - trace
    )
  )
  q <- q / sqrt(sum(q^2))
  if (q[1] < 0) -q else q
}


#' @keywords internal
quat_matrix <- function(w, x, y, z) {
  out <- cbind(w = w, x = x, y = y, z = z)
  rownames(out) <- NULL
  out
}


#' Coerce a quaternion argument to a 4-column matrix
#'
#' @keywords internal
as_quat <- function(q, arg = "q") {
  if (is.data.frame(q)) {
    nm <- names(q)
    for (set in list(c("w", "x", "y", "z"), c("qw", "qx", "qy", "qz"))) {
      if (all(set %in% nm)) {
        q <- q[, set]
        break
      }
    }
    q <- as.matrix(q)
  }
  q <- as_rows(q, 4L, arg)
  colnames(q) <- c("w", "x", "y", "z")
  q
}


#' @keywords internal
as_rows <- function(x, width, arg) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (is.numeric(x) && is.null(dim(x))) {
    x <- matrix(x, nrow = 1L)
  }
  if (!is.matrix(x) || !is.numeric(x) || ncol(x) != width) {
    cli::cli_abort(
      "{.arg {arg}} must be a numeric vector of length {width}, or have {width} columns."
    )
  }
  x
}


#' @keywords internal
common_rows <- function(a, b) {
  n <- max(nrow(a), nrow(b))
  if (!all(c(nrow(a), nrow(b)) %in% c(1L, n))) {
    cli::cli_abort("Arguments must have one row or {n}.")
  }
  n
}


#' @keywords internal
recycle_quat <- function(x, n) {
  if (nrow(x) == n) x else x[rep(1L, n), , drop = FALSE]
}


#' @keywords internal
cross_rows3 <- function(a, b) {
  cbind(
    a[, 2] * b[, 3] - a[, 3] * b[, 2],
    a[, 3] * b[, 1] - a[, 1] * b[, 3],
    a[, 1] * b[, 2] - a[, 2] * b[, 1]
  )
}
