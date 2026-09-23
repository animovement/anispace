#' Interpolate, average and differentiate rotations
#'
#' @description
#' * `quat_slerp(p, q, t)`: spherical linear interpolation, `t = 0` giving `p`
#'   and `t = 1` giving `q`, along the shorter arc.
#' * `quat_mean(q, weights)`: the average rotation, as the eigenvector of the
#'   largest eigenvalue of `sum(w q q')` (Markley et al. 2007). Unlike
#'   averaging components, it is unaffected by the sign ambiguity.
#' * `quat_continuous(q)`: flips signs so each row lies on the same side as
#'   the previous one. `q` and `-q` are the same rotation, so this changes no
#'   rotation, but smoothing or differentiating a series without it goes
#'   wrong wherever the sign jumps.
#' * `quat_angular_velocity(q, dt)`: angular velocity between successive
#'   rows, in radians per unit of `dt`, about the fixed frame's axes
#'   (`frame = "fixed"`) or the body's own (`frame = "body"`). The first row
#'   is `NA`.
#'
#' Rows with `NA` are skipped by `quat_mean()` and carried through by the
#' others.
#'
#' @param p,q Quaternions; see [quaternions].
#' @param t Interpolation fraction(s), recycled against the rows.
#' @param weights Optional non-negative weights, one per row.
#' @param dt Time between rows: one value, or one per row.
#' @param frame `"fixed"` or `"body"`.
#'
#' @return A quaternion matrix; `quat_mean()` returns one row;
#'   `quat_angular_velocity()` returns a matrix with columns `x`, `y`, `z`.
#'
#' @examples
#' a <- quat_from_axis_angle(c(0, 0, 1), 0)
#' b <- quat_from_axis_angle(c(0, 0, 1), pi / 2)
#' quat_slerp(a, b, 0.5)
#' quat_mean(rbind(a, b))
#'
#' spin <- quat_from_axis_angle(c(0, 0, 1), seq(0, 1, by = 0.1))
#' quat_angular_velocity(spin, dt = 1 / 30)
#' @export
quat_slerp <- function(p, q, t) {
  p <- quat_normalise(p)
  q <- quat_normalise(q)
  n <- max(nrow(p), nrow(q), length(t))
  p <- recycle_to(p, n, "p")
  q <- recycle_to(q, n, "q")
  t <- rep_len(t, n)

  dot <- rowSums(p * q)
  flip <- !is.na(dot) & dot < 0
  q[flip, ] <- -q[flip, ]
  dot <- abs(dot)
  theta <- acos(pmin(1, dot))
  sin_theta <- sin(theta)
  near <- !is.na(sin_theta) & sin_theta < 1e-8
  a <- ifelse(near, 1 - t, sin((1 - t) * theta) / sin_theta)
  b <- ifelse(near, t, sin(t * theta) / sin_theta)
  quat_normalise(a * p + b * q)
}


#' @rdname quat_slerp
#' @export
quat_mean <- function(q, weights = NULL) {
  q <- quat_normalise(q)
  if (is.null(weights)) {
    weights <- rep(1, nrow(q))
  }
  if (length(weights) != nrow(q) || any(weights < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg weights} must be non-negative, one per row.")
  }
  keep <- stats::complete.cases(q) & !is.na(weights)
  if (!any(keep)) {
    return(quat_matrix(NA_real_, NA_real_, NA_real_, NA_real_))
  }
  q <- q[keep, , drop = FALSE]
  m <- crossprod(q * weights[keep], q)
  v <- eigen(m, symmetric = TRUE)$vectors[, 1]
  if (v[1] < 0) {
    v <- -v
  }
  quat_matrix(v[1], v[2], v[3], v[4])
}


#' @rdname quat_slerp
#' @export
quat_continuous <- function(q) {
  q <- as_quat(q)
  previous <- NULL
  for (i in seq_len(nrow(q))) {
    if (anyNA(q[i, ])) {
      next
    }
    if (!is.null(previous) && sum(previous * q[i, ]) < 0) {
      q[i, ] <- -q[i, ]
    }
    previous <- q[i, ]
  }
  q
}


#' @rdname quat_slerp
#' @export
quat_angular_velocity <- function(q, dt, frame = c("fixed", "body")) {
  frame <- rlang::arg_match(frame)
  q <- quat_normalise(q)
  n <- nrow(q)
  dt <- rep_len(dt, n)
  out <- matrix(NA_real_, n, 3, dimnames = list(NULL, c("x", "y", "z")))
  if (n < 2L) {
    return(out)
  }
  previous <- q[-n, , drop = FALSE]
  current <- q[-1, , drop = FALSE]
  step <- if (identical(frame, "fixed")) {
    quat_multiply(current, quat_conjugate(previous))
  } else {
    quat_multiply(quat_conjugate(previous), current)
  }
  rotation <- quat_to_axis_angle(step)
  rate <- rotation$angle / dt[-1]
  velocity <- rotation$axis * rate
  velocity[!is.na(rotation$angle) & rotation$angle < 1e-11, ] <- 0
  out[-1, ] <- velocity
  out
}


#' @keywords internal
recycle_to <- function(x, n, arg) {
  if (!nrow(x) %in% c(1L, n)) {
    cli::cli_abort("{.arg {arg}} must have one row or {n}.")
  }
  recycle_quat(x, n)
}
