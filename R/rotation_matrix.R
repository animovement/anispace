# Building rotation matrices (#4)
#
# One representation for both dimensionalities: a rotation is a matrix, and
# 2D is the case where the axis is fixed to z. Axis-angle is used rather than
# quaternions because a single rotation is all that is composed here -- for
# composing or interpolating several, #7 is where quaternions earn their
# place, and they can replace the inside of `rotation_from_axis_angle()`
# without changing anything above it.

#' A rotation matrix from an axis and an angle
#'
#' Rodrigues' formula. For a unit axis `k` and angle `theta`,
#' `R = I + sin(theta) K + (1 - cos(theta)) K^2`, where `K` is the
#' cross-product matrix of `k`.
#'
#' @param axis Numeric vector of length 3. Need not be a unit vector.
#' @param angle Rotation angle in radians, counter-clockwise about `axis`.
#'
#' @return A 3x3 rotation matrix.
#' @keywords internal
rotation_from_axis_angle <- function(axis, angle) {
  norm <- sqrt(sum(axis^2))
  if (!is.finite(norm) || norm == 0) {
    return(diag(3))
  }
  k <- axis / norm

  cross <- matrix(
    c(0, k[3], -k[2], -k[3], 0, k[1], k[2], -k[1], 0),
    nrow = 3
  )
  diag(3) + sin(angle) * cross + (1 - cos(angle)) * (cross %*% cross)
}


#' The rotation taking one vector onto another
#'
#' The minimal rotation: about the axis perpendicular to both, through the
#' angle between them. In 3D two points leave the roll about the target
#' undetermined, and this is the conventional choice of what to do with it --
#' nothing.
#'
#' @param from,to Numeric vectors of length 3.
#'
#' @return A 3x3 rotation matrix.
#' @keywords internal
rotation_onto <- function(from, to) {
  from_n <- sqrt(sum(from^2))
  to_n <- sqrt(sum(to^2))
  if (!is.finite(from_n) || from_n == 0 || to_n == 0) {
    return(diag(3))
  }
  a <- from / from_n
  b <- to / to_n

  axis <- cross3(a, b)
  if (sqrt(sum(axis^2)) < .Machine$double.eps^0.5) {
    # Parallel or antiparallel: either nothing to do, or a half turn about
    # any perpendicular axis.
    if (sum(a * b) > 0) {
      return(diag(3))
    }
    perp <- if (abs(a[1]) < 0.9) c(1, 0, 0) else c(0, 1, 0)
    return(rotation_from_axis_angle(cross3(a, perp), pi))
  }

  rotation_from_axis_angle(axis, atan2(sqrt(sum(axis^2)), sum(a * b)))
}


#' The rotation taking one frame of reference onto another
#'
#' Three points fix an orientation outright: the first vector gives the
#' primary axis, and the second, made perpendicular to it, gives the plane.
#' Both bases are orthonormal, so the rotation between them is `B %*% t(A)`.
#'
#' @param primary,secondary Numeric vectors of length 3 spanning the source.
#' @param to_primary,to_secondary The target axes they should land on.
#'
#' @return A 3x3 rotation matrix.
#' @keywords internal
rotation_onto_basis <- function(
  primary,
  secondary,
  to_primary,
  to_secondary
) {
  source <- orthonormal_basis(primary, secondary)
  target <- orthonormal_basis(to_primary, to_secondary)
  if (is.null(source) || is.null(target)) {
    return(diag(3))
  }
  target %*% t(source)
}


#' An orthonormal basis from two vectors
#'
#' Gram-Schmidt: the first vector normalised, the second with its projection
#' onto the first removed, and their cross product.
#'
#' @param primary,secondary Numeric vectors of length 3.
#'
#' @return A 3x3 matrix whose columns are the basis, or `NULL` when the two
#'   vectors are parallel and so span no plane.
#' @keywords internal
orthonormal_basis <- function(primary, secondary) {
  n1 <- sqrt(sum(primary^2))
  if (!is.finite(n1) || n1 == 0) {
    return(NULL)
  }
  e1 <- primary / n1

  rest <- secondary - sum(secondary * e1) * e1
  n2 <- sqrt(sum(rest^2))
  if (!is.finite(n2) || n2 < .Machine$double.eps^0.5) {
    return(NULL)
  }
  e2 <- rest / n2

  cbind(e1, e2, cross3(e1, e2))
}


#' Cross product of two 3-vectors
#'
#' @param a,b Numeric vectors of length 3.
#'
#' @return A numeric vector of length 3.
#' @keywords internal
cross3 <- function(a, b) {
  c(
    a[2] * b[3] - a[3] * b[2],
    a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1]
  )
}
