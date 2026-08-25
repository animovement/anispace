#' Radius (rho) from Cartesian coordinates
#'
#' Computes the Euclidean distance from the origin to a point, in either two
#' dimensions (`z` omitted) or three.
#'
#' @param x A numeric vector of x-coordinates.
#' @param y A numeric vector of y-coordinates.
#' @param z An optional numeric vector of z-coordinates (default `NULL`). When
#'   `NULL`, a two-dimensional radius is returned.
#' @return A numeric vector of radii, the same length as `x`.
#' @family coordinate conversion
#' @examples
#' cartesian_to_rho(3, 4)
#'
#' # Supplying z gives the three-dimensional radius
#' cartesian_to_rho(3, 4, 12)
#' @export
cartesian_to_rho <- function(x, y, z = NULL) {
  if (is.null(z)) {
    sqrt(x^2 + y^2)
  } else {
    sqrt(x^2 + y^2 + z^2)
  }
}

#' Azimuth (phi) from Cartesian coordinates
#'
#' Returns the planar angle measured from the positive x-axis towards the
#' positive y-axis.
#'
#' @param x A numeric vector of x-coordinates.
#' @param y A numeric vector of y-coordinates.
#' @param centered A logical value (default `FALSE`) determining the range of
#'   the result. `FALSE` maps angles to `[0, 2*pi)`; `TRUE` keeps the native
#'   [atan2()] range of `[-pi, pi]`.
#' @return A numeric vector of azimuth angles in radians.
#' @family coordinate conversion
#' @examples
#' cartesian_to_phi(1, 1)
#'
#' # The two ranges differ for points below the x-axis
#' cartesian_to_phi(-1, -1)
#' cartesian_to_phi(-1, -1, centered = TRUE)
#' @export
cartesian_to_phi <- function(x, y, centered = FALSE) {
  # atan2(y, x) returns angles in [-pi, pi]
  angle <- atan2(y, x)

  if (!centered) {
    # map to [0, 2*pi)
    angle <- (angle %% (2 * pi))
  }
  angle
}

#' Inclination (theta) from Cartesian coordinates
#'
#' Calculates the angle measured from the positive z-axis. Points at the origin
#' return `0`.
#'
#' @param x A numeric vector of x-coordinates.
#' @param y A numeric vector of y-coordinates.
#' @param z A numeric vector of z-coordinates.
#' @return A numeric vector of inclination angles in radians, between `0` and
#'   `pi`.
#' @family coordinate conversion
#' @examples
#' # On the positive z-axis, the inclination is zero
#' cartesian_to_theta(0, 0, 1)
#'
#' # In the xy-plane, it is a quarter turn
#' cartesian_to_theta(1, 0, 0)
#' @export
cartesian_to_theta <- function(x, y, z) {
  # Full 3-D radius for each observation
  rho <- cartesian_to_rho(x, y, z)

  # Initialise theta with zeros (covers the origin case automatically)
  theta <- numeric(length(rho))

  # Identify rows where rho > 0 (i.e., not the origin)
  idx <- rho > 0

  # Compute acos only where it is safe
  theta[idx] <- acos(z[idx] / rho[idx])

  theta
}

#' Cartesian x-coordinate from polar coordinates
#'
#' @param rho A numeric vector of radial distances.
#' @param phi A numeric vector of azimuth angles, in radians.
#' @return A numeric vector of x-coordinates.
#' @family coordinate conversion
#' @examples
#' polar_to_x(1, pi / 3)
#' @export
polar_to_x <- function(rho, phi) {
  rho * cos(phi)
}

#' Cartesian y-coordinate from polar coordinates
#'
#' @param rho A numeric vector of radial distances.
#' @param phi A numeric vector of azimuth angles, in radians.
#' @return A numeric vector of y-coordinates.
#' @family coordinate conversion
#' @examples
#' polar_to_y(1, pi / 3)
#' @export
polar_to_y <- function(rho, phi) {
  rho * sin(phi)
}

#' Cartesian z-coordinate from spherical coordinates
#'
#' Handles the two pole regions (inclination near `0` or `pi`) as well as
#' regular points. Non-finite inputs return `NA`.
#'
#' @param rho A numeric vector of cylindrical radii, that is `sqrt(x^2 + y^2)`.
#' @param theta A numeric vector of inclination angles measured from the
#'   positive z-axis, in radians.
#' @return A numeric vector of z-coordinates, the same length as `rho`.
#' @family coordinate conversion
#' @examples
#' spherical_to_z(1, pi / 4)
#'
#' # Non-finite input propagates as NA
#' spherical_to_z(c(1, NA), c(pi / 4, pi / 4))
#' @export
spherical_to_z <- function(rho, theta) {
  # Initialise output with NA so that any non-finite input stays NA.
  z <- rep(NA_real_, length(rho))

  ## -----------------------------------------------------------------
  ## 1.  Identify well-behaved (non-pole, finite) entries
  ## -----------------------------------------------------------------
  ok_idx <- is.finite(rho) &
    is.finite(theta) &
    abs(sin(theta)) > .Machine$double.eps # sin(theta) ≠ 0 → not a pole

  if (any(ok_idx)) {
    # Regular case:  z = ρ / tan(θ)  (equivalently ρ * cot(θ))
    z[ok_idx] <- rho[ok_idx] / tan(theta[ok_idx])
  }

  ## -----------------------------------------------------------------
  ## 2.  Handle the two pole regions (θ ≈ 0  or  θ ≈ π)
  ## -----------------------------------------------------------------
  # Positive-z pole (θ ≈ 0)
  pos_pole_idx <- is.finite(rho) &
    is.finite(theta) &
    !ok_idx &
    theta < 0.5 * .Machine$double.eps

  if (any(pos_pole_idx)) {
    # By definition the point lies on the +z axis → z = 0
    z[pos_pole_idx] <- 0
  }

  # Negative-z pole (θ ≈ π)
  neg_pole_idx <- is.finite(rho) &
    is.finite(theta) &
    !ok_idx &
    abs(theta - pi) < 0.5 * .Machine$double.eps

  if (any(neg_pole_idx)) {
    # Point lies on the -z axis → z = 0 (sign is irrelevant because radius = 0)
    z[neg_pole_idx] <- 0
  }

  ## -----------------------------------------------------------------
  ## 3.  Anything left untouched remains NA (covers NA/NaN/Inf inputs)
  ## -----------------------------------------------------------------
  z
}
