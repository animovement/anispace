#' Cartesian radius (ρ) from coordinates
#'
#' Computes the Euclidean distance from the origin to a point in either 2‑D
#' (`z` omitted) or 3‑D space.
#'
#' @param x numeric vector of x‑coordinates
#' @param y numeric vector of y‑coordinates
#' @param z optional numeric vector of z‑coordinates; if `NULL` a 2‑D radius is returned
#' @return numeric vector of radii (ρ)
#' @export
cartesian_to_rho <- function(x, y, z = NULL) {
  if (is.null(z)) {
    sqrt(x^2 + y^2)
  } else {
    sqrt(x^2 + y^2 + z^2)
  }
}

#' Cartesian azimuth (φ) from coordinates
#'
#' Returns the planar angle measured from the positive x‑axis toward the
#' positive y‑axis.  By default the result is mapped to \[0, 2π); setting
#' `centered = TRUE` leaves the native `atan2` range \[-π, π\].
#'
#' @param x numeric vector of x‑coordinates
#' @param y numeric vector of y‑coordinates
#' @param centered logical; if `TRUE` keep the \[-π, π\] range, otherwise map to \[0, 2π\)
#' @return numeric vector of azimuth angles (φ) in radians
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

#' Polar angle (θ) from Cartesian coordinates
#'
#' Calculates the inclination angle measured from the positive z‑axis
#' (the “polar” angle) for each point.
#'
#' @param x numeric vector of x‑coordinates
#' @param y numeric vector of y‑coordinates
#' @param z numeric vector of z‑coordinates
#' @return numeric vector of polar angles (θ) in radians
#' @export
cartesian_to_theta <- function(x, y, z) {
  # Full 3‑D radius for each observation
  rho <- cartesian_to_rho(x, y, z)

  # Initialise theta with zeros (covers the origin case automatically)
  theta <- numeric(length(rho))

  # Identify rows where rho > 0 (i.e., not the origin)
  idx <- rho > 0

  # Compute acos only where it is safe
  theta[idx] <- acos(z[idx] / rho[idx])

  invisible(theta)
}

#' Convert polar radius to Cartesian x‑coordinate
#'
#' @param rho numeric vector of radial distances
#' @param phi numeric vector of azimuth angles (radians)
#' @return numeric vector of x‑coordinates
#' @export
polar_to_x <- function(rho, phi) {
  rho * cos(phi)
}

#' Convert polar radius to Cartesian y‑coordinate
#'
#' @param rho numeric vector of radial distances
#' @param phi numeric vector of azimuth angles (radians)
#' @return numeric vector of y‑coordinates
#' @export
polar_to_y <- function(rho, phi) {
  rho * sin(phi)
}

#' Convert cylindrical radius and polar angle to Cartesian z‑coordinate
#'
#' Handles regular points as well as the two pole regions (θ≈0 and θ≈π).
#' Non‑finite inputs remain `NA`.
#'
#' @param rho   Numeric vector – cylindrical radius (√(x² + y²)).
#' @param theta Numeric vector – polar angle measured from the +z axis (radians).
#' @return Numeric vector of z‑coordinates (same length as input)
#' @export
spherical_to_z <- function(rho, theta) {
  # Initialise output with NA so that any non‑finite input stays NA.
  z <- rep(NA_real_, length(rho))

  ## -----------------------------------------------------------------
  ## 1.  Identify well‑behaved (non‑pole, finite) entries
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
  # Positive‑z pole (θ ≈ 0)
  pos_pole_idx <- is.finite(rho) &
    is.finite(theta) &
    !ok_idx &
    theta < 0.5 * .Machine$double.eps

  if (any(pos_pole_idx)) {
    # By definition the point lies on the +z axis → z = 0
    z[pos_pole_idx] <- 0
  }

  # Negative‑z pole (θ ≈ π)
  neg_pole_idx <- is.finite(rho) &
    is.finite(theta) &
    !ok_idx &
    abs(theta - pi) < 0.5 * .Machine$double.eps

  if (any(neg_pole_idx)) {
    # Point lies on the –z axis → z = 0 (sign is irrelevant because radius = 0)
    z[neg_pole_idx] <- 0
  }

  ## -----------------------------------------------------------------
  ## 3.  Anything left untouched remains NA (covers NA/NaN/Inf inputs)
  ## -----------------------------------------------------------------
  invisible(z)
}
