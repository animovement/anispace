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
#' positive y-axis, in `(-pi, pi]`. That is the signed range the animovement
#' suite uses for every direction (see [anicore::wrap_angle()]), and the one
#' [atan2()] gives, except that a point on the negative x-axis is `pi`, never
#' `-pi`. For `[0, 2 * pi)`, wrap the result with
#' `anicore::wrap_angle(phi, "2pi")`.
#'
#' @param x A numeric vector of x-coordinates.
#' @param y A numeric vector of y-coordinates.
#' @param centered `r lifecycle::badge("deprecated")` The result is always in
#'   `(-pi, pi]`, which `centered = TRUE` used to ask for. `centered = FALSE`
#'   still gives `[0, 2 * pi)`, with a warning, until the argument is removed;
#'   use `anicore::wrap_angle(cartesian_to_phi(x, y), "2pi")` instead.
#' @return A numeric vector of azimuth angles in radians, in `(-pi, pi]`.
#' @family coordinate conversion
#' @examples
#' cartesian_to_phi(1, 1)
#'
#' # Below the x-axis, the azimuth is negative
#' cartesian_to_phi(-1, -1)
#'
#' # On the negative x-axis, it is pi
#' cartesian_to_phi(-1, 0)
#'
#' # For [0, 2 * pi), wrap the result
#' anicore::wrap_angle(cartesian_to_phi(-1, -1), "2pi")
#' @export
cartesian_to_phi <- function(x, y, centered = deprecated()) {
  # atan2(y, x) returns angles in [-pi, pi]
  angle <- atan2(y, x)

  if (lifecycle::is_present(centered)) {
    if (isTRUE(centered)) {
      lifecycle::deprecate_warn(
        "0.4.0",
        "cartesian_to_phi(centered)",
        details = "The result is always in (-pi, pi], so it is not needed."
      )
    } else {
      lifecycle::deprecate_warn(
        "0.4.0",
        "cartesian_to_phi(centered)",
        details = paste(
          "For [0, 2 * pi), use",
          "`anicore::wrap_angle(cartesian_to_phi(x, y), \"2pi\")`."
        )
      )
      return(angle %% (2 * pi))
    }
  }

  # -pi and pi are the same direction; the signed range keeps pi
  angle[!is.na(angle) & angle == -pi] <- pi
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
#' @param phi A numeric vector of azimuth angles, in radians, in any range.
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
#' @param phi A numeric vector of azimuth angles, in radians, in any range.
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
#' Non-finite inputs return `NA`.
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
  # z = r * cos(theta). The pole cases need no special handling: cos(0) = 1
  # and cos(pi) = -1 recover the full height, which the old cylindrical
  # formulation could not — with rho as the xy-plane radius, a point on the
  # z-axis has rho = 0 and its height is unrecoverable (#19).
  z <- rho * cos(theta)
  z[!is.finite(rho) | !is.finite(theta)] <- NA_real_
  z
}
