#' Differences between successive angles
#'
#' Computes lagged differences between successive angles, converting each raw
#' subtraction into the shortest signed angular distance with
#' [calculate_angular_difference()]. The output mirrors [base::diff()], returning
#' `NA` for the first `lag` positions so it can be used inside
#' [dplyr::mutate()].
#'
#' @param x A numeric vector of angles, in radians.
#' @param lag A positive integer (default `1L`) giving the lag to difference at.
#' @return A numeric vector the same length as `x`. The first `lag` entries are
#'   `NA`; the rest are angular differences in radians.
#' @family angle utilities
#' @examples
#' angles <- c(0, pi / 2, pi, 3 * pi / 2)
#' diff_angle(angles)
#'
#' # A larger lag compares points further apart
#' diff_angle(angles, lag = 2L)
#' @export
diff_angle <- function(x, lag = 1L) {
  # Input validation - mimic base::diff's checks
  if (!is.numeric(x)) {
    cli::cli_abort("`x` must be a numeric vector of angles (in radians).")
  }
  if (lag < 1L || !is.integer(lag)) {
    cli::cli_abort("`lag` must be a positive integer.")
  }

  # Base case: no work to do
  if (length(x) <= lag) {
    return(numeric(0))
  }

  # Compute successive differences recursively, just like base::diff
  result <- x[(lag + 1):length(x)] - x[seq_len(length(x) - lag)]

  # Apply the angular-distance conversion element-wise
  result <- mapply(
    calculate_angular_difference,
    from_angle = x[seq_len(length(x) - lag)],
    to_angle = x[(lag + 1):length(x)]
  )

  # Prepend NAs to make it work in dplyr mutate functions
  result <- c(rep(NA, lag), result)
  result
}

#' Shortest signed distance between two angles
#'
#' Computes the shortest signed angular distance from `from_angle` to
#' `to_angle`, wrapped to `(-pi, pi]`. Going from one angle to another the long
#' way round therefore returns the short way, with a sign giving the direction.
#'
#' @param from_angle A numeric starting angle, in radians.
#' @param to_angle A numeric target angle, in radians.
#' @return A numeric angular difference in radians, in `(-pi, pi]`.
#' @family angle utilities
#' @examples
#' calculate_angular_difference(0, pi / 2)
#'
#' # The short way round is anticlockwise, so the result is negative
#' calculate_angular_difference(0.1, 2 * pi - 0.1)
#' @export
calculate_angular_difference <- function(from_angle, to_angle) {
  wrap_angle(to_angle - from_angle, modulo = "pi")
}

#' Constrain angles to a standard range
#'
#' Wraps a vector of angles to a standard interval using modulo arithmetic.
#'
#' @param x A numeric vector of angles, in radians.
#' @param modulo A character string (default `"2pi"`) giving the target range:
#'   \describe{
#'     \item{`"2pi"`}{Wrap to `[0, 2*pi)`.}
#'     \item{`"pi"`}{Wrap to `(-pi, pi]`.}
#'     \item{`"asis"`}{Return unchanged.}
#'   }
#' @return A numeric vector the same length as `x`, wrapped to the chosen range.
#' @family angle utilities
#' @examples
#' angles <- c(-pi, 0, pi, 2 * pi, 3 * pi)
#'
#' wrap_angle(angles, "2pi")
#'
#' # The same angles on the signed interval
#' wrap_angle(angles, "pi")
#'
#' # "asis" is a no-op, useful when the range is chosen by a caller
#' wrap_angle(angles, "asis")
#' @export
wrap_angle <- function(x, modulo = c("2pi", "pi", "asis")) {
  modulo <- match.arg(modulo)

  switch(
    modulo,
    "2pi" = x %% (2 * pi),
    "pi" = pi - ((pi - x) %% (2 * pi)),
    "asis" = x
  )
}

#' Remove wrapping from a sequence of angles
#'
#' Reverses the discontinuity introduced by wrapping, by accumulating the
#' shortest step between successive angles. A heading that crosses `2*pi`
#' therefore continues to increase rather than jumping back to zero, which is
#' what makes it differentiable. `NA` values are preserved in place.
#'
#' @param x A numeric vector of angles, in radians.
#' @return A numeric vector the same length as `x`, without wrapping
#'   discontinuities.
#' @family angle utilities
#' @examples
#' # A heading turning steadily past a full circle, wrapped to [0, 2*pi)
#' wrapped <- wrap_angle(seq(0, 3 * pi, length.out = 7), "2pi")
#' wrapped
#'
#' # Unwrapping restores the steady progression
#' unwrap_angle(wrapped)
#' @export
unwrap_angle <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }

  if (all(is.na(x))) {
    return(x)
  }

  result <- numeric(length(x))
  result[is.na(x)] <- NA_real_

  non_na_idx <- which(!is.na(x))
  x_clean <- x[non_na_idx]

  angle_diff <- diff(x_clean)
  angle_diff_wrapped <- wrap_angle(angle_diff, modulo = "pi")
  unwrapped_clean <- c(x_clean[1], x_clean[1] + cumsum(angle_diff_wrapped))

  result[non_na_idx] <- unwrapped_clean
  result
}
