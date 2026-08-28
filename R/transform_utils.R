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
  anicore::wrap_angle(to_angle - from_angle, modulo = "pi")
}
