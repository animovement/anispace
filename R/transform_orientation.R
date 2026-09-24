#' Convert orientation between Euler angles and quaternions
#'
#' @description
#' Motion-capture software often exports 3D orientation as three Euler
#' angles, but an anipoint stores 3D orientation as a unit
#' quaternion (animovement/anicore#46), which has no gimbal lock and no
#' wraparound. These functions are where the two meet, and where the Euler
#' convention is stated once.
#'
#' * `transform_euler_to_quaternion()` adds quaternion columns computed from
#'   the Euler columns, declares them as the frame's orientation, and records
#'   the convention (`euler_sequence`, `euler_intrinsic`).
#' * `transform_quaternion_to_euler()` adds Euler columns computed from the
#'   declared orientation, as a derived view for reporting or plotting. It
#'   uses the recorded convention unless another is given.
#'
#' Angles are read and written in the frame's `unit_angle`.
#'
#' @param data A 3D anipoint.
#' @param euler The three Euler angle columns, in `sequence` order.
#' @param sequence,intrinsic The Euler convention; see [quat_from_euler()].
#'   For `transform_quaternion_to_euler()`, `NULL` uses the convention the
#'   frame recorded.
#' @param names Names for the new columns.
#'
#' @return `data` with the new columns; for `transform_euler_to_quaternion()`,
#'   with `where$orientation` declared.
#'
#' @examples
#' df <- data.frame(
#'   time = 1:3, x = 0, y = 0, z = 0,
#'   yaw = c(0, 0.1, 0.2), pitch = 0, roll = c(0, 0, 0.1)
#' )
#' af <- anicore::as_anipoint(df) |>
#'   transform_euler_to_quaternion(
#'     c("yaw", "pitch", "roll"),
#'     sequence = "ZYX",
#'     intrinsic = TRUE
#'   )
#' anicore::get_variables(af, "where", "orientation")
#' @export
transform_euler_to_quaternion <- function(
  data,
  euler,
  sequence,
  intrinsic,
  names = c("qw", "qx", "qy", "qz")
) {
  anicore::ensure_is_anipoint(data)
  ensure_new_columns(data, names, 4L)
  if (
    !is.character(euler) ||
      length(euler) != 3L ||
      !all(euler %in% colnames(data))
  ) {
    cli::cli_abort("{.arg euler} must name three columns of {.arg data}.")
  }
  angles <- as.matrix(as.data.frame(data)[, euler])
  if (!is.numeric(angles)) {
    cli::cli_abort("The Euler columns must be numeric.")
  }
  if (is_degrees(data)) {
    angles <- angles * pi / 180
  }

  q <- quat_from_euler(angles, sequence, intrinsic)
  for (i in 1:4) {
    data[[names[[i]]]] <- q[, i]
  }
  data <- anicore::set_variables(
    data,
    where = list(
      orientation = stats::setNames(names, c("qw", "qx", "qy", "qz"))
    )
  )
  anicore::set_metadata(
    data,
    euler_sequence = sequence,
    euler_intrinsic = intrinsic
  )
}


#' @rdname transform_euler_to_quaternion
#' @export
transform_quaternion_to_euler <- function(
  data,
  sequence = NULL,
  intrinsic = NULL,
  names = c("euler_1", "euler_2", "euler_3")
) {
  anicore::ensure_is_anipoint(data)
  if (is.null(sequence) || is.null(intrinsic)) {
    recorded_sequence <- anicore::get_metadata(data, "euler_sequence")
    recorded_intrinsic <- anicore::get_metadata(data, "euler_intrinsic")
    if (is.na(recorded_sequence %||% NA) || is.na(recorded_intrinsic %||% NA)) {
      cli::cli_abort(c(
        "No Euler convention is recorded for this frame.",
        "i" = "Pass {.arg sequence} and {.arg intrinsic}, or record one with {.code anicore::set_metadata(data, euler_sequence = , euler_intrinsic = )}."
      ))
    }
    sequence <- sequence %||% recorded_sequence
    intrinsic <- intrinsic %||% recorded_intrinsic
  }
  ensure_new_columns(data, names, 3L)
  orientation <- anicore::get_variables(data, "where", "orientation")
  if (!all(c("qw", "qx", "qy", "qz") %in% names(orientation))) {
    cli::cli_abort(c(
      "The frame has no quaternion orientation declared.",
      "i" = "Declare one with {.fn transform_euler_to_quaternion} or {.code anicore::set_variables(data, where = list(orientation = ))}."
    ))
  }

  q <- as.matrix(as.data.frame(data)[, orientation[c("qw", "qx", "qy", "qz")]])
  angles <- quat_to_euler(q, sequence, intrinsic)
  if (is_degrees(data)) {
    angles <- angles * 180 / pi
  }
  for (i in 1:3) {
    data[[names[[i]]]] <- angles[, i]
  }
  data
}


#' @keywords internal
ensure_new_columns <- function(data, names, n) {
  if (!is.character(names) || length(names) != n || anyDuplicated(names)) {
    cli::cli_abort("{.arg names} must be {n} distinct column names.")
  }
  taken <- intersect(names, colnames(data))
  if (length(taken) > 0L) {
    cli::cli_abort("Column{?s} {.val {taken}} already exist{?s/}.")
  }
  invisible(TRUE)
}


#' @keywords internal
is_degrees <- function(data) {
  identical(as.character(anicore::get_metadata(data, "unit_angle")), "deg")
}
