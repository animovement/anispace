# Convert orientation between Euler angles and quaternions

Trackers such as FicTrac and motion-capture software export orientation
as three Euler angles, but an anipoint stores 3D orientation as a unit
quaternion (animovement/anicore#46), which has no gimbal lock and no
wraparound. These functions are where the two meet, and where the Euler
convention is stated once.

- `transform_euler_to_quaternion()` adds quaternion columns computed
  from the Euler columns, declares them as the frame's orientation, and
  records the convention (`euler_sequence`, `euler_intrinsic`).

- `transform_quaternion_to_euler()` adds Euler columns computed from the
  declared orientation, as a derived view for reporting or plotting. It
  uses the recorded convention unless another is given.

Angles are read and written in the frame's `unit_angle`.

## Usage

``` r
transform_euler_to_quaternion(
  data,
  euler,
  sequence,
  intrinsic,
  names = c("qw", "qx", "qy", "qz")
)

transform_quaternion_to_euler(
  data,
  sequence = NULL,
  intrinsic = NULL,
  names = c("euler_1", "euler_2", "euler_3")
)
```

## Arguments

- data:

  A 3D anipoint.

- euler:

  The three Euler angle columns, in `sequence` order.

- sequence, intrinsic:

  The Euler convention; see
  [`quat_from_euler()`](https://animovement.dev/anispace/reference/quat_from_euler.md).
  For `transform_quaternion_to_euler()`, `NULL` uses the convention the
  frame recorded.

- names:

  Names for the new columns.

## Value

`data` with the new columns; for `transform_euler_to_quaternion()`, with
`where$orientation` declared.

## Examples

``` r
df <- data.frame(
  time = 1:3, x = 0, y = 0, z = 0,
  yaw = c(0, 0.1, 0.2), pitch = 0, roll = c(0, 0, 0.1)
)
af <- anicore::as_anipoint(df) |>
  transform_euler_to_quaternion(
    c("yaw", "pitch", "roll"),
    sequence = "ZYX",
    intrinsic = TRUE
  )
anicore::get_variables(af, "where", "orientation")
#>   qw   qx   qy   qz 
#> "qw" "qx" "qy" "qz" 
```
