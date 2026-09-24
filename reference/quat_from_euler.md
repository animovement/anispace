# Quaternions from and to Euler angles

Three rotations about coordinate axes, applied in `sequence`. There are
twelve sequences: six Tait-Bryan (`"ZYX"`, `"XYZ"`, …, all three axes)
and six proper Euler (`"ZXZ"`, `"XYX"`, …, first and last axis the
same).

- **Intrinsic** rotations are about the body's own axes, which move with
  each rotation. Intrinsic `"ZYX"` is yaw, pitch, roll.

- **Extrinsic** rotations are about the fixed frame's axes. An intrinsic
  sequence equals the reversed extrinsic sequence with the angles
  reversed.

Neither `sequence` nor `intrinsic` has a default: an assumed convention
is the classic source of Euler-angle bugs, so the call must say which is
meant. Euler angles are a report format; store orientation as
quaternions.

## Usage

``` r
quat_from_euler(angles, sequence, intrinsic)

quat_to_euler(q, sequence, intrinsic)
```

## Arguments

- angles:

  Angles in radians: a vector of length 3, or three columns, in
  `sequence` order.

- sequence:

  Three axes, e.g. `"ZYX"`. Case is ignored.

- intrinsic:

  `TRUE` for rotations about the moving body axes, `FALSE` for rotations
  about the fixed axes.

- q:

  Quaternions; see
  [quaternions](https://animovement.dev/anispace/reference/quaternions.md).

## Value

`quat_from_euler()`: a quaternion matrix. `quat_to_euler()`: a matrix
with three columns in `sequence` order. The middle angle is in
`[-pi/2, pi/2]` for Tait-Bryan sequences and `[0, pi]` for proper ones;
the others are in `(-pi, pi]`. At gimbal lock the third angle is 0.

## Examples

``` r
ypr <- c(pi / 2, 0.1, -0.2)
q <- quat_from_euler(ypr, sequence = "ZYX", intrinsic = TRUE)
quat_to_euler(q, sequence = "ZYX", intrinsic = TRUE)
#>           z_1 y_2  x_3
#> [1,] 1.570796 0.1 -0.2

# The same rotation, described extrinsically
quat_to_euler(q, sequence = "XYZ", intrinsic = FALSE)
#>       x_1 y_2      z_3
#> [1,] -0.2 0.1 1.570796
```
