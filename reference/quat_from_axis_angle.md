# Quaternions from and to other rotation representations

- `quat_from_axis_angle()` / `quat_to_axis_angle()`: a rotation of
  `angle` radians about `axis`, counter-clockwise by the right-hand
  rule.

- `quat_from_matrix()` / `quat_to_matrix()`: 3x3 rotation matrices, or a
  3x3xn array with one matrix per row.

## Usage

``` r
quat_from_axis_angle(axis, angle)

quat_to_axis_angle(q)

quat_from_matrix(rotation)

quat_to_matrix(q)
```

## Arguments

- axis:

  Axis vectors (length 3, or 3 columns); need not be unit length.

- angle:

  Angles in radians.

- q:

  Quaternions; see
  [quaternions](https://animovement.dev/anispace/reference/quaternions.md).

- rotation:

  A 3x3 rotation matrix, or a 3x3xn array.

## Value

`quat_from_*()`: a quaternion matrix. `quat_to_axis_angle()`: a list
with `axis` (3 columns) and `angle` in `[0, pi]`; the axis of a zero
rotation is `NA`. `quat_to_matrix()`: a 3x3xn array.

## Examples

``` r
q <- quat_from_axis_angle(c(0, 0, 2), pi / 2)
quat_to_axis_angle(q)
#> $axis
#>      x y z
#> [1,] 0 0 1
#> 
#> $angle
#> [1] 1.570796
#> 
quat_to_matrix(q)[, , 1]
#>              [,1]          [,2] [,3]
#> [1,] 2.220446e-16 -1.000000e+00    0
#> [2,] 1.000000e+00  2.220446e-16    0
#> [3,] 0.000000e+00  0.000000e+00    1
quat_from_matrix(quat_to_matrix(q))
#>              w x y         z
#> [1,] 0.7071068 0 0 0.7071068
```
