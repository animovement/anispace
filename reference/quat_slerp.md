# Interpolate, average and differentiate rotations

- `quat_slerp(p, q, t)`: spherical linear interpolation, `t = 0` giving
  `p` and `t = 1` giving `q`, along the shorter arc.

- `quat_mean(q, weights)`: the average rotation, as the eigenvector of
  the largest eigenvalue of `sum(w q q')` (Markley et al. 2007). Unlike
  averaging components, it is unaffected by the sign ambiguity.

- `quat_continuous(q)`: flips signs so each row lies on the same side as
  the previous one. `q` and `-q` are the same rotation, so this changes
  no rotation, but smoothing or differentiating a series without it goes
  wrong wherever the sign jumps.

- `quat_angular_velocity(q, dt)`: angular velocity between successive
  rows, in radians per unit of `dt`, about the fixed frame's axes
  (`frame = "fixed"`) or the body's own (`frame = "body"`). The first
  row is `NA`.

Rows with `NA` are skipped by `quat_mean()` and carried through by the
others.

## Usage

``` r
quat_slerp(p, q, t)

quat_mean(q, weights = NULL)

quat_continuous(q)

quat_angular_velocity(q, dt, frame = c("fixed", "body"))
```

## Arguments

- p, q:

  Quaternions; see
  [quaternions](https://animovement.dev/anispace/reference/quaternions.md).

- t:

  Interpolation fraction(s), recycled against the rows.

- weights:

  Optional non-negative weights, one per row.

- dt:

  Time between rows: one value, or one per row.

- frame:

  `"fixed"` or `"body"`.

## Value

A quaternion matrix; `quat_mean()` returns one row;
`quat_angular_velocity()` returns a matrix with columns `x`, `y`, `z`.

## Examples

``` r
a <- quat_from_axis_angle(c(0, 0, 1), 0)
b <- quat_from_axis_angle(c(0, 0, 1), pi / 2)
quat_slerp(a, b, 0.5)
#>              w x y         z
#> [1,] 0.9238795 0 0 0.3826834
quat_mean(rbind(a, b))
#>              w x y         z
#> [1,] 0.9238795 0 0 0.3826834

spin <- quat_from_axis_angle(c(0, 0, 1), seq(0, 1, by = 0.1))
quat_angular_velocity(spin, dt = 1 / 30)
#>        x  y  z
#>  [1,] NA NA NA
#>  [2,]  0  0  3
#>  [3,]  0  0  3
#>  [4,]  0  0  3
#>  [5,]  0  0  3
#>  [6,]  0  0  3
#>  [7,]  0  0  3
#>  [8,]  0  0  3
#>  [9,]  0  0  3
#> [10,]  0  0  3
#> [11,]  0  0  3
```
