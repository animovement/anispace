# Quaternion algebra

Row-wise operations on unit quaternions. A quaternion argument is a
numeric vector of length 4, or a matrix or data frame with four columns,
in the order `w`, `x`, `y`, `z` (Hamilton convention, scalar first).
Columns named `w`/`x`/`y`/`z` or `qw`/`qx`/`qy`/`qz` are matched by
name. A single row is recycled against the other argument.

- `quat_multiply(p, q)`: the Hamilton product `p q`, the rotation `q`
  followed by `p`.

- `quat_conjugate(q)`: the inverse of a unit quaternion.

- `quat_normalise(q)`: scaled to unit norm; a zero quaternion gives
  `NA`.

- `quat_rotate(q, v)`: rotates 3D vectors `v` (length 3, or 3 columns).

- `quat_distance(p, q)`: the angle of the rotation between `p` and `q`,
  in `[0, pi]`. `q` and `-q` are the same rotation, so their distance is
  0.

## Usage

``` r
quat_multiply(p, q)

quat_conjugate(q)

quat_normalise(q)

quat_rotate(q, v)

quat_distance(p, q)
```

## Arguments

- p, q:

  Quaternions.

- v:

  3D vectors.

## Value

A matrix with columns `w`, `x`, `y`, `z` (or `x`, `y`, `z` for
`quat_rotate()`), or a numeric vector for `quat_distance()`.

## See also

[`quat_from_euler()`](https://animovement.dev/anispace/reference/quat_from_euler.md),
[`quat_slerp()`](https://animovement.dev/anispace/reference/quat_slerp.md)

## Examples

``` r
quarter_z <- quat_from_axis_angle(c(0, 0, 1), pi / 2)
quat_rotate(quarter_z, c(1, 0, 0))
#>              x y z
#> y 2.220446e-16 1 0
quat_multiply(quarter_z, quarter_z)
#>                 w x y z
#> [1,] 2.220446e-16 0 0 1
quat_distance(quarter_z, quat_conjugate(quarter_z))
#> [1] 3.141593
```
