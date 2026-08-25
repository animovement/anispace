# Shortest signed distance between two angles

Computes the shortest signed angular distance from `from_angle` to
`to_angle`, wrapped to `(-pi, pi]`. Going from one angle to another the
long way round therefore returns the short way, with a sign giving the
direction.

## Usage

``` r
calculate_angular_difference(from_angle, to_angle)
```

## Arguments

- from_angle:

  A numeric starting angle, in radians.

- to_angle:

  A numeric target angle, in radians.

## Value

A numeric angular difference in radians, in `(-pi, pi]`.

## See also

Other angle utilities:
[`diff_angle()`](https://animovement.dev/anispace/reference/diff_angle.md),
[`unwrap_angle()`](https://animovement.dev/anispace/reference/unwrap_angle.md),
[`wrap_angle()`](https://animovement.dev/anispace/reference/wrap_angle.md)

## Examples

``` r
calculate_angular_difference(0, pi / 2)
#> [1] 1.570796

# The short way round is anticlockwise, so the result is negative
calculate_angular_difference(0.1, 2 * pi - 0.1)
#> [1] -0.2
```
