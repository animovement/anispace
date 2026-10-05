# Azimuth (phi) from Cartesian coordinates

Returns the planar angle measured from the positive x-axis towards the
positive y-axis, in `(-pi, pi]`. That is the signed range the
animovement suite uses for every direction (see
[`anicore::wrap_angle()`](https://animovement.dev/anicore/reference/wrap_angle.html)),
and the one [`atan2()`](https://rdrr.io/r/base/Trig.html) gives, except
that a point on the negative x-axis is `pi`, never `-pi`. For
`[0, 2 * pi)`, wrap the result with `anicore::wrap_angle(phi, "2pi")`.

## Usage

``` r
cartesian_to_phi(x, y, centered = deprecated())
```

## Arguments

- x:

  A numeric vector of x-coordinates.

- y:

  A numeric vector of y-coordinates.

- centered:

  **\[deprecated\]** The result is always in `(-pi, pi]`, which
  `centered = TRUE` used to ask for. `centered = FALSE` still gives
  `[0, 2 * pi)`, with a warning, until the argument is removed; use
  `anicore::wrap_angle(cartesian_to_phi(x, y), "2pi")` instead.

## Value

A numeric vector of azimuth angles in radians, in `(-pi, pi]`.

## See also

Other coordinate conversion:
[`cartesian_to_rho()`](https://animovement.dev/anispace/reference/cartesian_to_rho.md),
[`cartesian_to_theta()`](https://animovement.dev/anispace/reference/cartesian_to_theta.md),
[`polar_to_x()`](https://animovement.dev/anispace/reference/polar_to_x.md),
[`polar_to_y()`](https://animovement.dev/anispace/reference/polar_to_y.md),
[`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)

## Examples

``` r
cartesian_to_phi(1, 1)
#> [1] 0.7853982

# Below the x-axis, the azimuth is negative
cartesian_to_phi(-1, -1)
#> [1] -2.356194

# On the negative x-axis, it is pi
cartesian_to_phi(-1, 0)
#> [1] 3.141593

# For [0, 2 * pi), wrap the result
anicore::wrap_angle(cartesian_to_phi(-1, -1), "2pi")
#> [1] 3.926991
```
