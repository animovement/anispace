# Azimuth (phi) from Cartesian coordinates

Returns the planar angle measured from the positive x-axis towards the
positive y-axis.

## Usage

``` r
cartesian_to_phi(x, y, centered = FALSE)
```

## Arguments

- x:

  A numeric vector of x-coordinates.

- y:

  A numeric vector of y-coordinates.

- centered:

  A logical value (default `FALSE`) determining the range of the result.
  `FALSE` maps angles to `[0, 2*pi)`; `TRUE` keeps the native
  [`atan2()`](https://rdrr.io/r/base/Trig.html) range of `[-pi, pi]`.

## Value

A numeric vector of azimuth angles in radians.

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

# The two ranges differ for points below the x-axis
cartesian_to_phi(-1, -1)
#> [1] 3.926991
cartesian_to_phi(-1, -1, centered = TRUE)
#> [1] -2.356194
```
