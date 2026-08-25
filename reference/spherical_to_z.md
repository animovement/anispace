# Cartesian z-coordinate from spherical coordinates

Non-finite inputs return `NA`.

## Usage

``` r
spherical_to_z(rho, theta)
```

## Arguments

- rho:

  A numeric vector of cylindrical radii, that is `sqrt(x^2 + y^2)`.

- theta:

  A numeric vector of inclination angles measured from the positive
  z-axis, in radians.

## Value

A numeric vector of z-coordinates, the same length as `rho`.

## See also

Other coordinate conversion:
[`cartesian_to_phi()`](https://animovement.dev/anispace/reference/cartesian_to_phi.md),
[`cartesian_to_rho()`](https://animovement.dev/anispace/reference/cartesian_to_rho.md),
[`cartesian_to_theta()`](https://animovement.dev/anispace/reference/cartesian_to_theta.md),
[`polar_to_x()`](https://animovement.dev/anispace/reference/polar_to_x.md),
[`polar_to_y()`](https://animovement.dev/anispace/reference/polar_to_y.md)

## Examples

``` r
spherical_to_z(1, pi / 4)
#> [1] 0.7071068

# Non-finite input propagates as NA
spherical_to_z(c(1, NA), c(pi / 4, pi / 4))
#> [1] 0.7071068        NA
```
