# Cartesian x-coordinate from polar coordinates

Cartesian x-coordinate from polar coordinates

## Usage

``` r
polar_to_x(rho, phi)
```

## Arguments

- rho:

  A numeric vector of radial distances.

- phi:

  A numeric vector of azimuth angles, in radians.

## Value

A numeric vector of x-coordinates.

## See also

Other coordinate conversion:
[`cartesian_to_phi()`](https://animovement.dev/anispace/reference/cartesian_to_phi.md),
[`cartesian_to_rho()`](https://animovement.dev/anispace/reference/cartesian_to_rho.md),
[`cartesian_to_theta()`](https://animovement.dev/anispace/reference/cartesian_to_theta.md),
[`polar_to_y()`](https://animovement.dev/anispace/reference/polar_to_y.md),
[`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)

## Examples

``` r
polar_to_x(1, pi / 3)
#> [1] 0.5
```
