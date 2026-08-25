# Inclination (theta) from Cartesian coordinates

Calculates the angle measured from the positive z-axis. Points at the
origin return `0`.

## Usage

``` r
cartesian_to_theta(x, y, z)
```

## Arguments

- x:

  A numeric vector of x-coordinates.

- y:

  A numeric vector of y-coordinates.

- z:

  A numeric vector of z-coordinates.

## Value

A numeric vector of inclination angles in radians, between `0` and `pi`.

## See also

Other coordinate conversion:
[`cartesian_to_phi()`](https://animovement.dev/anispace/reference/cartesian_to_phi.md),
[`cartesian_to_rho()`](https://animovement.dev/anispace/reference/cartesian_to_rho.md),
[`polar_to_x()`](https://animovement.dev/anispace/reference/polar_to_x.md),
[`polar_to_y()`](https://animovement.dev/anispace/reference/polar_to_y.md),
[`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)

## Examples

``` r
# On the positive z-axis, the inclination is zero
cartesian_to_theta(0, 0, 1)
#> [1] 0

# In the xy-plane, it is a quarter turn
cartesian_to_theta(1, 0, 0)
#> [1] 1.570796
```
