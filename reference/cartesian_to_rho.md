# Radius (rho) from Cartesian coordinates

Computes the Euclidean distance from the origin to a point, in either
two dimensions (`z` omitted) or three.

## Usage

``` r
cartesian_to_rho(x, y, z = NULL)
```

## Arguments

- x:

  A numeric vector of x-coordinates.

- y:

  A numeric vector of y-coordinates.

- z:

  An optional numeric vector of z-coordinates (default `NULL`). When
  `NULL`, a two-dimensional radius is returned.

## Value

A numeric vector of radii, the same length as `x`.

## See also

Other coordinate conversion:
[`cartesian_to_phi()`](https://animovement.dev/anispace/reference/cartesian_to_phi.md),
[`cartesian_to_theta()`](https://animovement.dev/anispace/reference/cartesian_to_theta.md),
[`polar_to_x()`](https://animovement.dev/anispace/reference/polar_to_x.md),
[`polar_to_y()`](https://animovement.dev/anispace/reference/polar_to_y.md),
[`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)

## Examples

``` r
cartesian_to_rho(3, 4)
#> [1] 5

# Supplying z gives the three-dimensional radius
cartesian_to_rho(3, 4, 12)
#> [1] 13
```
