# Map from Cartesian to spherical coordinates

Map from Cartesian to spherical coordinates

## Usage

``` r
map_to_spherical(data)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

## Value

An aniframe with `rho`, `phi` and `theta` in place of `x`, `y` and `z`.
`rho` is the radial distance from the origin, `theta` the inclination
from the positive z-axis, and `phi` the azimuth — the convention of ISO
80000-2.

## See also

Other coordinate systems:
[`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md),
[`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md),
[`map_to_polar()`](https://animovement.dev/anispace/reference/map_to_polar.md)

## Examples

``` r
af <- anicore::example_aniframe(
  n_obs = 5, n_individuals = 1, n_keypoints = 1, n_dims = 3
)
map_to_spherical(af)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint session trial  time   rho    phi theta confidence
#>        <int> <fct>      <int> <int> <int> <dbl>  <dbl> <dbl>      <dbl>
#> 1          1 centroid       1     1     1 0.547 4.79   0.467      0.782
#> 2          1 centroid       1     1     2 2.07  4.74   2.54       0.669
#> 3          1 centroid       1     1     3 1.85  5.23   2.49       0.450
#> 4          1 centroid       1     1     4 2.53  2.70   1.46       0.830
#> 5          1 centroid       1     1     5 3.00  0.0490 1.11       0.438
```
