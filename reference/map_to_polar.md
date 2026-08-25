# Map from Cartesian to polar coordinates

Map from Cartesian to polar coordinates

## Usage

``` r
map_to_polar(data)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

## Value

An aniframe with `rho` and `phi` in place of `x` and `y`.

## See also

Other coordinate systems:
[`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md),
[`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md),
[`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md)

## Examples

``` r
af <- aniframe::example_aniframe(
  n_obs = 5, n_individuals = 1, n_keypoints = 1
)
map_to_polar(af)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint session trial  time   rho   phi confidence
#>        <int> <fct>      <int> <int> <int> <dbl> <dbl>      <dbl>
#> 1          1 centroid       1     1     1 0.303 2.74       0.724
#> 2          1 centroid       1     1     2 1.94  4.55       0.762
#> 3          1 centroid       1     1     3 1.37  0.679      0.616
#> 4          1 centroid       1     1     4 0.253 4.99       0.705
#> 5          1 centroid       1     1     5 0.672 3.45       0.583
```
