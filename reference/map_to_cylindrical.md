# Map from Cartesian to cylindrical coordinates

The Cartesian columns are read from the frame's declared axes, so they
can have any name. `phi` is written in the frame's `unit_angle`, in
`(-pi, pi]` or `(-180, 180]` (see
[`cartesian_to_phi()`](https://animovement.dev/anispace/reference/cartesian_to_phi.md)).

## Usage

``` r
map_to_cylindrical(data)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

## Value

An aniframe with `rho` and `phi` in place of `x` and `y`, keeping `z`.

## See also

Other coordinate systems:
[`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md),
[`map_to_polar()`](https://animovement.dev/anispace/reference/map_to_polar.md),
[`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md)

## Examples

``` r
af <- anicore::example_anipoint(
  n_obs = 5, n_individuals = 1, n_keypoints = 1, n_dims = 3
)
map_to_cylindrical(af)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint session trial  time   rho   phi      z confidence
#>        <int> <fct>      <int> <int> <int> <dbl> <dbl>  <dbl>      <dbl>
#> 1          1 centroid       1     1     1 1.86  -3.13  0.244      0.610
#> 2          1 centroid       1     1     2 0.978 -2.13  1.62       0.635
#> 3          1 centroid       1     1     3 1.51  -1.61  0.112      0.909
#> 4          1 centroid       1     1     4 1.08   1.04 -0.134      0.734
#> 5          1 centroid       1     1     5 0.931  2.95 -1.91       0.868
```
