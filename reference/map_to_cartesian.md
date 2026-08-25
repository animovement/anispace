# Map to Cartesian coordinates

Converts an aniframe back to Cartesian coordinates, detecting whether it
is currently polar, cylindrical or spherical.

## Usage

``` r
map_to_cartesian(data)
```

## Arguments

- data:

  An aniframe in a polar, cylindrical or spherical coordinate system.

## Value

An aniframe with `x` and `y` (and `z`, where the input was
three-dimensional) in place of the polar columns.

## See also

Other coordinate systems:
[`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md),
[`map_to_polar()`](https://animovement.dev/anispace/reference/map_to_polar.md),
[`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md)

## Examples

``` r
af <- aniframe::example_aniframe(n_obs = 5, n_individuals = 1, n_keypoints = 1)

# Round-trips back to the coordinates it started from
map_to_cartesian(map_to_polar(af))
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint session trial  time        x      y confidence
#>        <int> <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#> 1          1 centroid       1     1     1 -1.40    -0.554      0.428
#> 2          1 centroid       1     1     2  0.255    0.629      0.952
#> 3          1 centroid       1     1     3 -2.44     2.07       0.762
#> 4          1 centroid       1     1     4 -0.00557 -1.63       0.761
#> 5          1 centroid       1     1     5  0.622    0.512      0.768
```
