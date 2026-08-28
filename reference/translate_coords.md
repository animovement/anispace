# Translate coordinates in Cartesian space

Moves the origin, either to a fixed point, to a per-observation
position, or onto a keypoint. Translating onto a keypoint is how
coordinates are made relative to the animal rather than the arena.

## Usage

``` r
translate_coords(data, to_x = 0, to_y = 0, to_z = NULL, to_keypoint = NULL)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

- to_x:

  A numeric x-coordinate: either one value, or one per time point.

- to_y:

  A numeric y-coordinate: either one value, or one per time point.

- to_z:

  A numeric z-coordinate, for three-dimensional data: either one value,
  or one per time point.

- to_keypoint:

  A character string naming a keypoint to place at the origin. All other
  coordinates become relative to it.

## Value

An aniframe with translated coordinates.

## See also

Other coordinate transforms:
[`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md),
[`transform_to_egocentric()`](https://animovement.dev/anispace/reference/transform_to_egocentric.md)

## Examples

``` r
af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

# Everything becomes relative to the head, which sits at the origin
translate_coords(af, to_keypoint = "head")
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint       session trial  time      x      y confidence
#>        <int> <fct>            <int> <int> <int>  <dbl>  <dbl>      <dbl>
#> 1          1 head                 1     1     1  0      0          0.700
#> 2          1 head                 1     1     2  0      0          0.746
#> 3          1 head                 1     1     3  0      0          0.853
#> 4          1 neck                 1     1     1 -0.159  1.63       0.226
#> 5          1 neck                 1     1     2 -1.79  -2.67       0.534
#> 6          1 neck                 1     1     3  0.373 -0.987      0.476
#> 7          1 shoulder_right       1     1     1  0.291  2.56       0.379
#> 8          1 shoulder_right       1     1     2 -1.13  -0.855      0.890
#> 9          1 shoulder_right       1     1     3  0.813 -0.391      0.599
```
