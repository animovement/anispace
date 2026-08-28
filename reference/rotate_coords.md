# Rotate coordinates in Cartesian space

Rotates coordinates so that two chosen keypoints define the axis,
detecting whether the data are two- or three-dimensional.

## Usage

``` r
rotate_coords(data, alignment_points, align_perpendicular = FALSE)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

- alignment_points:

  A character vector of length 2 naming the keypoints used for
  alignment.

- align_perpendicular:

  A logical value (default `FALSE`) determining the axis of alignment.
  `FALSE` aligns `alignment_points` with the x-axis; `TRUE` rotates them
  perpendicular to it.

## Value

An aniframe with rotated `x` and `y` (and `z`, where present).

## See also

Other coordinate transforms:
[`transform_to_egocentric()`](https://animovement.dev/anispace/reference/transform_to_egocentric.md),
[`translate_coords()`](https://animovement.dev/anispace/reference/translate_coords.md)

## Examples

``` r
af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)
rotate_coords(af, alignment_points = c("head", "neck"))
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint       session trial  time      x      y confidence
#>        <int> <fct>            <int> <int> <int>  <dbl>  <dbl>      <dbl>
#> 1          1 head                 1     1     1  0.205  0.125      0.589
#> 2          1 head                 1     1     2 -1.11  -0.867      0.812
#> 3          1 head                 1     1     3 -1.56  -0.803      0.439
#> 4          1 neck                 1     1     1  0.691  0.125      0.745
#> 5          1 neck                 1     1     2  0.395 -0.867      0.619
#> 6          1 neck                 1     1     3 -1.06  -0.803      0.785
#> 7          1 shoulder_right       1     1     1  0.124 -0.284      0.621
#> 8          1 shoulder_right       1     1     2 -1.95  -0.222      0.453
#> 9          1 shoulder_right       1     1     3 -0.354  1.48       0.457
```
