# Transform coordinates to an egocentric reference frame

Places the animal at the centre of its own coordinate system, by
translating all coordinates onto a reference keypoint and then rotating
so that two chosen keypoints define the forward axis. Positions then
describe the animal's own geometry rather than where it happened to be
in the arena, which is what makes poses comparable across frames and
individuals.

## Usage

``` r
transform_to_egocentric(
  data,
  to_keypoint,
  alignment_points,
  align_perpendicular = FALSE
)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

- to_keypoint:

  A character string naming the keypoint to place at the origin.

- alignment_points:

  A character vector of length 2 naming the keypoints that define the
  axis.

- align_perpendicular:

  A logical value (default `FALSE`) determining the axis of alignment.
  `FALSE` aligns `alignment_points` with the forward axis; `TRUE`
  rotates them perpendicular to it.

## Value

An aniframe with translated and rotated coordinates, in which
`to_keypoint` sits at the origin.

## See also

[`translate_coords()`](https://animovement.dev/anispace/reference/translate_coords.md)
and
[`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md),
which this combines.

Other coordinate transforms:
[`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md),
[`translate_coords()`](https://animovement.dev/anispace/reference/translate_coords.md)

## Examples

``` r
af <- aniframe::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

# The head becomes the origin, and the head-neck axis points forward
transform_to_egocentric(
  af,
  to_keypoint = "head",
  alignment_points = c("head", "neck")
)
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint       session trial  time     x         y confidence
#>        <int> <fct>            <int> <int> <int> <dbl>     <dbl>      <dbl>
#> 1          1 head                 1     1     1 0      0             0.919
#> 2          1 head                 1     1     2 0      0             0.659
#> 3          1 head                 1     1     3 0      0             0.795
#> 4          1 neck                 1     1     1 0.501 -2.78e-17      0.585
#> 5          1 neck                 1     1     2 1.39   1.67e-16      0.621
#> 6          1 neck                 1     1     3 1.23   0             0.942
#> 7          1 shoulder_right       1     1     1 1.42   3.56e- 1      0.661
#> 8          1 shoulder_right       1     1     2 2.19   1.42e+ 0      0.450
#> 9          1 shoulder_right       1     1     3 1.50  -4.54e- 1      0.186

# Aligning perpendicular instead puts that axis across the forward direction
transform_to_egocentric(
  af,
  to_keypoint = "head",
  alignment_points = c("head", "neck"),
  align_perpendicular = TRUE
)
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint       session trial  time         x     y confidence
#>        <int> <fct>            <int> <int> <int>     <dbl> <dbl>      <dbl>
#> 1          1 head                 1     1     1  0        0          0.919
#> 2          1 head                 1     1     2  0        0          0.659
#> 3          1 head                 1     1     3  0        0          0.795
#> 4          1 neck                 1     1     1 -2.78e-17 0.501      0.585
#> 5          1 neck                 1     1     2 -5.55e-17 1.39       0.621
#> 6          1 neck                 1     1     3  0        1.23       0.942
#> 7          1 shoulder_right       1     1     1 -3.56e- 1 1.42       0.661
#> 8          1 shoulder_right       1     1     2 -1.42e+ 0 2.19       0.450
#> 9          1 shoulder_right       1     1     3  4.54e- 1 1.50       0.186
```
