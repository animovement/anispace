# Rotate coordinates in Cartesian space

Rotates each subject's coordinates so that chosen members of its
identity define the axes. Two members give a direction; in three
dimensions a third fixes the roll about it, which two cannot.

## Usage

``` r
rotate_coords(
  data,
  align,
  level = NULL,
  about = NULL,
  align_perpendicular = FALSE
)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

- align:

  Two or three values of `level`. The first two define the primary axis.
  A third, in 3D, defines the plane and so the orientation outright.

- level:

  The identity variable `align` names members of. Defaults to the
  frame's only one; a frame declaring several has to be told.

- about:

  Centre of rotation: a value of `level` to rotate around, or a named
  numeric such as `c(x = 500, y = 500)`. Defaults to the coordinate
  origin, which is what to rotate about once the frame has been
  translated onto its subject.

- align_perpendicular:

  Put the primary axis across the target rather than along it.

## Value

An aniframe with rotated coordinates.

## See also

Other coordinate transforms:
[`transform_to_egocentric()`](https://animovement.dev/anispace/reference/transform_to_egocentric.md),
[`translate_coords()`](https://animovement.dev/anispace/reference/translate_coords.md)

## Examples

``` r
af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

# Align the head-neck axis with x, rotating about the origin
rotate_coords(af, align = c("head", "neck"), level = "keypoint")
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

# Rotate each animal about its own head instead
rotate_coords(af, align = c("head", "neck"), level = "keypoint", about = "head")
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint       session trial  time     x       y confidence
#>        <int> <fct>            <int> <int> <int> <dbl>   <dbl>      <dbl>
#> 1          1 head                 1     1     1 0.237 -0.0381      0.589
#> 2          1 head                 1     1     2 1.32   0.486       0.812
#> 3          1 head                 1     1     3 0.524  1.67        0.439
#> 4          1 neck                 1     1     1 0.723 -0.0381      0.745
#> 5          1 neck                 1     1     2 2.82   0.486       0.619
#> 6          1 neck                 1     1     3 1.02   1.67        0.785
#> 7          1 shoulder_right       1     1     1 0.156 -0.447       0.621
#> 8          1 shoulder_right       1     1     2 0.474  1.13        0.453
#> 9          1 shoulder_right       1     1     3 1.73   3.95        0.457
```
