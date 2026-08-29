# Transform coordinates to an egocentric reference frame

Places the subject at the centre of its own coordinate system:
translating onto a reference member, and then, if alignment points are
given, rotating so they define the axes. Positions then describe the
subject's own geometry rather than where it happened to be in the arena,
which is what makes poses comparable across moments and individuals.

Translation alone re-centres without changing orientation. Rotation
alone is
[`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md),
which turns the frame about the coordinate origin rather than about the
subject.

## Usage

``` r
transform_to_egocentric(
  data,
  to,
  align = NULL,
  level = NULL,
  align_perpendicular = FALSE
)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

- to:

  A value of `level` to place at the origin.

- align:

  Optionally, two or three values of `level` defining the axes. Two give
  a direction; in 3D a third fixes the roll about it. Omitted, the frame
  is re-centred and left as it was oriented.

- level:

  The identity variable `to` and `align` name members of. Defaults to
  the frame's only one; a frame declaring several has to be told.

- align_perpendicular:

  Put the primary axis across the target rather than along it.

## Value

An aniframe centred on `to`, with `reference_frame` set to
`"egocentric"`.

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
af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

# The head becomes the origin, and the head-neck axis points forward
transform_to_egocentric(
  af,
  to = "head",
  align = c("head", "neck"),
  level = "keypoint"
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
#> 4          1 neck                 1     1     1 0.501 -1.49e-17      0.585
#> 5          1 neck                 1     1     2 1.39  -1.75e-16      0.621
#> 6          1 neck                 1     1     3 1.23  -1.14e-17      0.942
#> 7          1 shoulder_right       1     1     1 1.42   3.56e- 1      0.661
#> 8          1 shoulder_right       1     1     2 2.19   1.42e+ 0      0.450
#> 9          1 shoulder_right       1     1     3 1.50  -4.54e- 1      0.186

# Re-centre without reorienting
transform_to_egocentric(af, to = "head", level = "keypoint")
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint       session trial  time     x      y confidence
#>        <int> <fct>            <int> <int> <int> <dbl>  <dbl>      <dbl>
#> 1          1 head                 1     1     1 0      0          0.919
#> 2          1 head                 1     1     2 0      0          0.659
#> 3          1 head                 1     1     3 0      0          0.795
#> 4          1 neck                 1     1     1 0.366 -0.342      0.585
#> 5          1 neck                 1     1     2 0.237 -1.37       0.621
#> 6          1 neck                 1     1     3 1.22   0.177      0.942
#> 7          1 shoulder_right       1     1     1 1.28  -0.712      0.661
#> 8          1 shoulder_right       1     1     2 1.77  -1.92       0.450
#> 9          1 shoulder_right       1     1     3 1.55  -0.234      0.186
```
