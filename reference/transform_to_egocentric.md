# Transform coordinates to an egocentric reference frame

Places the subject at the centre of its own coordinate system:
translating onto a reference member, and then, if asked, rotating it to
face a fixed direction. Positions then describe the subject's own
geometry rather than where it happened to be in the arena, which is what
makes poses comparable across moments and individuals.

The rotation comes from one of two places. Alignment points are members
whose positions define the axes. Alternatively, `align = "orientation"`
uses the subject's declared orientation (`where$orientation`), for data
that has one – FicTrac, rigid-body motion capture, a centroid with a
heading – whether or not it also has keypoints to align on.

Translation alone re-centres without changing orientation. Rotation
alone is
[`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md),
which turns the frame about the coordinate origin rather than about the
subject. A declared orientation is turned with the positions, as
[`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md)
describes.

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

  Optionally, how to rotate. Two or three values of `level` define the
  axes: two give a direction, and in 3D a third fixes the roll about it.
  **\[experimental\]** `"orientation"` turns each subject to face `+x`
  by its declared orientation; see below. This option is experimental,
  and may change without a deprecation cycle. Omitted, the frame is
  re-centred and left as it was oriented.

- level:

  The identity variable `to` and `align` name members of. Defaults to
  the frame's only one; a frame declaring several has to be told.

- align_perpendicular:

  Put the primary axis across the target rather than along it.

## Value

An aniframe centred on `to`, with `reference_frame` set to
`"egocentric"`.

## Aligning by orientation

Each subject at each moment is turned by the inverse of its own
orientation, so that it faces `+x`. In 2D that is a rotation by `-yaw`,
after which its `yaw` is 0. In 3D it is the inverse of its quaternion,
after which its body axes lie along the coordinate axes and its
orientation is the identity, `(1, 0, 0, 0)`. With `align_perpendicular`,
it faces across instead, as for alignment points.

Orientation is declared per row, so the subject's is read from its `to`
member: the member it is centred on is the one it is turned to face
with. Any other member's orientation is turned with it, and so ends up
relative to the `to` member's. To centre on one member and face by
another's orientation, align on the second and then
[`translate_coords()`](https://animovement.dev/anispace/reference/translate_coords.md)
onto the first; translating changes no orientation.

A moment whose `to` member has no orientation cannot be aligned, and its
positions and orientation come back `NA` rather than unrotated.

A single value can never be alignment points, which come in twos and
threes, so `"orientation"` is unambiguous even when `level` has a member
of that name: `c("orientation", "head")` aligns on that member.

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
af <- anicore::example_anipoint(n_obs = 3, n_individuals = 1, n_keypoints = 3)

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

# Face the way the declared heading says, whatever the keypoints do
heading <- anicore::as_anipoint(data.frame(
  time = rep(1:2, each = 2),
  keypoint = c("centroid", "head"),
  x = c(0, 1, 5, 5),
  y = c(0, 1, 5, 6),
  yaw = rep(c(pi / 4, pi / 2), each = 2)
)) |>
  anicore::set_variables(where = list(
    position = c(x = "x", y = "y"),
    orientation = c(yaw = "yaw")
  ))
transform_to_egocentric(heading, to = "centroid", align = "orientation")
#> # Keypoints: centroid, head
#>   keypoint  time     x        y   yaw
#>   <fct>    <int> <dbl>    <dbl> <dbl>
#> 1 centroid     1  0    0            0
#> 2 centroid     2  0    0            0
#> 3 head         1  1.41 1.11e-16     0
#> 4 head         2  1    1.11e-16     0
```
