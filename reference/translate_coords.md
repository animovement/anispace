# Translate coordinates in Cartesian space

Moves the origin: to a fixed offset, or onto a member of the frame's
identity — a keypoint, an animal, or whatever level the frame declares.
Translating onto a member is how coordinates are made relative to the
subject rather than the arena.

## Usage

``` r
translate_coords(data, to = NULL, level = NULL, by = NULL)
```

## Arguments

- data:

  An aniframe in a Cartesian coordinate system.

- to:

  A value of `level` to place at the origin. All other coordinates
  become relative to it.

- level:

  The identity variable `to` is a member of. Defaults to the frame's
  only one; a frame declaring several has to be told.

- by:

  Named numeric giving a fixed offset per axis role, e.g.
  `c(x = 100, y = 50)`. The offset moves the *origin*, as `to` does, so
  the coordinates shift by the negative of it: `c(x = 100)` moves every
  point 100 to the left. Mutually exclusive with `to`.

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
translate_coords(af, to = "head", level = "keypoint")
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

# Or move the origin by a fixed amount, which shifts the coordinates the
# other way: x becomes x - 100
translate_coords(af, by = c(x = 100, y = 50))
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>   individual keypoint       session trial  time      x     y confidence
#>        <int> <fct>            <int> <int> <int>  <dbl> <dbl>      <dbl>
#> 1          1 head                 1     1     1 -101.  -51.5      0.700
#> 2          1 head                 1     1     2  -99.2 -48.5      0.746
#> 3          1 head                 1     1     3  -99.9 -49.6      0.853
#> 4          1 neck                 1     1     1 -101.  -49.9      0.226
#> 5          1 neck                 1     1     2 -101.  -51.1      0.534
#> 6          1 neck                 1     1     3  -99.5 -50.6      0.476
#> 7          1 shoulder_right       1     1     1 -101.  -48.9      0.379
#> 8          1 shoulder_right       1     1     2 -100.  -49.3      0.890
#> 9          1 shoulder_right       1     1     3  -99.1 -50.0      0.599
```
