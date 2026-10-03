# Turn a declared orientation by the rotation applied to the positions

In 2D, `yaw` is measured from `x` toward `y`, the sense a rotation about
`z` turns in, so the rotation's angle is added to it. In 3D, the
quaternion expresses the body's axes in the frame's coordinates, so a
rotation of the frame's coordinates pre-multiplies it,
`quat_multiply(r, q)`. The centre of rotation plays no part; orientation
is a direction, not a place.

## Usage

``` r
rotate_orientation(rows, turns, data)
```

## Arguments

- rows:

  A data frame holding the orientation columns.

- turns:

  A list, one element per row of `rows`: the rotation as
  [`turn_of()`](https://animovement.dev/anispace/reference/turn_of.md)
  gives it, or `NULL` to leave the row as it is.

- data:

  The aniframe the orientation is declared on.

## Value

`rows`, with the orientation columns turned.
