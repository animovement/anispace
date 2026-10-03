# A rotation as it turns an orientation

A rotation as it turns an orientation

## Usage

``` r
turn_of(rotation, n_axes)
```

## Arguments

- rotation:

  A 3x3 rotation matrix, or `NULL`.

- n_axes:

  How many spatial axes the frame has.

## Value

`NULL` for `NULL`. In 2D, the angle in radians it turns about `z`; in
3D, its quaternion.
