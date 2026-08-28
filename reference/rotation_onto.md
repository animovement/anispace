# The rotation taking one vector onto another

The minimal rotation: about the axis perpendicular to both, through the
angle between them. In 3D two points leave the roll about the target
undetermined, and this is the conventional choice of what to do with it
– nothing.

## Usage

``` r
rotation_onto(from, to)
```

## Arguments

- from, to:

  Numeric vectors of length 3.

## Value

A 3x3 rotation matrix.
