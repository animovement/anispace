# The rotation taking one frame of reference onto another

Three points fix an orientation outright: the first vector gives the
primary axis, and the second, made perpendicular to it, gives the plane.
Both bases are orthonormal, so the rotation between them is
`B %*% t(A)`.

## Usage

``` r
rotation_onto_basis(primary, secondary, to_primary, to_secondary)
```

## Arguments

- primary, secondary:

  Numeric vectors of length 3 spanning the source.

- to_primary, to_secondary:

  The target axes they should land on.

## Value

A 3x3 rotation matrix.
