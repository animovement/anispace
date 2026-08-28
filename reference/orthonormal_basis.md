# An orthonormal basis from two vectors

Gram-Schmidt: the first vector normalised, the second with its
projection onto the first removed, and their cross product.

## Usage

``` r
orthonormal_basis(primary, secondary)
```

## Arguments

- primary, secondary:

  Numeric vectors of length 3.

## Value

A 3x3 matrix whose columns are the basis, or `NULL` when the two vectors
are parallel and so span no plane.
