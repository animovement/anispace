# Pad a coordinate to three dimensions

2D is the case where the third component is zero and the rotation axis
is fixed to z, so both dimensionalities go through the same matrices.

## Usage

``` r
pad3(v, n)
```

## Arguments

- v:

  A numeric vector.

- n:

  How many dimensions it came from.

## Value

A numeric vector of length 3.
