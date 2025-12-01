# Cartesian radius (ρ) from coordinates

Computes the Euclidean distance from the origin to a point in either 2‑D
(`z` omitted) or 3‑D space.

## Usage

``` r
cartesian_to_rho(x, y, z = NULL)
```

## Arguments

- x:

  numeric vector of x‑coordinates

- y:

  numeric vector of y‑coordinates

- z:

  optional numeric vector of z‑coordinates; if `NULL` a 2‑D radius is
  returned

## Value

numeric vector of radii (ρ)
