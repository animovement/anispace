# Cartesian azimuth (φ) from coordinates

Returns the planar angle measured from the positive x‑axis toward the
positive y‑axis. By default the result is mapped to \[0, 2π); setting
`centered = TRUE` leaves the native `atan2` range \[-π, π\].

## Usage

``` r
cartesian_to_phi(x, y, centered = FALSE)
```

## Arguments

- x:

  numeric vector of x‑coordinates

- y:

  numeric vector of y‑coordinates

- centered:

  logical; if `TRUE` keep the \[-π, π\] range, otherwise map to
  \[0, 2π\\

## Value

numeric vector of azimuth angles (φ) in radians
