# Convert cylindrical radius and polar angle to Cartesian z‑coordinate

Handles regular points as well as the two pole regions (θ≈0 and θ≈π).
Non‑finite inputs remain `NA`.

## Usage

``` r
spherical_to_z(rho, theta)
```

## Arguments

- rho:

  Numeric vector – cylindrical radius (√(x² + y²)).

- theta:

  Numeric vector – polar angle measured from the +z axis (radians).

## Value

Numeric vector of z‑coordinates (same length as input)
