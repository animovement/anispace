# Difference of angular values

Computes lagged differences between successive angles (in radians) and
converts each raw subtraction into the shortest signed angular distance
using
[`calculate_angular_difference()`](http://animovement.dev/anispace/reference/calculate_angular_difference.md).
The output mimics [`base::diff()`](https://rdrr.io/r/base/diff.html),
returning `NA`s for the first `lag` positions so it works nicely inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
diff_angle(x, lag = 1L)
```

## Arguments

- x:

  Numeric vector of angles (radians).

- lag:

  Positive integer indicating the lag (default = 1L). Must be an integer
  ≥ 1.

## Value

Numeric vector of the same length as `x`. The first `lag` entries are
`NA`; subsequent entries contain the angular differences.

## Examples

``` r
# Simple example
angles <- c(0, pi/2, pi, 3*pi/2)
diff_angle(angles)
#> [1]       NA 1.570796 1.570796 1.570796

# Using a lag of 2
diff_angle(angles, lag = 2L)
#> [1]       NA       NA 3.141593 3.141593
```
