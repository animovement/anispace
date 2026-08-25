# Differences between successive angles

Computes lagged differences between successive angles, converting each
raw subtraction into the shortest signed angular distance with
[`calculate_angular_difference()`](https://animovement.dev/anispace/reference/calculate_angular_difference.md).
The output mirrors [`base::diff()`](https://rdrr.io/r/base/diff.html),
returning `NA` for the first `lag` positions so it can be used inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

## Usage

``` r
diff_angle(x, lag = 1L)
```

## Arguments

- x:

  A numeric vector of angles, in radians.

- lag:

  A positive integer (default `1L`) giving the lag to difference at.

## Value

A numeric vector the same length as `x`. The first `lag` entries are
`NA`; the rest are angular differences in radians.

## See also

Other angle utilities:
[`calculate_angular_difference()`](https://animovement.dev/anispace/reference/calculate_angular_difference.md),
[`unwrap_angle()`](https://animovement.dev/anispace/reference/unwrap_angle.md),
[`wrap_angle()`](https://animovement.dev/anispace/reference/wrap_angle.md)

## Examples

``` r
angles <- c(0, pi / 2, pi, 3 * pi / 2)
diff_angle(angles)
#> [1]       NA 1.570796 1.570796 1.570796

# A larger lag compares points further apart
diff_angle(angles, lag = 2L)
#> [1]       NA       NA 3.141593 3.141593
```
