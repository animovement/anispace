# Remove wrapping from a sequence of angles

Reverses the discontinuity introduced by wrapping, by accumulating the
shortest step between successive angles. A heading that crosses `2*pi`
therefore continues to increase rather than jumping back to zero, which
is what makes it differentiable. `NA` values are preserved in place.

## Usage

``` r
unwrap_angle(x)
```

## Arguments

- x:

  A numeric vector of angles, in radians.

## Value

A numeric vector the same length as `x`, without wrapping
discontinuities.

## See also

Other angle utilities:
[`calculate_angular_difference()`](https://animovement.dev/anispace/reference/calculate_angular_difference.md),
[`diff_angle()`](https://animovement.dev/anispace/reference/diff_angle.md),
[`wrap_angle()`](https://animovement.dev/anispace/reference/wrap_angle.md)

## Examples

``` r
# A heading turning steadily past a full circle, wrapped to [0, 2*pi)
wrapped <- wrap_angle(seq(0, 3 * pi, length.out = 7), "2pi")
wrapped
#> [1] 0.000000 1.570796 3.141593 4.712389 0.000000 1.570796 3.141593

# Unwrapping restores the steady progression
unwrap_angle(wrapped)
#> [1] 0.000000 1.570796 3.141593 4.712389 6.283185 7.853982 9.424778
```
