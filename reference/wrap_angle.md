# Constrain angles to a standard range

Wraps any numeric vector of angles (in radians) to a standard interval
using modulo arithmetic.

## Usage

``` r
wrap_angle(x, modulo = c("2pi", "pi", "asis"))
```

## Arguments

- x:

  Numeric vector of angles (radians).

- modulo:

  Character string specifying the target range:

  `"2pi"`

  :   Wrap to \[0, 2π) (default)

  `"pi"`

  :   Wrap to (-π, π\]

  `"asis"`

  :   No wrapping, return unchanged

## Value

Numeric vector of the same length as `x`, with angles wrapped to the
specified range.

## Examples

``` r
angles <- c(-pi, 0, pi, 2 * pi, 3 * pi)

# Wrap to [0, 2π)
wrap_angle(angles, "2pi")
#> [1] 3.141593 0.000000 3.141593 0.000000 3.141593

# Wrap to (-π, π]
wrap_angle(angles, "pi")
#> [1] 3.141593 0.000000 3.141593 0.000000 3.141593

# No wrapping
wrap_angle(angles, "asis")
#> [1] -3.141593  0.000000  3.141593  6.283185  9.424778
```
