# Wrap angles to the range their source used

Signed, `(-pi, pi]`, or unsigned, `[0, 2 pi)`, as
[`anicore::reflect_axis()`](https://animovement.dev/anicore/reference/reflect_axis.html)
decides it. A value within a rounding error of where the range wraps can
land exactly on the end it excludes – `-1e-17` wraps to `2 pi` – so that
end is folded onto the other, which is the same angle.

## Usage

``` r
wrap_like(radians, signed)
```

## Arguments

- radians:

  Numeric vector of angles in radians.

- signed:

  Wrap to `(-pi, pi]` rather than `[0, 2 pi)`.

## Value

`radians`, wrapped.
