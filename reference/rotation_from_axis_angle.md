# A rotation matrix from an axis and an angle

Rodrigues' formula. For a unit axis `k` and angle `theta`,
`R = I + sin(theta) K + (1 - cos(theta)) K^2`, where `K` is the
cross-product matrix of `k`.

## Usage

``` r
rotation_from_axis_angle(axis, angle)
```

## Arguments

- axis:

  Numeric vector of length 3. Need not be a unit vector.

- angle:

  Rotation angle in radians, counter-clockwise about `axis`.

## Value

A 3x3 rotation matrix.
