# Changelog

## anispace (development version)

## anispace 0.2.0 (2026-08-18)

First tagged release. anispace has been usable for a while but was never
tagged, so this release marks the current state rather than a change in
it, and starts a changelog for what follows.

### Contents

- Coordinate-system conversion:
  [`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md),
  [`map_to_polar()`](https://animovement.dev/anispace/reference/map_to_polar.md),
  [`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md)
  and
  [`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md),
  with the element-wise helpers behind them
  ([`cartesian_to_rho()`](https://animovement.dev/anispace/reference/cartesian_to_rho.md),
  [`polar_to_x()`](https://animovement.dev/anispace/reference/polar_to_x.md)
  and friends).
- Angular arithmetic:
  [`wrap_angle()`](https://animovement.dev/anispace/reference/wrap_angle.md),
  [`unwrap_angle()`](https://animovement.dev/anispace/reference/unwrap_angle.md),
  [`diff_angle()`](https://animovement.dev/anispace/reference/diff_angle.md),
  [`calculate_angular_difference()`](https://animovement.dev/anispace/reference/calculate_angular_difference.md).
- [`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md)
  for rotating coordinates in the plane.

### Since 0.1.3

- [`unwrap_angle()`](https://animovement.dev/anispace/reference/unwrap_angle.md)
  handles `NA` correctly.
- CI installs binary packages rather than compiling every dependency
  from source, and R-devel runs on merges to main rather than on every
  pull request ([\#8](https://github.com/animovement/anispace/issues/8),
  [\#9](https://github.com/animovement/anispace/issues/9)).
