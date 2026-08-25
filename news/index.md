# Changelog

## anispace (development version)

### Bug fixes

- [`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md)
  now returns the radial distance from the origin as `rho`, rather than
  the cylindrical radius — the distance from the z-axis
  ([\#19](https://github.com/animovement/anispace/issues/19)). `theta`
  already used the full radius, so the triple was internally
  inconsistent with the name “spherical”; ISO 80000-2 and the usual
  physics convention both use the radial distance.

  This was **lossy**, not merely non-standard. A point on the z-axis has
  a cylindrical radius of zero, so its height could not be recovered:
  `(0, 0, 5)` round-tripped through
  [`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md)
  to the origin. It now returns `(0, 0, 5)`.

  [`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md)
  and
  [`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)
  follow the same convention, so round trips are unaffected for points
  away from the axis. **Code that read `rho` from a spherical frame, or
  called
  [`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)
  directly, will need updating**: `spherical_to_z(rho, theta)` is now
  `rho * cos(theta)` where it was `rho / tan(theta)`.
  [`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md)
  is unchanged — `rho` there is still the distance from the z-axis,
  which is correct for a cylindrical frame.

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
