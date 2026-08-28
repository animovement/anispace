# Changelog

## anispace (development version)

### Changed

- `wrap_angle()` and `unwrap_angle()` move to `anicore`, which already
  held `deg_to_rad()` and `rad_to_deg()` (animovement/aniframe#128).
  They are angle arithmetic rather than coordinate transformation. Use
  [`anicore::wrap_angle()`](https://animovement.dev/anicore/reference/wrap_angle.html).

- The core data structures come from `anicore`, which is what the
  `aniframe` package was renamed to in its 0.8.0
  (animovement/anicore#84). The `aniframe` class keeps its name; only
  the package providing it changed.

### Fixed

- [`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md)
  returns the radial distance from the origin as `rho`, rather than the
  cylindrical radius
  ([\#19](https://github.com/animovement/anispace/issues/19)). `theta`
  already used the full radius, so the triple was inconsistent with the
  name; ISO 80000-2 uses the radial distance. This was lossy as well as
  non-standard — a point on the z-axis has a cylindrical radius of zero,
  so `(0, 0, 5)` round-tripped through
  [`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md)
  to the origin, and now returns `(0, 0, 5)`.

  `spherical_to_z(rho, theta)` is now `rho * cos(theta)` where it was
  `rho / tan(theta)`. **Code reading `rho` from a spherical frame, or
  calling
  [`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)
  directly, needs updating.**
  [`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md)
  is unchanged: `rho` there is the distance from the z-axis, which is
  correct for a cylindrical frame.

## anispace 0.2.0 (2026-08-18)

First tagged release. anispace had been usable for a while but was never
tagged, so this marks the current state rather than a change in it.

### Added

- Coordinate-system conversion:
  [`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md),
  [`map_to_polar()`](https://animovement.dev/anispace/reference/map_to_polar.md),
  [`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md)
  and
  [`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md),
  with the element-wise helpers behind them —
  [`cartesian_to_rho()`](https://animovement.dev/anispace/reference/cartesian_to_rho.md),
  [`cartesian_to_phi()`](https://animovement.dev/anispace/reference/cartesian_to_phi.md),
  [`cartesian_to_theta()`](https://animovement.dev/anispace/reference/cartesian_to_theta.md),
  [`polar_to_x()`](https://animovement.dev/anispace/reference/polar_to_x.md),
  [`polar_to_y()`](https://animovement.dev/anispace/reference/polar_to_y.md)
  and
  [`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md).
- Angular arithmetic: `wrap_angle()`, `unwrap_angle()`,
  [`diff_angle()`](https://animovement.dev/anispace/reference/diff_angle.md)
  and
  [`calculate_angular_difference()`](https://animovement.dev/anispace/reference/calculate_angular_difference.md).
- Rigid transformations:
  [`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md),
  [`translate_coords()`](https://animovement.dev/anispace/reference/translate_coords.md)
  and
  [`transform_to_egocentric()`](https://animovement.dev/anispace/reference/transform_to_egocentric.md).

### Changed

- CI installs binary packages rather than compiling every dependency
  from source, and R-devel runs on merges to `main` rather than on every
  pull request ([\#8](https://github.com/animovement/anispace/issues/8),
  [\#9](https://github.com/animovement/anispace/issues/9)).

## anispace 0.1.3

### Fixed

- `unwrap_angle()` handles `NA` correctly.

## anispace 0.1.2

### Fixed

- Corrected the expected Cartesian values in the conversion tests.

## anispace 0.1.1

### Removed

- `deg_to_rad()` and `rad_to_deg()`. Unit conversion belongs to
  aniframe, which owns `unit_angle`; anispace converts between
  coordinate systems.

## anispace 0.1.0

First commit. anispace holds the spatial transformations that moved out
of aniframe in its 0.3.0: converting between coordinate systems, and
rotating, translating and re-centring coordinates within one.
