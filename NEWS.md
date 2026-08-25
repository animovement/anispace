# anispace (development version)

## Bug fixes

* `map_to_spherical()` now returns the radial distance from the origin as `rho`, rather than the cylindrical radius — the distance from the z-axis (#19). `theta` already used the full radius, so the triple was internally inconsistent with the name "spherical"; ISO 80000-2 and the usual physics convention both use the radial distance.

  This was **lossy**, not merely non-standard. A point on the z-axis has a cylindrical radius of zero, so its height could not be recovered: `(0, 0, 5)` round-tripped through `map_to_cartesian()` to the origin. It now returns `(0, 0, 5)`.

  `map_to_cartesian()` and `spherical_to_z()` follow the same convention, so round trips are unaffected for points away from the axis. **Code that read `rho` from a spherical frame, or called `spherical_to_z()` directly, will need updating**: `spherical_to_z(rho, theta)` is now `rho * cos(theta)` where it was `rho / tan(theta)`. `map_to_cylindrical()` is unchanged — `rho` there is still the distance from the z-axis, which is correct for a cylindrical frame.

# anispace (development version)

# anispace 0.2.0 (2026-08-18)

First tagged release. anispace has been usable for a while but was never
tagged, so this release marks the current state rather than a change in
it, and starts a changelog for what follows.

## Contents

* Coordinate-system conversion: `map_to_cartesian()`, `map_to_polar()`,
  `map_to_cylindrical()` and `map_to_spherical()`, with the element-wise
  helpers behind them (`cartesian_to_rho()`, `polar_to_x()` and friends).
* Angular arithmetic: `wrap_angle()`, `unwrap_angle()`, `diff_angle()`,
  `calculate_angular_difference()`.
* `rotate_coords()` for rotating coordinates in the plane.

## Since 0.1.3

* `unwrap_angle()` handles `NA` correctly.
* CI installs binary packages rather than compiling every dependency from
  source, and R-devel runs on merges to main rather than on every pull
  request (#8, #9).
