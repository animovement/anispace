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
