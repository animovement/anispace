# anispace (development version)

## Fixed

* `map_to_spherical()` returns the radial distance from the origin as `rho`, rather than the cylindrical radius (#19). `theta` already used the full radius, so the triple was inconsistent with the name; ISO 80000-2 uses the radial distance. This was lossy as well as non-standard — a point on the z-axis has a cylindrical radius of zero, so `(0, 0, 5)` round-tripped through `map_to_cartesian()` to the origin, and now returns `(0, 0, 5)`.

  `spherical_to_z(rho, theta)` is now `rho * cos(theta)` where it was `rho / tan(theta)`. **Code reading `rho` from a spherical frame, or calling `spherical_to_z()` directly, needs updating.** `map_to_cylindrical()` is unchanged: `rho` there is the distance from the z-axis, which is correct for a cylindrical frame.

# anispace 0.2.0 (2026-08-18)

First tagged release. anispace had been usable for a while but was never tagged, so this marks the current state rather than a change in it.

## Added

* Coordinate-system conversion: `map_to_cartesian()`, `map_to_polar()`, `map_to_cylindrical()` and `map_to_spherical()`, with the element-wise helpers behind them — `cartesian_to_rho()`, `cartesian_to_phi()`, `cartesian_to_theta()`, `polar_to_x()`, `polar_to_y()` and `spherical_to_z()`.
* Angular arithmetic: `wrap_angle()`, `unwrap_angle()`, `diff_angle()` and `calculate_angular_difference()`.
* Rigid transformations: `rotate_coords()`, `translate_coords()` and `transform_to_egocentric()`.

## Changed

* CI installs binary packages rather than compiling every dependency from source, and R-devel runs on merges to `main` rather than on every pull request (#8, #9).

# anispace 0.1.3

## Fixed

* `unwrap_angle()` handles `NA` correctly.

# anispace 0.1.2

## Fixed

* Corrected the expected Cartesian values in the conversion tests.

# anispace 0.1.1

## Removed

* `deg_to_rad()` and `rad_to_deg()`. Unit conversion belongs to aniframe, which owns `unit_angle`; anispace converts between coordinate systems.

# anispace 0.1.0

First commit. anispace holds the spatial transformations that moved out of aniframe in its 0.3.0: converting between coordinate systems, and rotating, translating and re-centring coordinates within one.
