# Package index

## Transformations

These functions allow you to make tranformations to your coordinate
system, such as translations, rotations or conversion to polar
coordinates.

- [`transform_to_egocentric()`](https://animovement.dev/anispace/reference/transform_to_egocentric.md)
  : Transform coordinates to egocentric reference frame
- [`translate_coords()`](https://animovement.dev/anispace/reference/translate_coords.md)
  : Translate coordinates (Cartesian)
- [`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md)
  : Rotate coordinates in Cartesian space (2D or 3D)
- [`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md)
  : Map from polar to Cartesian coordinates
- [`map_to_polar()`](https://animovement.dev/anispace/reference/map_to_polar.md)
  : Map from Cartesian to polar coordinates
- [`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md)
  : Map from Cartesian to cylindrical coordinates
- [`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md)
  : Map from Cartesian to spherical coordinates
- [`cartesian_to_rho()`](https://animovement.dev/anispace/reference/cartesian_to_rho.md)
  : Cartesian radius (ρ) from coordinates
- [`cartesian_to_phi()`](https://animovement.dev/anispace/reference/cartesian_to_phi.md)
  : Cartesian azimuth (φ) from coordinates
- [`cartesian_to_theta()`](https://animovement.dev/anispace/reference/cartesian_to_theta.md)
  : Polar angle (θ) from Cartesian coordinates
- [`polar_to_x()`](https://animovement.dev/anispace/reference/polar_to_x.md)
  : Convert polar radius to Cartesian x‑coordinate
- [`polar_to_y()`](https://animovement.dev/anispace/reference/polar_to_y.md)
  : Convert polar radius to Cartesian y‑coordinate
- [`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)
  : Convert cylindrical radius and polar angle to Cartesian z‑coordinate

## Helpers

- [`calculate_angular_difference()`](https://animovement.dev/anispace/reference/calculate_angular_difference.md)
  : Calculate angular difference
- [`wrap_angle()`](https://animovement.dev/anispace/reference/wrap_angle.md)
  : Constrain angles to a standard range
- [`unwrap_angle()`](https://animovement.dev/anispace/reference/unwrap_angle.md)
  : Remove constrain for angles to keep within \[0, 2π)
- [`diff_angle()`](https://animovement.dev/anispace/reference/diff_angle.md)
  : Difference of angular values
