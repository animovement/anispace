# Package index

## Transformations

These functions allow you to make tranformations to your coordinate
system, such as translations, rotations or conversion to polar
coordinates.

- [`transform_to_egocentric()`](https://animovement.dev/anispace/reference/transform_to_egocentric.md)
  : Transform coordinates to an egocentric reference frame
- [`translate_coords()`](https://animovement.dev/anispace/reference/translate_coords.md)
  : Translate coordinates in Cartesian space
- [`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md)
  : Rotate coordinates in Cartesian space
- [`map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.md)
  : Map to Cartesian coordinates
- [`map_to_polar()`](https://animovement.dev/anispace/reference/map_to_polar.md)
  : Map from Cartesian to polar coordinates
- [`map_to_cylindrical()`](https://animovement.dev/anispace/reference/map_to_cylindrical.md)
  : Map from Cartesian to cylindrical coordinates
- [`map_to_spherical()`](https://animovement.dev/anispace/reference/map_to_spherical.md)
  : Map from Cartesian to spherical coordinates
- [`cartesian_to_rho()`](https://animovement.dev/anispace/reference/cartesian_to_rho.md)
  : Radius (rho) from Cartesian coordinates
- [`cartesian_to_phi()`](https://animovement.dev/anispace/reference/cartesian_to_phi.md)
  : Azimuth (phi) from Cartesian coordinates
- [`cartesian_to_theta()`](https://animovement.dev/anispace/reference/cartesian_to_theta.md)
  : Inclination (theta) from Cartesian coordinates
- [`polar_to_x()`](https://animovement.dev/anispace/reference/polar_to_x.md)
  : Cartesian x-coordinate from polar coordinates
- [`polar_to_y()`](https://animovement.dev/anispace/reference/polar_to_y.md)
  : Cartesian y-coordinate from polar coordinates
- [`spherical_to_z()`](https://animovement.dev/anispace/reference/spherical_to_z.md)
  : Cartesian z-coordinate from spherical coordinates

## Rotations

Unit quaternions (Hamilton, scalar first) for 3D orientation: algebra,
conversion to and from axis-angle, matrices and Euler angles, and
interpolation, averaging and angular velocity.

- [`quat_multiply()`](https://animovement.dev/anispace/reference/quaternions.md)
  [`quat_conjugate()`](https://animovement.dev/anispace/reference/quaternions.md)
  [`quat_normalise()`](https://animovement.dev/anispace/reference/quaternions.md)
  [`quat_rotate()`](https://animovement.dev/anispace/reference/quaternions.md)
  [`quat_distance()`](https://animovement.dev/anispace/reference/quaternions.md)
  : Quaternion algebra
- [`quat_from_axis_angle()`](https://animovement.dev/anispace/reference/quat_from_axis_angle.md)
  [`quat_to_axis_angle()`](https://animovement.dev/anispace/reference/quat_from_axis_angle.md)
  [`quat_from_matrix()`](https://animovement.dev/anispace/reference/quat_from_axis_angle.md)
  [`quat_to_matrix()`](https://animovement.dev/anispace/reference/quat_from_axis_angle.md)
  : Quaternions from and to other rotation representations
- [`quat_from_euler()`](https://animovement.dev/anispace/reference/quat_from_euler.md)
  [`quat_to_euler()`](https://animovement.dev/anispace/reference/quat_from_euler.md)
  : Quaternions from and to Euler angles
- [`quat_slerp()`](https://animovement.dev/anispace/reference/quat_slerp.md)
  [`quat_mean()`](https://animovement.dev/anispace/reference/quat_slerp.md)
  [`quat_continuous()`](https://animovement.dev/anispace/reference/quat_slerp.md)
  [`quat_angular_velocity()`](https://animovement.dev/anispace/reference/quat_slerp.md)
  : Interpolate, average and differentiate rotations
- [`transform_euler_to_quaternion()`](https://animovement.dev/anispace/reference/transform_euler_to_quaternion.md)
  [`transform_quaternion_to_euler()`](https://animovement.dev/anispace/reference/transform_euler_to_quaternion.md)
  : Convert orientation between Euler angles and quaternions
