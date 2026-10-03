# anispace (development version)

## Added

* `quat_from_vectors(primary, secondary, axes)` builds the orientation whose body axis `axes[1]` points along `primary` and whose `axes[2]` points towards `secondary`, using only the part of `secondary` perpendicular to `primary`. Three points define an orientation this way: one axis from the first point to the second, and roll fixed by any third point off that line. It is the primitive behind animetric's planned `add_orientation()` (animovement/animetric#97). Rows with a missing or zero vector, or parallel vectors, give `NA`.

* `transform_to_egocentric(align = "orientation")` aligns each subject by its own declared orientation (#49). Alignment used to need two or three keypoints defining an axis. Many datasets have none but do record an orientation: FicTrac, rigid-body motion capture, or a centroid with a heading. Where both exist, the measured orientation is often better than a noisy keypoint axis. The frame is centred on `to`, then each subject at each moment is turned by the inverse of its `to` member's orientation, so it faces +x. In 2D that is a rotation by `-yaw`, after which `yaw` is exactly 0. In 3D it is the inverse quaternion, after which the body axes lie along the coordinate axes and the orientation is the identity. `align_perpendicular` faces it across, as for keypoints. A moment whose `to` member has no orientation comes back `NA`, not unrotated. A single value can never be alignment points, so `"orientation"` is unambiguous even when a member has that name.

* Quaternions for 3D orientation (#7): `quat_multiply()`, `quat_conjugate()`, `quat_normalise()`, `quat_rotate()` and `quat_distance()`; conversion with `quat_from_axis_angle()` / `quat_to_axis_angle()`, `quat_from_matrix()` / `quat_to_matrix()` and `quat_from_euler()` / `quat_to_euler()` (all twelve sequences, with `sequence` and `intrinsic` always stated); and `quat_slerp()`, `quat_mean()`, `quat_continuous()` and `quat_angular_velocity()`. `transform_euler_to_quaternion()` turns exported Euler angles into a declared quaternion orientation (animovement/anicore#46), records their convention, and `transform_quaternion_to_euler()` gives them back as a derived view, in the recorded convention unless another is given.

## Changed

* Help pages show each function's lifecycle stage (animovement/.github#46). An unlabelled function is stable, and changes only through a deprecation cycle. Five new interfaces are labelled experimental, so they may still change without one: `transform_euler_to_quaternion()` and `transform_quaternion_to_euler()`, `quat_from_vectors()`, `quat_angular_velocity()`, and `transform_to_egocentric(align = "orientation")`. The rest of the quaternion toolkit, and aligning on members, are stable.

* Works with anicore's `anipoint` class and rebuilt accessor API (animovement/anicore#154). The transforms now require an anipoint, so the error for other input reads "not an anipoint".

## Fixed

* `rotate_coords()`, and through it `transform_to_egocentric()`, rotate a declared orientation along with the positions (#49). They turned the positions and left `yaw` or the quaternion as it was, so the two disagreed: aligning a body on its tail-to-head axis put the head on +x while `yaw` still gave its old heading. The orientation is now turned by the same rotation, per subject and moment. In 2D the rotation's angle is added to `yaw`, in the frame's `unit_angle`, and wrapped to the range the input used — signed if any value is negative, as `anicore::reflect_axis()` decides it. In 3D the quaternion is pre-multiplied by the rotation's, which is the side a rotation in the frame's coordinates goes on; the quaternion help now says so. The centre of rotation (`about`) plays no part, and a moment left unrotated for want of an alignment point keeps its orientation as it keeps its positions. `translate_coords()` leaves orientation alone, as before.

* `rotate_coords()` gives every moment its own rotation even when some have none. A moment whose alignment point was missing dropped out of the list of rotations rather than holding a place in it, so when it was the last moment the list came up short: with two moments the first's rotation was recycled onto the second's rows, and with more the call failed with "Assigned data `rotations` must be compatible with existing data". The alignment points were also paired up by row position rather than by moment, so a member with no row at some moment failed with "non-conformable arrays", or, with equal counts, paired one moment's point with another's. They are now matched on the moment, and a moment without one is left unrotated, as was intended.

* `map_to_cartesian()`, `map_to_polar()`, `map_to_cylindrical()` and `map_to_spherical()` honour the frame's `unit_angle` (#47). They treated every angle as radians, so a frame declared in degrees converted wrongly: `phi = 90` was read as 90 radians, and `map_to_polar()` wrote radians under a `"deg"` label. A frame that arrived in degrees, from a reader or from `anicore::convert_unit_angle()`, came out wrong. A round trip only looked right because both directions made the same mistake. They now read and write `phi` and `theta` in the frame's unit, through `anicore::angle_to_rad()` and `anicore::angle_from_rad()` (animovement/anicore#170). The component converters (`cartesian_to_phi()`, `polar_to_x()`, ...) stay in radians.

* The same functions read their input columns from the frame's declared axes, so they work on frames whose columns are not called `x`, `y`, `z`, `rho`, `phi` and `theta`. They used to stop with "Column `rho` not found". A cylindrical frame's `z` keeps its column name through both directions.

## Removed

* `calculate_angular_difference()` and `diff_angle()` move to anicore, as `circ_difference()` and `circ_successive_difference()` (animovement/anicore#147). Both are general-purpose circular primitives rather than spatial transforms — the shortest signed distance between two angles, and that distance applied along a vector — and `calculate_angular_difference()` was already a one-line wrapper over `anicore::wrap_angle()`. anicore owns the angle utilities, and keeping the circular family together means it can be split out on its own later without unpicking anispace.

  The new names follow anicore's convention: `circ_*()` computes with the wraparound, `*_angle()` manipulates how an angle is written. Call `anicore::circ_difference()` and `anicore::circ_successive_difference()` instead; both are attached by `library(animovement)`, so a script using the metapackage needs no change.

# anispace 0.3.0 (2026-08-28)

## Changed

* The transforms read the frame's declaration instead of naming its columns (#20). `translate_coords()`, `rotate_coords()` and `transform_to_egocentric()` took `individual`, `keypoint`, `time`, `x`, `y` and `z` literally, so a frame declaring anything else failed outright. They now resolve identity through `variables_what`, the index through `get_index()` and the coordinates through `get_axes()`.

* The reference point is a member of any identity level, not a keypoint (#20). `to_keypoint` becomes `to`, `alignment_points` becomes `align`, and a new `level` says which identity variable they name members of — so a group centre can be taken on `individual` as readily as on `keypoint`. `level` defaults to the frame's only identity variable; a frame declaring several has to be told, since `variables_what` order is not something to rely on (animovement/anicore#140, animovement/anicore#141).

* `translate_coords()` takes `by`, a named offset per axis role, in place of `to_x` / `to_y` / `to_z`.

* `rotate_coords()` takes `about`, the centre of rotation — a member to turn around, or a fixed point. It defaults to the coordinate origin, which is what the previous behaviour was, unstated.

* `transform_to_egocentric()`'s `align` is optional. Omitted, the frame is re-centred without being reoriented.

* A quarter turn follows the frame's declared sense of rotation (#29, in part). `align_perpendicular` turned one way regardless; on a frame whose axes say its angles run clockwise it now turns the other. The `map_to_*()` half of that issue is still open.

* `wrap_angle()` and `unwrap_angle()` move to `anicore`, which already held `deg_to_rad()` and `rad_to_deg()` (animovement/aniframe#128). They are angle arithmetic rather than coordinate transformation. Use `anicore::wrap_angle()`.

* The minimum `anicore` is 0.8.0, which is the first version published under that name — the dependency was renamed without a version constraint, so nothing recorded that a pre-rename `aniframe` will not do.

* The core data structures come from `anicore`, which is what the `aniframe` package was renamed to in its 0.8.0 (animovement/anicore#84). The `aniframe` class keeps its name; only the package providing it changed.

## Fixed

* `map_to_polar()`, `map_to_cylindrical()` and `map_to_spherical()` declare the system they mapped to (#27). They returned frames whose metadata still described the system they came from — `variables_where` naming `x` and `y` on a frame that no longer had them, and `coordinate_system` still `cartesian_2d`, which `validate_aniframe()` rejects outright.

  It went unnoticed because `ensure_is_polar()` matched column names, so `rho` and `phi` being present satisfied it whatever the metadata said. Those predicates now read `coordinate_system` and fail correctly. The trailing `as_aniframe()` would have re-derived the declaration, but it runs *after* the check, so the check had never validated the object being returned.


* Rotating a frame with more than one temporal group no longer multiplies its rows (#20). `rotate_coords()` joined the rotation angles by the index alone, so every trial's angle matched every trial's rows: two trials turned 12 rows into 48, of which 36 were duplicates. It returned plausible-looking data rather than an error. The standing `# TODO: Will likely break with multiple trials` is resolved.

* Rotating three-dimensional coordinates works (#4). It aborted with "not yet supported". Two alignment points give the minimal rotation onto the target axis, leaving the roll about it as it was; a third fixes the orientation outright. Two dimensions are the same code with the rotation axis fixed to `z`.

* `translate_coords_keypoint()` looped with `1:length()`, which runs twice on empty input (#15). The loops are gone entirely, replaced by grouped operations.

* `map_to_spherical()` returns the radial distance from the origin as `rho`, rather than the cylindrical radius (#19). `theta` already used the full radius, so the triple was inconsistent with the name; ISO 80000-2 uses the radial distance. This was lossy as well as non-standard — a point on the z-axis has a cylindrical radius of zero, so `(0, 0, 5)` round-tripped through `map_to_cartesian()` to the origin, and now returns `(0, 0, 5)`.

  `spherical_to_z(rho, theta)` is now `rho * cos(theta)` where it was `rho / tan(theta)`. **Code reading `rho` from a spherical frame, or calling `spherical_to_z()` directly, needs updating.** `map_to_cylindrical()` is unchanged: `rho` there is the distance from the z-axis, which is correct for a cylindrical frame.

## Removed

* `convert_nan_to_na()`, which was neither called, exported nor tested — left behind when the mappers were rewritten.

# anispace 0.2.0 (2026-08-18)

First tagged release. anispace had been usable for a while but was never tagged, so this marks the current state rather than a change in it.

## Added

* Coordinate-system conversion: `map_to_cartesian()`, `map_to_polar()`, `map_to_cylindrical()` and `map_to_spherical()`, with the element-wise helpers behind them — `cartesian_to_rho()`, `cartesian_to_phi()`, `cartesian_to_theta()`, `polar_to_x()`, `polar_to_y()` and `spherical_to_z()`.
* Angular arithmetic: `wrap_angle()`, `unwrap_angle()`, `diff_angle()` and `circ_difference()`.
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
