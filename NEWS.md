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
