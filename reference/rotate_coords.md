# Rotate coordinates in Cartesian space (2D or 3D)

Automatically detects whether data are 2D or 3D and applies the
corresponding rotation method.

## Usage

``` r
rotate_coords(data, alignment_points, align_perpendicular = FALSE)
```

## Arguments

- data:

  movement data frame with columns: time, individual, keypoint, x, y, z
  (optional)

- alignment_points:

  character vector of length 2 specifying the keypoints used for
  alignment

- align_perpendicular:

  logical; if TRUE, alignment_points are rotated to be perpendicular to
  the 0-degree axis (y-axis). If FALSE (default), they are aligned with
  the x-axis.

## Value

movement data frame with rotated coordinates
