# Translate coordinates (Cartesian)

Translates coordinates in Cartesian space. Takes either a single point
(`to_x` and `to_y`), a vector with the same length as the time dimension
or a keypoint (`to_keypoint`), which can be used to transform the data
into an egocentric reference frame.

## Usage

``` r
translate_coords(data, to_x = 0, to_y = 0, to_z = NULL, to_keypoint = NULL)
```

## Arguments

- data:

  movement data frame with columns: time, individual, keypoint, x, y

- to_x:

  x coordinates; either a single value or a time-length vector

- to_y:

  y coordinates; either a single value or a time-length vector

- to_z:

  z coordinates (only if 3D); either a single value or a time-length
  vector

- to_keypoint:

  all other coordinates becomes relative to this keypoint

## Value

movement data frame with translated coordinates
