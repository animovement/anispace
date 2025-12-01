# Translate coordinates relative to coordinates

Translate coordinates relative to coordinates

## Usage

``` r
translate_coords_vector(data, to_x, to_y, to_z = NULL)
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
