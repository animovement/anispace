# Apply a rotation per group to the coordinates and orientation

Apply a rotation per group to the coordinates and orientation

## Usage

``` r
apply_rotations(data, axes, rotations, grouping)
```

## Arguments

- data:

  An aniframe.

- axes:

  Named character vector, axis role to column.

- rotations:

  One row per group, as from
  [`alignment_rotations()`](https://animovement.dev/anispace/reference/alignment_rotations.md):
  `.rot` turns the positions and `.turn` the orientation, and the two
  must be the same rotation. A rotation of `NA`s makes the rows it
  applies to `NA`.

- grouping:

  The columns to join `rotations` on.

## Value

`data`, rotated.
