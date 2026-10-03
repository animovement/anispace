# The rotation for each subject at each moment, from its alignment points

Each alignment point is looked up in every group by joining on the
grouping, not by position, so a member missing from a moment leaves that
moment without a rotation rather than pairing the points of different
moments.

## Usage

``` r
alignment_rotations(data, axes, align, level, grouping, target)
```

## Arguments

- data:

  An aniframe.

- axes:

  Named character vector, axis role to column.

- align:

  Values of `level` defining the axes.

- level:

  The identity variable they belong to.

- grouping:

  The columns a rotation is held constant within.

- target:

  Where the alignment axes should end up; see
  [`rotation_targets()`](https://animovement.dev/anispace/reference/rotation_targets.md).

## Value

A tibble with one row per group: the `grouping` columns; `.rot`, a list
of 3x3 rotation matrices, `NULL` where the rotation is undefined; and
`.turn`, the same rotations as
[`turn_of()`](https://animovement.dev/anispace/reference/turn_of.md)
gives them.
