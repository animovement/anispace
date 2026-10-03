# The rotation for each subject at each moment, from its orientation

The inverse of the member's orientation turns the body's own axes onto
the coordinate axes; the same rotation the alignment points would use
then takes them to `target`, which is nothing unless facing across. The
orientation's turn is composed exactly, rather than read back off the
matrix, so the member's `yaw` comes out 0, not a rounding error either
side of it.

## Usage

``` r
orientation_rotations(data, axes, member, level, grouping, target)
```

## Arguments

- data:

  An aniframe.

- axes:

  Named character vector, axis role to column.

- member:

  The member whose orientation gives the subject's.

- level:

  The identity variable it belongs to.

- grouping:

  The columns a rotation is held constant within.

- target:

  Where the body's axes should end up; see
  [`rotation_targets()`](https://animovement.dev/anispace/reference/rotation_targets.md).

## Value

As
[`alignment_rotations()`](https://animovement.dev/anispace/reference/alignment_rotations.md),
with `NA` rotations where the member's orientation is missing.
