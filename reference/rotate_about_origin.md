# Rotate every subject's coordinates about the origin

The rotation is worked out per group – everything the frame is
identified and positioned by, except the level the alignment points
belong to – so each subject at each moment gets its own. Reading the
grouping from the frame rather than assuming `individual` and `time` is
what stops a second trial's angle being applied to the first's rows
(#20).

## Usage

``` r
rotate_about_origin(data, axes, align, level, align_perpendicular = FALSE)
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

- align_perpendicular:

  Put the primary axis across the target.

## Value

`data`, rotated.
