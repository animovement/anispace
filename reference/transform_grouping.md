# The columns a transform must hold constant

A reference point is looked up per group, and the groups are everything
the frame is identified and positioned by, minus the level the reference
belongs to. Getting this wrong is what made
[`rotate_coords()`](https://animovement.dev/anispace/reference/rotate_coords.md)
join a trial's angle onto every other trial's rows.

## Usage

``` r
transform_grouping(data, level)
```

## Arguments

- data:

  An aniframe.

- level:

  The identity variable the reference is a member of.

## Value

Character vector of column names.
