# Where the reference member sits, for every row

One offset per row, looked up in the row's own group, so each subject at
each moment is measured against its own reference rather than the first
one found (#20).

## Usage

``` r
member_offsets(data, axes, to, level)
```

## Arguments

- data:

  An aniframe.

- axes:

  Named character vector, axis role to column.

- to:

  The reference member.

- level:

  The identity variable it belongs to.

## Value

A data frame of coordinate columns, aligned to `data`'s rows.
