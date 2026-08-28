# Shift every coordinate by a fixed offset

Shift every coordinate by a fixed offset

## Usage

``` r
translate_by_offset(data, axes, by, call = rlang::caller_env())
```

## Arguments

- data:

  An aniframe.

- axes:

  Named character vector, axis role to column.

- by:

  Named numeric offset per axis role.

## Value

`data`, translated.
