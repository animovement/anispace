# Move the origin onto one member of an identity level

The reference is looked up per group – everything the frame is
identified and positioned by, except the level the member belongs to –
so each subject at each moment is centred on its own reference rather
than on the first one found (#20).

## Usage

``` r
translate_onto_member(data, axes, to, level, sign = -1)
```

## Arguments

- data:

  An aniframe.

- axes:

  Named character vector, axis role to column.

- to:

  The member to place at the origin.

- level:

  The identity variable it belongs to.

## Value

`data`, translated.
