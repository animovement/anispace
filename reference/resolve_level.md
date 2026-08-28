# The identity variable a reference point belongs to

Not guessed. `variables_what` is documented coarse to fine, but nothing
enforces it and attributes like sex or treatment do not nest at all
(animovement/anicore#140, animovement/anicore#141), so a frame declaring
more than one has to be told which level `to` or `align` name members
of.

## Usage

``` r
resolve_level(data, level = NULL, call = rlang::caller_env())
```

## Arguments

- data:

  An aniframe.

- level:

  The caller's choice, or `NULL`.

## Value

Length-one character vector naming the column.
