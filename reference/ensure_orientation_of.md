# Can the frame be aligned by a member's orientation?

It needs a declared orientation, and the member has to carry it at least
once: a frame recording orientation on one member only, aligned by
another's, would otherwise come back entirely `NA`.

## Usage

``` r
ensure_orientation_of(data, member, level, call = rlang::caller_env())
```

## Arguments

- data:

  An aniframe.

- member:

  The member whose orientation is used, known to be one.

- level:

  The identity variable it belongs to.

## Value

`TRUE`, invisibly.
