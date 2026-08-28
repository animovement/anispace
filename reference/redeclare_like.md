# Re-declare a transformed frame the way its source was declared

A transform changes coordinates, never the declaration, so letting
`as_aniframe()` re-detect risks it inventing an identity column and
replacing the metadata. The rest of the source's metadata comes with it.

## Usage

``` r
redeclare_like(transformed, source)
```

## Arguments

- transformed:

  A plain data frame derived from `source`.

- source:

  The aniframe it came from.

## Value

`transformed` as an aniframe, declared as `source` was.
