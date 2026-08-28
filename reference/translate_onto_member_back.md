# Put a frame back where its reference member was

The offsets come from the frame as it was before centring, since the
member sits at the origin afterwards and no longer knows where it came
from.

## Usage

``` r
translate_onto_member_back(rotated, original, axes, about, level)
```

## Arguments

- rotated:

  The frame after rotation.

- original:

  The frame before centring.

- axes:

  Named character vector, axis role to column.

- about:

  The member it was centred on.

- level:

  The identity variable it belongs to.

## Value

`rotated`, shifted back.
