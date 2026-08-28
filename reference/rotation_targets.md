# Where the alignment axes should end up

The primary axis goes onto x. Put across it instead, it goes onto y – or
onto -y on a frame whose angles run clockwise, since a quarter turn
there goes the other way round.

## Usage

``` r
rotation_targets(n_axes, align_perpendicular, sense = "unknown")
```

## Arguments

- n_axes:

  How many spatial axes the frame has.

- align_perpendicular:

  Put the primary axis across the target.

- sense:

  The frame's `angle_direction`.

## Value

A list of two length-3 target vectors.
