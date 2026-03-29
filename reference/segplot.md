# Plot the segment-analysis model

Produces a plot based on Endler's (1990) segment analysis.

## Usage

``` r
segplot(
  segdata,
  labels = TRUE,
  lab.cex = 0.9,
  out.lwd = 1,
  out.lty = 1,
  out.lcol = "black",
  tick.loc = c(-1, -0.5, 0.5, 1),
  square = TRUE,
  ...
)
```

## Arguments

- segdata:

  (required) a data frame, possibly a result from the
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md) or
  [`segspace()`](https://pavo.colrverse.com/reference/segspace.md)
  function, containing values for 'LM' and 'MS' as columns (labeled as
  such).

- labels:

  logical. Should the name of each cone be printed next to the
  corresponding vertex?

- tick.loc:

  a numeric vector specifying the location of tick marks on x & y axes.

- square:

  logical. Should the aspect ratio of the plot be held to 1:1? (defaults
  to `TRUE`).

- ...:

  additional graphical options. See
  [`par()`](https://rdrr.io/r/graphics/par.html).

## References

Endler, J. A. (1990) On the measurement and classification of colour in
studies of animal colour patterns. Biological Journal of the Linnean
Society, 41, 315-352.

## Author

Thomas White <thomas.white026@gmail.com>

## Examples

``` r
data(flowers)
vis.flowers <- vismodel(flowers, visual = "segment", achromatic = "all")
seg.flowers <- colspace(vis.flowers, space = "segment")
#> Warning: Input data is a vismodel object and is not tetrachromatic. Extra receptors will be dropped.
plot(seg.flowers)
```
