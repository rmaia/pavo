# Plot the categorical colour vision model

Produces a plot based on Troje's (1993) categorical colour model.

## Usage

``` r
catplot(catdata, labels = TRUE, labels.cex = 0.9, ...)
```

## Arguments

- catdata:

  (required) a data frame, possibly a result from the
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md) or
  [`categorical()`](https://pavo.colrverse.com/reference/categorical.md)
  function, containing values for 'x' and 'y' coordinates as columns
  (labeled as such).

- labels:

  plot category labels inside? Defaults to `TRUE`.

- labels.cex:

  size of the arrow labels.

- ...:

  additional graphical options. See
  [`par()`](https://rdrr.io/r/graphics/par.html).

## References

Troje N. (1993). Spectral categories in the learning behaviour of
blowflies. Zeitschrift fur Naturforschung C, 48, 96-96.

## Author

Thomas White <thomas.white026@gmail.com>

## Examples

``` r
data(flowers)
vis.flowers <- vismodel(flowers, qcatch = "Qi", visual = "musca", achro = "none", relative = TRUE)
cat.flowers <- colspace(vis.flowers, space = "categorical")
plot(cat.flowers)
```
