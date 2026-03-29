# Add legend to a static tetrahedral colourspace

Adds a legend to a static tetrahedral colourspace plot.

## Usage

``` r
legendtetra(x = 0.8, y = 1.2, ...)
```

## Arguments

- x, y:

  position of the legend relative to plot limits (usually a value
  between 0 and 1, but because of the perspective distortion, values
  greater than 1 are possible)

- ...:

  additional arguments passed to
  [`legend()`](https://rdrr.io/r/graphics/legend.html).

## Value

`legendtetra()` adds a legend to a static tetrahedral colourspace plot.
for additional information on which arguments are necessary and how they
are used, see [`legend()`](https://rdrr.io/r/graphics/legend.html).

## Author

Rafael Maia <rm72@zips.uakron.edu>

## Examples

``` r
data(sicalis)

vis_sicalis <- vismodel(sicalis)
tcs_sicalis <- colspace(vis_sicalis)

cols <- c("#1B9E77", "#D95F02", "#7570B3")
plot(tcs_sicalis, col = cols)
legendtetra(
  legend = c("Crown", "Throat", "Breast"),
  col = cols, pch = 16
)
```
