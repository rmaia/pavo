# Colour hexagon

Calculates coordinates and colourimetric variables that represent
reflectance spectra in the hymenopteran colour hexagon.

## Usage

``` r
hexagon(vismodeldata)
```

## Arguments

- vismodeldata:

  (required) quantum catch colour data. Can be either the result from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) or
  independently calculated data (in the form of a data frame with three
  columns named 's', 'm', 'l', representing a trichromatic viewer).

## Value

A data frame of class `colspace` consisting of the following columns:

- `s`, `m`, `l`: the quantum catch data used to calculate the remaining
  variables

- `x`, `y`: cartesian coordinates in the colour hexagon.

- `h.theta`: hue angle theta (in degrees), with 0-degrees at the 1200
  angle, increasing clockwise.

- `r.vec`: the r vector (saturation, distance from the center).

- `sec.fine`: fine 'hue sector', wherein the full hexagon is composed of
  36 10-degree sectors, with 0-degrees at the 1200 angle.

- `sec.coarse`: coarse 'hue sector', wherein the full hexagon is
  composed of five sectors: blue, bluegreen, green, uvgreen, uv, and
  uvblue.

## References

Chittka L. (1992). The colour hexagon: a chromaticity diagram based on
photoreceptor excitations as a generalized representation of colour
opponency. Journal of Comparative Physiology A, 170(5), 533-543.

Chittka L, Shmida A, Troje N, Menzel R. (1994). Ultraviolet as a
component of flower reflections, and the colour perception of
Hymenoptera. Vision research, 34(11), 1489-1508.

## Author

Thomas White <thomas.white026@gmail.com>

## Examples

``` r
data(flowers)
vis.flowers <- vismodel(flowers,
  visual = "apis", qcatch = "Ei", relative = FALSE,
  vonkries = TRUE, achromatic = "l", bkg = "green"
)
flowers.hex <- colspace(vis.flowers, space = "hexagon")
```
