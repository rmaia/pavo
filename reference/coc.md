# Color opponent coding model

Calculates coordinates and colorimetric variables that represent
reflectance spectra in the color opponent coding model of hymenopteran
vision.

## Usage

``` r
coc(vismodeldata)
```

## Arguments

- vismodeldata:

  (required) quantum catch colour data. Can be either the result from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) or
  independently calculated data (in the form of a data frame with three
  columns named 's', 'm', 'l', representing a trichromatic viewer).

## Value

A data frame of class
[`colspace`](https://pavo.colrverse.com/reference/colspace.md)
consisting of the following columns:

- `s`, `m`, `l`: the quantum catch data used to calculate the remaining
  variables.

- `x`, `y`: coordinates for the points in coc space

- `r.vec`: the r vector (saturation, distance from the center using a
  city-block metric).

## References

Backhaus W. (1991). Color opponent coding in the visual system of the
honeybee. Vision Research, 31, 1381-1397.

## Author

Thomas White <thomas.white026@gmail.com>

## Examples

``` r
data(flowers)
vis.flowers <- vismodel(flowers, visual = "apis", qcatch = "Ei", relative = FALSE, vonkries = TRUE)
coc.flowers <- colspace(vis.flowers, space = "coc")
```
