# Trichromatic colour space

Calculates coordinates and colourimetric variables that represent
reflectance spectra in a trichromatic chromaticity space.

## Usage

``` r
trispace(vismodeldata)
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

- `x`, `y`: cartesian coordinates for the points in the Maxwell
  triangle.

- `h.theta`: angle theta, in radians, determining the hue of the colour.

- `r.vec`: the r vector (saturation, distance from the center).

## References

Maxwell JC. (1970). On color vision. In: Macadam, D. L. (ed) Sources of
Color Science. Cambridge, MIT Press, 1872 - 1873.

Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision -
behavioural tests and physiological concepts. Biological Reviews, 78,
81 - 118.

MacLeod DIA, Boynton RM. (1979). Chromaticity diagram showing cone
excitation by stimuli of equal luminance. Journal of the Optical Society
of America, 69, 1183-1186.

## Author

Thomas E. White <thomas.white026@gmail.com>

## Examples

``` r
data(flowers)
vis.flowers <- vismodel(flowers, visual = "apis", achromatic = "l")
tri.flowers <- colspace(vis.flowers, space = "tri")
```
