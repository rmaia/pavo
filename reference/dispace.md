# Dichromatic colour space

Calculates coordinates and colorimetric variables that represent
reflectance spectra in a dichromatic colour space.

## Usage

``` r
dispace(vismodeldata)
```

## Arguments

- vismodeldata:

  (required) quantum catch color data. Can be either the result from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) or
  independently calculated data (in the form of a data frame with two
  columns named 's' and 'l', representing a dichromatic viewer's
  receptors).

## Value

A data frame of class
[`colspace`](https://pavo.colrverse.com/reference/colspace.md)
consisting of the following columns:

- `s`, `l`: the quantum catch data used to calculate the remaining
  variables.

- `x`: the coordinate of the stimulus along a segment

- `r.vec`: the r vector (saturation, distance from the center).

## References

Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision -
behavioural tests and physiological concepts. Biological Reviews, 78,
81 - 118.

## Author

Thomas White <thomas.white026@gmail.com>

## Examples

``` r
data(flowers)
vis.flowers <- vismodel(flowers, visual = "canis")
di.flowers <- colspace(vis.flowers, space = "di")
```
