# Categorical fly-visual model

Applies the categorical colour vision model of Troje (1993)

## Usage

``` r
categorical(vismodeldata)
```

## Arguments

- vismodeldata:

  (required) quantum catch color data. Can be either the result from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) or
  independently calculated data (in the form of a data frame with four
  columns named 'u' ,'s', 'm', 'l', representing a tetrachromatic
  viewer).

## Value

Object of class `colspace` consisting of the following columns:

- `R7p, R7y, R8p, R8y`: the quantum catch data used to calculate the
  remaining variables.

- `x, y`: cartesian coordinates in the categorical colour space.

- `r.vec`: the r vector (saturation, distance from the center).

- `h.theta`: angle theta (in radians), a continuous measure of stimulus
  hue.

- `category`: fly-colour category. One of `p-y-`, `p-y+`, `p+y-`,
  `p+y+`.

## References

Troje N. (1993). Spectral categories in the learning behaviour of
blowflies. Zeitschrift fur Naturforschung C, 48, 96-96.

## Author

Thomas White <thomas.white026@gmail.com>

## Examples

``` r
data(flowers)
vis.flowers <- vismodel(flowers, visual = "musca", achromatic = "md.r1")
cat.flowers <- colspace(vis.flowers, space = "categorical")
```
