# Segment classification

Calculates segment classification measures as defined in Endler (1990).

## Usage

``` r
segspace(vismodeldata)
```

## Arguments

- vismodeldata:

  (required) quantum catch color data. Can be either the result from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) or
  independently calculated data (in the form of a data frame with
  columns named 'S1', 'S2', 'S3', 'S4', and, optionally, 'lum',
  representing a generic 'tetrachromatic' viewer).

## Value

A data frame of class
[`colspace`](https://pavo.colrverse.com/reference/colspace.md)
consisting of the following columns:

- `S1`, `S2`, `S3`, `S4`: the relative reflectance at each of the four
  segments.

- `LM`, `MS`: segment scores

- `C`, `H`, `B`: 'chroma', 'hue' (degrees), and 'brightness' in the
  segment classification space

## References

Endler, J. A. (1990) On the measurement and classification of colour in
studies of animal colour patterns. Biological Journal of the Linnean
Society, 41, 315-352.

## Author

Thomas E. White <thomas.white026@gmail.com>

Pierre-Paul Bitton <bittonp@uwindsor.ca>

## Examples

``` r
data(sicalis)
vis.sic <- vismodel(sicalis, visual = "segment", achromatic = "all")
seg.sic <- colspace(vis.sic, space = "segment")
#> Warning: Input data is a vismodel object and is not tetrachromatic. Extra receptors will be dropped.
```
