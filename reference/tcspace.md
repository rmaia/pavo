# Tetrahedral colourspace

Calculates coordinates and colorimetric variables that represent
reflectance spectra in the avian tetrahedral color space.

## Usage

``` r
tcspace(vismodeldata)
```

## Arguments

- vismodeldata:

  (required) quantum catch color data. Can be either the result from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) or
  independently calculated data (in the form of a data frame with four
  columns name 'u', 's', 'm', 'l', representing the avian cones).

## Value

A data frame of class
[`colspace`](https://pavo.colrverse.com/reference/colspace.md)
consisting of the following columns:

- `u`, `s`, `m`, `l`: the quantum catch data used to calculate the
  remaining variables. NOTE: even if visual system is of type V-VIS, the
  output column will be labeled `u`.

- `u.r`, `s.r`, `m.r`, `l.r`: relative cone stimulation, for a given
  hue, as a function of saturation. See Stoddard & Prum (2008) for
  details.

- `x`, `y`, `z`: cartesian coordinates for the points in the tetrahedral
  color space.

- `h.theta`, `h.phi`: angles theta and phi, in radians, determining the
  hue of the color.

- `r.vec`: the r vector (saturation, distance from the achromatic
  center).

- `r.max`: the maximum r vector achievable for the colour's hue.

- `r.achieved`: the relative r distance from the achromatic center, in
  relation to the maximum distance achievable (`r.vec/r.max`).

## References

Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color
in a tetrahedral color space: A phylogenetic analysis of new world
buntings. The American Naturalist, 171(6), 755-776.

Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as
birds see them. Biological Journal Of The Linnean Society, 86(4),
405-431.

## Author

Rafael Maia <rm72@zips.uakron.edu>

## Examples

``` r
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
```
