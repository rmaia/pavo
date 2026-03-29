# Modeling spectral sensitivity

Models spectral sensitivity (with oil droplets; optional) based on peak
cone sensitivity according to the models of Govardovskii et al. (2000)
and Hart & Vorobyev (2005).

## Usage

``` r
sensmodel(
  peaksens,
  range = c(300, 700),
  lambdacut = NULL,
  Bmid = NULL,
  oiltype = NULL,
  beta = TRUE,
  om = NULL,
  integrate = TRUE,
  sensnames = paste0("lmax", peaksens)
)
```

## Arguments

- peaksens:

  (required) a vector with peak sensitivities for the cones to model.

- range:

  a vector of length 2 for the range over which to calculate the
  spectral sensitivities (defaults to 300nm to 700nm).

- lambdacut:

  a vector of same length as peaksens that lists the cut-off wavelength
  value for oil droplets. Needs either `Bmid` or `oiltype` to also be
  entered. See Hart and Vorobyev (2005).

- Bmid:

  a vector of same length as peaksens that lists the gradient of line
  tangent to the absorbance spectrum of the oil droplets. See Hart and
  Vorobyev (2005).

- oiltype:

  a list of same length as peaksens that lists the oil droplet types
  (currently accepts only "T", C", "Y", "R", "P") when Bmid is not
  known. Calculates Bmid based on the regression equations found in Hart
  ad Vorobyev (2005).

- beta:

  logical. If `TRUE` the sensitivities will include the beta peak See
  Govardovskii et al.(2000) (defaults to `TRUE`).

- om:

  a vector of same length as `range1`-`range2` that contains ocular
  media transmission data. If included, cone sensitivity will be
  corrected for ocular media transmission. Currently accepts "bird"
  using values from Hart et al. (2005), or user-defined values.

- integrate:

  logical. If `TRUE`, each curve is transformed to have a total area
  under the curve of 1 (best for visual models; defaults to `TRUE`).
  NOTE: integration is applied before any effects of ocular media are
  considered, for compatibility with visual model procedures.

- sensnames:

  A vector equal in length to `peaksens`, specifying custom names for
  the resulting sensitivity curves (e.g. c('s', 'm', 'l') for short-,
  medium- and long-wavelength sensitive receptors.)

## Value

A data frame of class `rspec` containing each cone model as a column.

## References

Govardovskii VI, Fyhrquist N, Reuter T, Kuzmin DG and Donner K. 2000. In
search of the visual pigment template. Visual Neuroscience 17:509-528,
[doi:10.1017/S0952523800174036](https://doi.org/10.1017/S0952523800174036)

Hart NS, and Vorobyev M. 2005. Modeling oil droplet absorption spectra
and spectral sensitivities of bird cone photoreceptors. Journal of
Comparative Physiology A. 191: 381-392,
[doi:10.1007/s00359-004-0595-3](https://doi.org/10.1007/s00359-004-0595-3)

Hart NS, Partridge JC, Cuthill IC, Bennett AT (2000) Visual pigments,
oil droplets, ocular media and cone photoreceptor distribution in two
species of passerine bird: the blue tit (*Parus caeruleus* L.) and the
blackbird (*Turdus merula* L.). J Comp Physiol A 186:375-387,
[doi:10.1007/s003590050437](https://doi.org/10.1007/s003590050437)

## Author

Pierre-Paul Bitton <bittonp@uwindsor.ca>

Chad Eliason <cme16@zips.uakron.edu>

## Examples

``` r
# Blue tit visual system based on Hart et al (2000)
bluesens <- sensmodel(c(371, 448, 502, 563),
  beta = FALSE,
  lambdacut = c(330, 413, 507, 572),
  oiltype = c("T", "C", "Y", "R"), om = TRUE
)

# Danio aequipinnatus based on Govardovskii et al. (2000)
daniosens <- sensmodel(c(357, 411, 477, 569))
```
