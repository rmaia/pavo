# Visual models

Calculates quantum catches at each photoreceptor. Both raw and relative
values can be returned, for use in a suite of colourspace and
non-colourspace models.

## Usage

``` r
vismodel(
  rspecdata,
  visual = c("avg.uv", "avg.v", "bluetit", "ctenophorus", "star", "pfowl", "apis",
    "canis", "cie2", "cie10", "musca", "drosophila", "segment", "habronattus",
    "rhinecanthus"),
  achromatic = c("none", "bt.dc", "ch.dc", "st.dc", "md.r1", "dm.r1", "ra.dc", "cf.r",
    "ml", "l", "all"),
  illum = c("ideal", "bluesky", "D65", "forestshade"),
  trans = c("ideal", "bluetit", "blackbird"),
  qcatch = c("Qi", "fi", "Ei"),
  bkg = c("ideal", "green"),
  vonkries = FALSE,
  scale = 1,
  relative = TRUE
)
```

## Arguments

- rspecdata:

  (required) a data frame, possibly of class `rspec`, which contains a
  column containing a wavelength range, named 'wl', and spectra data in
  remaining columns.

- visual:

  the visual system to be used. Options are:

  - a data frame such as one produced containing by
    [`sensmodel()`](https://pavo.colrverse.com/reference/sensmodel.md),
    containing user-defined sensitivity data for the receptors involved
    in colour vision. The data frame must contain a `'wl'` column with
    the range of wavelengths included, and the sensitivity for each
    other cone as a column.

  - `'apis'`: Honeybee *Apis mellifera*.

  - `'avg.uv'`: average avian UV system (default).

  - `'avg.v'`: average avian V system.

  - `'bluetit'`: Blue tit *Cyanistes caeruleus*.

  - `'canis'`: Canid *Canis familiaris*.

  - `'cie2'`: 2-degree colour matching functions for CIE models of human
    colour vision. Functions are linear transformations of the 2-degree
    cone fundamentals of Stockman & Sharpe (2000), as ratified by the
    CIE (2006).

  - `'cie10'`: 10-degree colour matching functions for CIE models of
    human colour vision. Functions are linear transformations of the
    10-degree cone fundamentals of Stockman & Sharpe (2000), as ratified
    by the CIE (2006).

  - `'ctenophorus'`: Ornate dragon lizard *Ctenophorus ornatus*.

  - `'musca'`: Housefly *Musca domestica*.

  - `'drosophila'`: Vinegar fly *Drosophila melanogaster* (Sharkey et
    al. 2020).

  - `'pfowl'`: Peafowl *Pavo cristatus*.

  - `'segment'`: Generic tetrachromat 'viewer' for use in the segment
    analysis of Endler (1990).

  - `'star'`: Starling *Sturnus vulgaris*.

  - `'habronattus'`: Jumping spider *Habronattus pyrrithrix*.

  - `'rhinecanthus'`: Triggerfish *Rhinecanthus aculeatus*.

- achromatic:

  the sensitivity data to be used to calculate luminance (achromatic)
  receptor stimulation. Either a vector containing the sensitivity for a
  single receptor, or one of the options:

  - `'none'`: no achromatic stimulation calculated (default).

  - `'bt.dc'`: Blue tit *Cyanistes caeruleus* double cone.

  - `'ch.dc'`: Chicken *Gallus gallus* double cone.

  - `'st.dc'`: Starling *Sturnus vulgaris* double cone.

  - `'md.r1'`: Housefly *Musca domestica* R1-6 photoreceptor.

  - `'dm.r1'`: Vinegar fly *Drosophila melanogaster* R1-6 photoreceptor.

  - `'ra.dc'`: Triggerfish *Rhinecanthus aculeatus* double cone.

  - `'cf.r'`: Canid *Canis familiaris* cone.

  - `'ml'`: the summed response of the two longest-wavelength
    photoreceptors.

  - `'l'`: the longest-wavelength photoreceptor.

  - `'all'`: the summed response of all photoreceptors.

- illum:

  either a vector containing the illuminant, or one of the options:

  - `'ideal'`: homogeneous illuminance of 1 across wavelengths (default)

  - `'bluesky'` open blue sky.

  - `'D65'`: standard daylight.

  - `'forestshade'` forest shade.

- trans:

  either a vector containing the ocular or environmental transmission
  spectra, or one of the options:

  - `'ideal'`: homogeneous transmission of 1 across all wavelengths
    (default)

  - `'bluetit'`: blue tit *Cyanistes caeruleus* ocular transmission
    (from Hart et al. 2000).

  - `'blackbird'`: blackbird *Turdus merula* ocular transmission (from
    Hart et al. 2000).

- qcatch:

  Which quantal catch metric to return. Options are:

  - `'Qi'`: Quantum catch for each photoreceptor (default)

  - `'fi'`: Quantum catch according to Fechner's law (the signal of the
    receptor channel is proportional to the logarithm of the quantum
    catch)

  - `'Ei'`: Hyperbolic-transformed quantum catch, where Ei = Qi / (Qi +
    1).

- bkg:

  background spectrum. Note that this will have no effect when
  `vonkries = FALSE`. Either a vector containing the spectral data, or
  one of the options:

  - `'ideal'`: homogeneous illuminance of 1 across all wavelengths
    (default).

  - `'green'`: green foliage.

- vonkries:

  logical. Should the von Kries colour correction transformation be
  applied? (defaults to `FALSE`).

- scale:

  a value by which the illuminant will be multiplied. Useful for when
  the illuminant is a relative value (i.e. transformed to a maximum of 1
  or to a percentage), and does not correspond to quantum flux units
  (μmol.s⁻¹.m⁻²). Useful values are, for example, 500 (for dim light)
  and 10000 (for bright illumination). Note that if `vonkries = TRUE`
  this transformation has no effect.

- relative:

  should relative quantum catches be returned (i.e. is it a colour space
  model? Defaults to `TRUE`).

## Value

An object of class `vismodel` containing the photon catches for each of
the photoreceptors considered. Information on the parameters used in the
calculation are also stored and can be called using the
[`summary.vismodel()`](https://pavo.colrverse.com/reference/summary.vismodel.md)
function.

## Note

Built-in `visual`, `achromatic`, `illum`, `bkg` and `trans` are only
defined on the 300 to 700nm wavelength range. If you wish to work
outside this range, you will need to provide your own data.

## References

Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I.
(1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of
Comparative Physiology A-Neuroethology Sensory Neural And Behavioral
Physiology, 183(5), 621-633.

Hart, N. S., Partridge, J. C., Cuthill, I. C., Bennett, A. T. D. (2000).
Visual pigments, oil droplets, ocular media and cone photoreceptor
distribution in two species of passerine bird: the blue tit (Parus
caeruleus L.) and the blackbird (Turdus merula L.). Journal of
Comparative Physiology A, 186, 375-387.

Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress
In Retinal And Eye Research, 20(5), 675-703.

Barbour H. R., Archer, M. A., Hart, N. S., Thomas, N., Dunlop, S. A.,
Beazley, L. D, Shand, J. (2002). Retinal characteristics of the Ornate
Dragon Lizard, Ctenophorus ornatus.

Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color
in a tetrahedral color space: A phylogenetic analysis of new world
buntings. The American Naturalist, 171(6), 755-776.

Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as
birds see them. Biological Journal Of The Linnean Society, 86(4),
405-431.

Chittka L. (1992). The colour hexagon: a chromaticity diagram based on
photoreceptor excitations as a generalized representation of colour
opponency. Journal of Comparative Physiology A, 170(5), 533-543.

Stockman, A., & Sharpe, L. T. (2000). Spectral sensitivities of the
middle- and long-wavelength sensitive cones derived from measurements in
observers of known genotype. Vision Research, 40, 1711-1737.

CIE (2006). Fundamental chromaticity diagram with physiological axes.
Parts 1 and 2. Technical Report 170-1. Vienna: Central Bureau of the
Commission Internationale de l' Eclairage.

Neitz, J., Geist, T., Jacobs, G.H. (1989) Color vision in the dog.
Visual Neuroscience, 3, 119-125.

Sharkey, C. R., Blanco, J., Leibowitz, M. M., Pinto-Benito, D., &
Wardill, T. J. (2020). The spectral sensitivity of Drosophila
photoreceptors. Scientific reports, 10(1), 1-13.

## See also

[`sensdata()`](https://pavo.colrverse.com/reference/sensdata.md) to
retrieve or plot in-built spectral sensitivity data used in `vismodel()`

## Author

Thomas E. White <thomas.white026@gmail.com>

Rafael Maia <rm72@zips.uakron.edu>

## Examples

``` r
# Dichromat (dingo)
data(flowers)
vis.dingo <- vismodel(flowers, visual = "canis")
di.dingo <- colspace(vis.dingo, space = "di")

# Trichromat (honeybee)
data(flowers)
vis.bee <- vismodel(flowers, visual = "apis")
tri.bee <- colspace(vis.bee, space = "tri")

# Tetrachromat (blue tit)
data(sicalis)
vis.bluetit <- vismodel(sicalis, visual = "bluetit")
tcs.bluetit <- colspace(vis.bluetit, space = "tcs")

# Tetrachromat (starling), receptor-noise model
data(sicalis)
vis.star <- vismodel(sicalis, visual = "star", achromatic = "bt.dc", relative = FALSE)
dist.star <- coldist(vis.star, achromatic = TRUE)
#> Calculating noise-weighted Euclidean distances and noise-weighted luminance contrasts

# Estimate quantum catches using a custom trichromatic visual phenotype
custom <- sensmodel(c(330, 440, 550))
names(custom) <- c("wl", "s", "m", "l")
vis.custom <- vismodel(flowers, visual = custom)
tri.custom <- colspace(vis.custom, space = "tri")
```
