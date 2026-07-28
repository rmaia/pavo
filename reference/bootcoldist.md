# Bootstrap colour distance confidence intervals

Uses a bootstrap procedure to generate confidence intervals for the mean
colour distance between two or more samples of colours

## Usage

``` r
bootcoldist(
  vismodeldata,
  by,
  boot.n = 1000,
  alpha = 0.95,
  raw = FALSE,
  ...,
  cluster = NULL,
  nesting = c("auto", "crossed", "nested"),
  ci.type = c("perc", "bca"),
  correct = FALSE
)
```

## Arguments

- vismodeldata:

  (required) quantum catch colour data. Can be the result from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md), or
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md). Data
  may also be independently calculated quantum catches, in the form of a
  data frame with columns representing photoreceptors.

- by:

  (required) a numeric or character vector indicating the group to which
  each row from the object belongs to.

- boot.n:

  number of bootstrap replicates (defaults to 1000)

- alpha:

  the confidence level for the confidence intervals (defaults to 0.95)

- raw:

  should the full set of bootstrapped distances (equal in length to
  boot.n) be returned, instead of the summary distances and CI's?
  Defaults to FALSE. Each row is one bootstrap replicate, so values
  sharing a row, whether for different contrasts or for dS and dL, were
  calculated from the same resampled data and can be compared with one
  another.

- ...:

  other arguments to be passed to
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md). Must
  at minimum include `n` and `weber`. See
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) for
  details.

- cluster:

  an optional numeric or character vector, of the same length as `by`,
  identifying the higher-level unit (e.g. the individual, colony, or
  patch-bearing pattern) that each row belongs to. When supplied,
  resampling is done over whole clusters rather than over individual
  rows, which is appropriate whenever rows are not independent of one
  another. Defaults to NULL, in which case rows are resampled
  independently within each group, as in previous versions.

- nesting:

  the relationship between `cluster` and `by`, one of `"auto"` (the
  default), `"crossed"`, or `"nested"`. Under `"crossed"`, clusters span
  the levels of `by` (e.g. the same individual contributes a crown,
  throat and breast measurement) and a single draw of clusters is shared
  across groups, which preserves the pairing between them. Under
  `"nested"`, each cluster belongs to exactly one group (e.g. repeated
  measurements of an individual within a population) and clusters are
  drawn independently within each group. `"auto"` chooses between the
  two by checking whether any cluster appears under more than one level
  of `by`. Ignored when `cluster` is NULL.

- ci.type:

  the type of confidence interval, either `"perc"` (the default) for
  percentiles of the bootstrap distribution, or `"bca"` for
  bias-corrected and accelerated limits. Colour distances are bounded
  below by zero and are usually right-skewed, which is the situation in
  which percentile limits sit off-centre. `bca` shifts them to account
  for both that skew and for where the empirical distance falls within
  the bootstrap distribution.

- correct:

  logical. Should the distance be corrected for the sampling error in
  the group means? Defaults to `FALSE` for consistency with previous
  versions, but `TRUE` is recommended wherever it is available: the
  uncorrected distance is biased upwards for any data at all, and the
  correction removes that bias exactly rather than approximately. Both
  the estimate and its interval move downwards, so a contrast will less
  often have its lower limit above the theoretical threshold. The
  interval is if anything slightly wider, since the correction is
  estimated rather than known and the bootstrap carries that uncertainty
  as well.

  The distance between two group means is biased upwards, because each
  mean is estimated with error and distance is a convex function of that
  error. On the squared scale the displacement is exactly the sum, over
  groups, of the mean squared pairwise distance among that group's
  observations divided by twice their number, so it is largest when
  groups are small and internally variable and it does not vanish as the
  true separation goes to zero. Two samples drawn from a single
  population will therefore be separated by an apparently non-zero
  distance. Setting `correct = TRUE` subtracts that displacement from
  the empirical distance and from every bootstrap replicate, using in
  each case the observations that replicate drew, and returns the square
  root of what remains. Distances that would become negative are
  returned as zero, in the same way and for the same reason as a
  negative variance component.

  The correction relies on the distance being one that arises from an
  inner product, and so is unavailable with `noise = "quantum"`, in the
  `CIELAB`, `CIELCh` and `coc` spaces, and for achromatic contrast in a
  colourspace model, where luminance contrast is a ratio rather than a
  distance. It cannot currently be combined with `ci.type = "bca"`.

  Note that `cluster`, `nesting`, `ci.type` and `correct` follow `...`,
  and so must all be named in full when used.

## Value

a matrix including the empirical mean and bootstrapped confidence limits
for dS (and dL if `achromatic = TRUE`), or a data.frame of raw
bootstraped dS (and dL if `achromatic = TRUE`) values equal in length to
boot.n.

## Details

You can customise the type of parallel processing used by this function
with the
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
function. This works on all operating systems, as well as high
performance computing (HPC) environment. Similarly, you can customise
the way progress is shown with the
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html)
functions (progress bar, acoustic feedback, nothing, etc.)

## References

Maia, R., White, T. E., (2018) Comparing colors using visual models.
Behavioral Ecology, ary017
[doi:10.1093/beheco/ary017](https://doi.org/10.1093/beheco/ary017)

## Examples

``` r
# \donttest{
# Run the receptor-noise limited model, using the visual phenotype
# of the blue tit
data(sicalis)
vm <- vismodel(sicalis, achromatic = "bt.dc", relative = FALSE)
gr <- gsub("ind..", "", rownames(vm))
bootcoldist(vm, by = gr, n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1)
#> Calculating noise-weighted Euclidean distances and noise-weighted luminance contrasts
#>      dS.mean    dS.lwr   dS.upr   dL.mean    dL.lwr   dL.upr
#> B-C 4.626548 3.0446919 6.861227 7.4677171 5.6748829 9.282542
#> B-T 1.741764 0.2188554 5.014370 0.2340913 0.0193291 1.616231
#> C-T 6.110519 4.1550209 8.637632 7.2336258 5.4127958 9.045694

# These data are hierarchically structured, since each of the seven individuals
# contributes one crown, throat, and breast measurement. Rows sharing an
# individual are therefore not independent, and we can resample whole
# individuals rather than individual rows to account for it.
ind <- substr(rownames(vm), 1, 4)
bootcoldist(vm,
  by = gr, cluster = ind,
  n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1
)
#> Calculating noise-weighted Euclidean distances and noise-weighted luminance contrasts
#>      dS.mean    dS.lwr   dS.upr   dL.mean    dL.lwr   dL.upr
#> B-C 4.626548 2.9651177 6.345287 7.4677171 6.3149932 8.576842
#> B-T 1.741764 0.2656546 3.990978 0.2340913 0.0196446 1.510707
#> C-T 6.110519 4.6425355 7.981340 7.2336258 5.3982137 8.953411

# The distances themselves are still inflated, since each group mean is
# estimated from only seven birds and the distance between two noisy means
# exceeds the distance between the true ones. correct = TRUE removes that
# displacement. Note what happens to the breast-throat contrast: an estimate
# of 1.74, comfortably above the theoretical threshold, is entirely accounted
# for by sampling error and falls to zero.
bootcoldist(vm,
  by = gr, cluster = ind, correct = TRUE,
  n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1
)
#> Calculating noise-weighted Euclidean distances and noise-weighted luminance contrasts
#>      dS.mean   dS.lwr   dS.upr  dL.mean   dL.lwr   dL.upr
#> B-C 4.108526 2.129681 5.925022 7.384813 6.302973 8.570131
#> B-T 0.000000 0.000000 3.527475 0.000000 0.000000 1.110461
#> C-T 5.728305 4.205223 7.837879 7.148008 5.492774 8.912263

# The two arguments do different jobs, and this design shows it cleanly.
# Dropping cluster leaves the corrected distances unchanged, because each bird
# contributes exactly one measurement to each patch: within a group the seven
# rows are the seven clusters, so there is nothing for the correction to do
# differently. The intervals do change, and are wider here without the pairing
# between patches that resampling whole birds preserves.
#
# Where the estimates themselves would differ is when a group contains several
# measurements of the same individual, since the correction is then governed
# by how many individuals there are rather than how many rows.
bootcoldist(vm,
  by = gr, correct = TRUE,
  n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1
)
#> Calculating noise-weighted Euclidean distances and noise-weighted luminance contrasts
#>      dS.mean   dS.lwr   dS.upr  dL.mean   dL.lwr   dL.upr
#> B-C 4.108526 2.209315 6.476223 7.384813 5.650389 9.287107
#> B-T 0.000000 0.000000 4.573293 0.000000 0.000000 1.250814
#> C-T 5.728305 3.482210 8.464476 7.148008 5.312734 8.821955

# Run the same again, though as a simple colourspace model
data(sicalis)
vm <- vismodel(sicalis, achromatic = "bt.dc")
space <- colspace(vm)
gr <- gsub("ind..", "", rownames(space))
bootcoldist(space, by = gr)
#> Quantum catch are relative, distances may not be meaningful
#> Calculating unweighted Euclidean distances and Weber luminance contrast
#>        dS.mean      dS.lwr     dS.upr    dL.mean      dL.lwr    dL.upr
#> B-C 0.08873077 0.058818120 0.12513336 1.11017675 0.768082432 1.4990065
#> B-T 0.02510607 0.005610291 0.06946696 0.02368528 0.002664469 0.1671409
#> C-T 0.11208534 0.080853592 0.14561773 1.06135304 0.724331098 1.4656737

# Estimate bootstrapped colour-distances for a more 'specialised' model,
# like the colour hexagon
data(flowers)
vis.flowers <- vismodel(flowers,
  visual = "apis", qcatch = "Ei", relative = FALSE,
  vonkries = TRUE, achromatic = "l", bkg = "green"
)
flowers.hex <- colspace(vis.flowers, space = "hexagon")
pop_group <- c(rep("pop_1", nrow(flowers.hex) / 2), rep("pop_2", nrow(flowers.hex) / 2))
bootcoldist(flowers.hex, by = pop_group)
#> Calculating unweighted Euclidean distances and simple luminance contrast
#>                dS.mean    dS.lwr    dS.upr   dL.mean    dL.lwr   dL.upr
#> pop_1-pop_2 0.07360865 0.0251154 0.1921055 0.9016919 0.7456218 1.056268
# }
```
