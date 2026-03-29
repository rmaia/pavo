# Bootstrap colour distance confidence intervals

Uses a bootstrap procedure to generate confidence intervals for the mean
colour distance between two or more samples of colours

## Usage

``` r
bootcoldist(vismodeldata, by, boot.n = 1000, alpha = 0.95, raw = FALSE, ...)
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
  Defaults to FALSE.

- ...:

  other arguments to be passed to
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md). Must
  at minimum include `n` and `weber`. See
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) for
  details.

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

# Run the same again, though as a simple colourspace model
data(sicalis)
vm <- vismodel(sicalis, achromatic = "bt.dc")
space <- colspace(vm)
gr <- gsub("ind..", "", rownames(space))
bootcoldist(space, by = gr)
#> Quantum catch are relative, distances may not be meaningful
#> Calculating unweighted Euclidean distances and Weber luminance contrast
#>        dS.mean      dS.lwr     dS.upr    dL.mean      dL.lwr    dL.upr
#> B-C 0.08873221 0.057492084 0.12509675 1.11017675 0.770862609 1.5208958
#> B-T 0.02510854 0.005678686 0.07323532 0.02368528 0.002474378 0.1646752
#> C-T 0.11208966 0.080721564 0.14476399 1.06135304 0.722669275 1.4676965

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
#>               dS.mean     dS.lwr    dS.upr   dL.mean    dL.lwr   dL.upr
#> pop_1-pop_2 0.0736203 0.02299502 0.2012074 0.9016919 0.7288422 1.057964
# }
```
