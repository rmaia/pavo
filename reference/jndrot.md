# Rotate Cartesian coordinates obtained from [`jnd2xyz()`](https://pavo.colrverse.com/reference/jnd2xyz.md)

Rotate Cartesian coordinates obtained from
[`jnd2xyz()`](https://pavo.colrverse.com/reference/jnd2xyz.md)

## Usage

``` r
jndrot(
  jnd2xyzres,
  center = c("mean", "achro"),
  ref1 = "l",
  ref2 = "u",
  axis1 = c(1, 1, 0),
  axis2 = c(0, 0, 1)
)
```

## Arguments

- jnd2xyzres:

  (required) the output from a
  [`jnd2xyz()`](https://pavo.colrverse.com/reference/jnd2xyz.md) call.

- center:

  should the vectors for rotation be centered in the achromatic center
  ("achro") or the data centroid ("mean", the default)?

- ref1:

  the cone to be used as a the first reference. May be `NULL` (for no
  first rotation in the 3-dimensional case) or must match name in the
  original data that was used for
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md).
  Defaults to 'l'.

- ref2:

  the cone to be used as a the second reference. May be `NULL` (for no
  first rotation in the 3-dimensional case) or must match name in the
  original data that was used for
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md).
  Defaults to 'u'. (only used if data has 3 dimensions).

- axis1:

  A vector of length number of cones minus 1 composed of 0's and 1's,
  with 1's representing the axes (x, y, z) to rotate around. Defaults to
  c(1, 1, 0) in 3 dimensions, such that the rotation aligns with the xy
  plane, and c(1, 0) in 2 dimentions, such that the rotation is centered
  on the x axis. Ignored if `ref1` is `NULL` (in 3-dimensional case
  only). Ignored for dichromats.

- axis2:

  A vector of length number of cones minus 1 composed of 0's and 1's,
  with 1's representing the axes (x, y, z) to rotate around. Defaults to
  c(0, 0, 1) in 3 dimensions, such that the rotation aligns with the z
  axis, and c(0, 1) in 2 dimentions, such that the rotation is centered
  on the y axis. Ignored if `ref1` is `NULL` (in 3-dimensional case
  only). Ignored for dichromats.

## Author

Rafael Maia <rm72@zips.uakron.edu>

## Examples

``` r
# Load floral reflectance spectra
data(flowers)

# Estimate quantum catches visual phenotype of a Blue Tit
vis.flowers <- vismodel(flowers, visual = "bluetit")

# Estimate noise-weighted colour distances between all flowers
cd.flowers <- coldist(vis.flowers)
#> Quantum catch are relative, distances may not be meaningful
#> Calculating noise-weighted Euclidean distances

# Convert points to Cartesian coordinates in which Euclidean distances are
# noise-weighted, before rotating them about the data centroid
jndrot(jnd2xyz(cd.flowers))
#>                                    x            y           z
#> Goodenia_heterophylla      0.3043555 -5.516498701   6.8629524
#> Goodenia_geniculata       -1.1026561 -2.369014577  -6.5456726
#> Goodenia_gracilis         -2.7905115 11.389376711 -22.9236622
#> Xyris_operculata           2.8991598 -1.638522717  -2.8997962
#> Eucalyptus_sp             -0.6686084 -3.126709336   3.1550303
#> Faradaya_splendida        -1.1554277  0.713583456  -2.8528294
#> Gaultheria_hispida         1.5099700 -4.303035463  -1.1742262
#> Geitonoplesium_cymosum     6.6260719 -8.277622401   8.2932612
#> Euryomyrtus_ramosissima   -2.0446299  0.004566434   4.6922247
#> Genista_linifolia         -0.9202192 -1.564532987  -2.1501789
#> Genista_monspessulana     -0.7558614 -2.941635249  -2.4820412
#> Geranium_sp                8.8177712  5.241977926  10.8297259
#> Glycine_clandestina       -0.6533986 -2.560055351  -4.2701064
#> Gompholobium_ecostatum_1   2.3269068 -0.300984437   5.5688334
#> Gompholobium_ecostatum_2  -0.7558888 -1.900221252  -5.3306567
#> Gompholobium_grandiflorum -2.1732134  4.938019815  -6.5433920
#> Gompholobium_huegelii      0.3960794 -1.864201051   6.1230907
#> Gompholobium_virgatum      1.3526063 -1.448665692   5.9025137
#> Gonocarpus_humilis        -0.6350125 -3.657225209  -2.6939291
#> Gonocarpus_teucrioides    -0.5313327 -2.061150168   2.1280934
#> Hibbertia_obtusifolia     -0.7880908 -1.241234118  -2.0026397
#> Zieria_arborescens        -2.6488920  9.639320108   8.7021306
#> Goodenia_lanata           -2.7785027  8.112480123 -12.6997701
#> Goodenia_ovata            -1.1444822 10.925514238   9.6861976
#> Goodenia_rotundifolia      1.5433111 -6.679755139   6.1909786
#> Grevillea_buxifolia        1.7437703 -3.855164992   1.9123426
#> Grevillea_steiglitziana   -3.4131666  5.885033918  -1.1327863
#> Grevillea_oleoides        -0.7171963 -2.936033254  -4.1570956
#> Gymnostachys_anceps       -2.5572567 10.584762035  -9.8694685
#> Hakea_actites             -0.1690555 -6.462443652   8.4297774
#> Hardenbergia_violaceae    -1.4260671  0.772786755  -3.4332417
#> Hibbertia_acicularis      -0.6369595 -3.354876726   1.9458170
#> Hibbertia_bracteata        6.6696068 -3.577994067   1.6605064
#> Hibbertia_empetrifolia    -2.1940185  8.619401043   8.8247528
#> Hibbertia_procumbens      -0.5525492 -3.263070039  -0.3154626
#> Hibbertia_linearis        -0.9766118 -1.926175986  -7.4312733
```
