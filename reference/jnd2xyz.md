# Convert JND distances into perceptually-corrected Cartesian coordinates

Converts a
[`coldist()`](https://pavo.colrverse.com/reference/coldist.md) output
into Cartesian coordinates that are perceptually-corrected (i.e.
noise-weighted Euclidean distances)

## Usage

``` r
jnd2xyz(
  coldistres,
  center = TRUE,
  rotate = TRUE,
  rotcenter = c("mean", "achro"),
  ref1 = "l",
  ref2 = "u",
  axis1,
  axis2
)
```

## Arguments

- coldistres:

  (required) the output from a
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) call.

- center:

  logical indicating if the data should be centered on its centroid
  (defaults to `TRUE`).

- rotate:

  logical indicating if the data should be rotated (defaults to `TRUE`).

- rotcenter:

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

## References

Pike, T.W. (2012). Preserving perceptual distances in chromaticity
diagrams. Behavioral Ecology, 23, 723-728.

Maia, R., White, T. E., (2018) Comparing colors using visual models.
Behavioral Ecology, ary017
[doi:10.1093/beheco/ary017](https://doi.org/10.1093/beheco/ary017)

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
# noise-weighted.
jnd2xyz(cd.flowers)
#>                                      x          y           z
#> Goodenia_heterophylla      1.312355137 -5.3667601   6.8629524
#> Goodenia_geniculata       -0.648798700 -2.5312331  -6.5456726
#> Goodenia_gracilis         -4.834850372 10.6831212 -22.9236622
#> Xyris_operculata           3.150779009 -1.0781817  -2.8997962
#> Eucalyptus_sp             -0.082973918 -3.1963203   3.1550303
#> Faradaya_splendida        -1.266831951  0.4892355  -2.8528294
#> Gaultheria_hispida         2.274592892 -3.9525120  -1.1742262
#> Geitonoplesium_cymosum     8.033650770 -6.9198494   8.2932612
#> Euryomyrtus_ramosissima   -2.010687966 -0.3710342   4.6922247
#> Genista_linifolia         -0.617218669 -1.7069294  -2.1501789
#> Genista_monspessulana     -0.202733928 -3.0304197  -2.4820412
#> Geranium_sp                7.705016804  6.7723067  10.8297259
#> Glycine_clandestina       -0.172096284 -2.6365121  -4.2701064
#> Gompholobium_ecostatum_1   2.342604074  0.1315023   5.5688334
#> Gompholobium_ecostatum_2  -0.394030123 -2.0067259  -5.3306567
#> Gompholobium_grandiflorum -3.043177182  4.4548815  -6.5433920
#> Gompholobium_huegelii      0.731726587 -1.7597445   6.1230907
#> Gompholobium_virgatum      1.595663823 -1.1755990   5.9025137
#> Gonocarpus_humilis         0.047486608 -3.7116414  -2.6939291
#> Gonocarpus_teucrioides    -0.143737261 -2.1236747   2.1280934
#> Hibbertia_obtusifolia     -0.546715949 -1.3648630  -2.0026397
#> Zieria_arborescens        -4.374219169  8.9888446   8.7021306
#> Goodenia_lanata           -4.221201078  7.4641726 -12.6997701
#> Goodenia_ovata            -3.131626826 10.5294641   9.6861976
#> Goodenia_rotundifolia      2.743882379 -6.2826784   6.1909786
#> Grevillea_buxifolia        2.422158828 -3.4693196   1.9123426
#> Grevillea_steiglitziana   -4.435969376  5.1580526  -1.1327863
#> Grevillea_oleoides        -0.165755432 -3.0178116  -4.1570956
#> Gymnostachys_anceps       -4.457785440  9.9350339  -9.8694685
#> Hakea_actites              1.020732338 -6.3835620   8.4297774
#> Hardenbergia_violaceae    -1.543741054  0.4977253  -3.4332417
#> Hibbertia_acicularis      -0.009957462 -3.4147937   1.9458170
#> Hibbertia_bracteata        7.213297113 -2.2921695   1.6605064
#> Hibbertia_empetrifolia    -3.739762049  8.0698186   8.8247528
#> Hibbertia_procumbens       0.056155445 -3.3090457  -0.3154626
#> Hibbertia_linearis        -0.606231617 -2.0727778  -7.4312733
```
