# Colourspace data summary

Returns the attributes of `colspace` objects.

## Usage

``` r
# S3 method for class 'colspace'
summary(object, by = NULL, ...)
```

## Arguments

- object:

  (required) a `colspace` object.

- by:

  when the input is in `tcs` colourspace, `by` is either a single value
  specifying the range of colour points for which summary
  tetrahedral-colourspace variables should be calculated (for example,
  `by` = 3 indicates summary will be calculated for groups of 3
  consecutive colour points (rows) in the quantum catch colour data
  frame) or a vector containing identifications for the rows in the
  quantum catch colour data frame (in which case summaries will be
  calculated for each group of points sharing the same identification).
  If `by` is left blank, the summary statistics are calculated across
  all colour points in the data.

- ...:

  class consistency (ignored).

## Value

returns all attributes of the data as mapped to the selected
colourspace, including options specified when calculating the visual
model. Also return the default `data.frame` summary, except when the
object is the result of
[`tcspace()`](https://pavo.colrverse.com/reference/tcspace.md), in which
case the following variables are output instead:

- `centroid.u, .s, .m, .l` the centroids of `usml` coordinates of
  points.

- `c.vol` the total volume occupied by the points, computed with a
  convex hull.

- `rel.c.vol` volume occupied by the points (convex hull volume)
  relative to the tetrahedron volume.

- `colspan.m` the mean hue span.

- `colspan.v` the variance in hue span.

- `huedisp.m` the mean hue disparity.

- `huedisp.v` the variance in hue disparity.

- `mean.ra` mean saturation.

- `max.ra` maximum saturation achieved by the group of points.

- `a.vol` colour volume computed with \\\alpha\\-shapes.

## References

Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color
in a tetrahedral color space: A phylogenetic analysis of new world
buntings. The American Naturalist, 171(6), 755-776.

Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as
birds see them. Biological Journal Of The Linnean Society, 86(4),
405-431.

Gruson H. (2020). Estimation of colour volumes as concave hypervolumes
using \\\alpha\\-shapes. Methods in Ecology and Evolution, 11(8),
955-963
[doi:10.1111/2041-210X.13398](https://doi.org/10.1111/2041-210X.13398)

## Author

Rafael Maia <rm72@zips.uakron.edu>

## Examples

``` r
# Colour hexagon
data(flowers)
vis.flowers <- vismodel(flowers,
  visual = "apis", qcatch = "Ei", relative = FALSE,
  vonkries = TRUE, bkg = "green"
)
flowers.hex <- hexagon(vis.flowers)
summary(flowers.hex)
#> Colorspace & visual model options:
#>  * Colorspace: hexagon 
#>  * Quantal catch: Ei 
#>  * Visual system, chromatic: apis 
#>  * Visual system, achromatic: none 
#>  * Illuminant: ideal, scale = 1 (von Kries colour correction applied) 
#>  * Background: green 
#>  * Relative: FALSE 
#>  * Max possible chromatic volume: NA 
#> 
#>        s                 m                l                x           
#>  Min.   :0.01889   Min.   :0.1623   Min.   :0.1863   Min.   :-0.27348  
#>  1st Qu.:0.33717   1st Qu.:0.7202   1st Qu.:0.6943   1st Qu.: 0.09593  
#>  Median :0.47192   Median :0.7827   Median :0.8103   Median : 0.27697  
#>  Mean   :0.46048   Mean   :0.7190   Mean   :0.7559   Mean   : 0.25584  
#>  3rd Qu.:0.57925   3rd Qu.:0.8456   3rd Qu.:0.8484   3rd Qu.: 0.40041  
#>  Max.   :0.90735   Max.   :0.9112   Max.   :0.9001   Max.   : 0.66006  
#>        y               h.theta            r.vec            sec.fine    
#>  Min.   :-0.23770   Min.   :  1.103   Min.   :0.09195   Min.   :  0.0  
#>  1st Qu.: 0.06693   1st Qu.: 52.549   1st Qu.:0.23098   1st Qu.: 50.0  
#>  Median : 0.15522   Median : 61.741   Median :0.32881   Median : 60.0  
#>  Mean   : 0.11077   Mean   : 77.206   Mean   :0.34600   Mean   : 72.5  
#>  3rd Qu.: 0.20693   3rd Qu.: 77.511   3rd Qu.:0.45400   3rd Qu.: 72.5  
#>  Max.   : 0.30778   Max.   :271.076   Max.   :0.70155   Max.   :270.0  
#>      sec.coarse
#>  Length   :36  
#>  N.unique : 6  
#>  N.blank  : 0  
#>  Min.nchar: 2  
#>  Max.nchar: 9  
#>                

# Tetrahedral model
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
csp.sicalis <- colspace(vis.sicalis)
summary(csp.sicalis, by = rep(c("C", "T", "B"), 7))
#> Colorspace & visual model options:
#>  * Colorspace: tcs 
#>  * Quantal catch: Qi 
#>  * Visual system, chromatic: avg.uv 
#>  * Visual system, achromatic: none 
#>  * Illuminant: ideal, scale = 1 (von Kries colour correction not applied) 
#>  * Background: ideal 
#>  * Relative: TRUE 
#>  * Max possible chromatic volume: 0.215735 
#> 
#> 'avalue' automatically set to 2.6255e-01
#> 'avalue' automatically set to 2.4445e-02
#> 'avalue' automatically set to 1.6251e-01
#>   centroid.u centroid.s centroid.m centroid.l        c.vol    rel.c.vol
#> B  0.1409130 0.04946432  0.3838526  0.4257701 6.281510e-06 2.901305e-05
#> C  0.0694746 0.03144895  0.4054651  0.4936114 4.739151e-06 2.188920e-05
#> T  0.1536845 0.06413427  0.3766734  0.4055078 5.183720e-06 2.394258e-05
#>    colspan.m    colspan.v  huedisp.m    huedisp.v   mean.ra    max.ra
#> B 0.05758428 0.0013841925 0.06717738 0.0011466888 0.8021427 0.9039261
#> C 0.03193252 0.0003263453 0.06164552 0.0013887684 0.8742042 0.9061528
#> T 0.06171418 0.0012215062 0.05595025 0.0005378621 0.7434629 0.8816378
#>          a.vol
#> B 4.586380e-06
#> C 1.849436e-06
#> T 2.388383e-06
```
