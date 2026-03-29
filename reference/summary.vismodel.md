# Visual model summary

Returns the attributes used when calculating a visual model using
[`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md)

## Usage

``` r
# S3 method for class 'vismodel'
summary(object, ...)
```

## Arguments

- object:

  (required) Results of
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md)

- ...:

  class consistency (ignored)

## Value

Returns all attributes chosen when calculating the visual model, as well
as the default `data.frame` summary

## References

Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I.
(1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of
Comparative Physiology A-Neuroethology Sensory Neural And Behavioral
Physiology, 183(5), 621-633.

Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress
In Retinal And Eye Research, 20(5), 675-703.

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
summary(vis.sicalis)
#> visual model options:
#>  * Quantal catch: Qi 
#>  * Visual system, chromatic: avg.uv 
#>  * Visual system, achromatic: none 
#>  * Illuminant: ideal, scale = 1 (von Kries colour correction not applied) 
#>  * Background: ideal 
#>  * Transmission: ideal 
#>  * Relative: TRUE 
#>        u                 s                 m                l         
#>  Min.   :0.05187   Min.   :0.02346   Min.   :0.3409   Min.   :0.3572  
#>  1st Qu.:0.09842   1st Qu.:0.02815   1st Qu.:0.3794   1st Qu.:0.4135  
#>  Median :0.12547   Median :0.03891   Median :0.3940   Median :0.4368  
#>  Mean   :0.12136   Mean   :0.04835   Mean   :0.3887   Mean   :0.4416  
#>  3rd Qu.:0.15914   3rd Qu.:0.05488   3rd Qu.:0.4057   3rd Qu.:0.4779  
#>  Max.   :0.18836   Max.   :0.12580   Max.   :0.4160   Max.   :0.5082  
#>    lum         
#>  Mode:logical  
#>  NA's:21       
#>                
#>                
#>                
#>                
```
