# `pavo`

## About

A set of functions and tools for the analysis of color data in a unified framework

### Citing pavo

The manuscript describing the package has been published and is free to access: 

Maia R., Eliason C.M., Bitton P.-P., Doucet S.M. and Shawkey M.D. 2013. 
**pavo: an R Package for the analysis, visualization and organization of spectral data.** 
*Methods in Ecology and Evolution* 4(10):609-613. [doi: 10.1111/2041-210X.12069]
(http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12069/abstract)

## Install

this is the development page for `pavo`. The stable release is available from CRAN. Simply use `install.packages('pavo')` to install.

If you want to install the bleeding edge version of `pavo`, you can:

* use Hadley Wickham's [devtools](https://github.com/hadley/devtools):

```r     
install.packages('devtools')
require(devtools)
install_github('pavo', 'rmaia')
require(pavo)
```

* download files from github and install using `$R CMD INSTALL` or, from within R:

```r
install.packages(path,type='source', repos=NULL)
```
