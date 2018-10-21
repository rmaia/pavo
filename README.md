# `pavo`

[![cran version](http://www.r-pkg.org/badges/version-ago/pavo)](https://cran.r-project.org/package=pavo/)
[![cran downloads](http://cranlogs.r-pkg.org/badges/grand-total/pavo)](https://cran.r-project.org/package=pavo/)
[![Build Status](https://travis-ci.org/rmaia/pavo.svg?branch=master)](https://travis-ci.org/rmaia/pavo/branches)
[![Coverage status](https://codecov.io/gh/rmaia/pavo/branch/master/graph/badge.svg)](https://codecov.io/github/rmaia/pavo?branch=master)  

## An `R` package for the spectral and spatial analysis of color patterns

### Currently maintained by [Rafael Maia](https://github.com/rmaia), [Thomas White](https://github.com/thomased), and [Hugo Gruson](https://github.com/bisaloo).

## About

A set of functions and tools for the analysis of color data in a unified framework.

### Need help with the package? 

- Click the links above for help with specific functions.
- Check out the [Latest News](http://rafaelmaia.net/pavo/news/index.html) for changes and updates.
- Read the [Package Vignette](http://rafaelmaia.net/pavo/articles/pavo.html) for detailed examples and workflow pipeline.
- **Can't find what you're looking for? Join the chat: [![Join the chat at https://gitter.im/r-pavo/help](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/r-pavo/help)**
  
## Citing pavo

The manuscripts describing the package has been published and are free to access: 

\> v. 2.0

Maia R., Gruson H., Endler J.A. and White T.E. 2018 **pavo 2.0: new tools for the spectral 
and spatial analysis of colour in R**. bioRxiv. [doi: 10.1101/427658](https://doi.org/10.1101/427658)

< v. 2.0

Maia R., Eliason C.M., Bitton P.-P., Doucet S.M. and Shawkey M.D. 2013. 
**pavo: an R Package for the analysis, visualization and organization of spectral data.** 
*Methods in Ecology and Evolution* 4(10):609-613.
[doi: 10.1111/2041-210X.12069](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12069/abstract)  
  
## Install

this is the development page for `pavo`. The stable release is available from CRAN. Simply use `install.packages('pavo')` to install.

If you want to install the bleeding edge version of `pavo`, you can:

* use Hadley Wickham's [devtools](https://github.com/r-lib/devtools):

```r     
install.packages('devtools')
devtools::install_github('rmaia/pavo')
require(pavo)
```

* download files from github and install using `$R CMD INSTALL` or, from within R:

```r
install.packages(path, type='source', repos=NULL)
```
