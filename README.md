# `pavo`

[![cran version](http://www.r-pkg.org/badges/version/pavo)](https://cran.r-project.org/package=pavo/)
![cran downloads](http://cranlogs.r-pkg.org/badges/grand-total/pavo)  
master branch (current version with minor changes):  [![Build Status](https://travis-ci.org/rmaia/pavo.svg?branch=master)](https://travis-ci.org/rmaia/pavo/branches)  
revamp branch (version 1.0 - bleeding edge) : [![Build Status](https://travis-ci.org/rmaia/pavo.svg?branch=revamp)](https://travis-ci.org/rmaia/pavo/branches)

**Need help with the package?** Join the chat: [![Join the chat at https://gitter.im/r-pavo/help](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/r-pavo/help?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

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
