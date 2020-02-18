# `pavo`

[![cran version](https://www.r-pkg.org/badges/version-ago/pavo)](https://cran.r-project.org/package=pavo/)
[![cran downloads](https://cranlogs.r-pkg.org/badges/grand-total/pavo)](https://cran.r-project.org/package=pavo/)
[![Build Status](https://travis-ci.org/rmaia/pavo.svg?branch=master)](https://travis-ci.org/rmaia/pavo/)
[![cran checks](https://cranchecks.info/badges/worst/pavo)](https://cranchecks.info/pkgs/pavo)

## An `R` package for the spectral and spatial analysis of color patterns

### Currently maintained by [Rafael Maia](https://github.com/rmaia), [Thomas White](https://github.com/thomased), and [Hugo Gruson](https://github.com/bisaloo).

## About

`pavo` is an R package developed with the goal of establishing a flexible and integrated workflow for working with spectral and spatial colour data. It includes functions that take advantage of new data classes to work seamlessly from importing raw spectra and images, to visualisation and analysis. It provides flexible ways to input spectral data from a variety of equipment manufacturers, process these data, extract variables, and produce publication-quality figures.

`pavo` was written with the following workflow in mind:

- **Organise** data by importing and processing spectra and images (e.g., to remove noise, negative values, smooth curves, etc.).
- **Analyse** the resulting files, using spectral analyses of shape (hue, saturation, brightness), visual models based on perceptual data, and/or spatial adjacency and boundary strength analyses.
- **Visualise** the output, with multiple options provided for exploration, presentation, and analysis.

### Need more information, or help with the package?

- Read the [package documentation and vignettes](http://pavo.colrverse.com/index.html) (or via `browseVignettes("pavo")`) for detailed examples and discussion.
- Check out the [latest news](http://pavo.colrverse.com/news/index.html) for changes and updates.
- **Can't find what you're looking for? Send an email to the mailing list: <r-pavo@googlegroups.com>**

## Citing pavo

The manuscript describing the current iteration of the package has been published and are free to access:

Maia R., Gruson H., Endler J.A., and White T.E. 2019 [pavo 2: New tools for the spectral 
and spatial analysis of colour in R](https://doi.org/10.1111/2041-210X.13174). _Methods in Ecology and Evolution_, 10(7):1097‑1107. 


## Install

This is the development page for `pavo`. The stable release is available from CRAN. Simply use `install.packages("pavo")` to install.

If you want to install the bleeding edge version of `pavo`, you can:

* use the [`remotes`](https://github.com/r-lib/remotes) package:

```r
# install.packages("remotes")
remotes::install_github("rmaia/pavo")
```

* download files from GitHub and install using `$R CMD INSTALL` or, from within R:

```r
install.packages(path, type = "source", repos = NULL)
```
