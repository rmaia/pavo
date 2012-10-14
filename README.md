# `pavo`

## About

A set of functions and tools for the analysis of color data in a unified framework


## Install

`pavo` is still UNDER DEVELOPMENT, and only has few capabilities implemented.

For now, you can:
* use Hadley Wickham's [devtools](https://github.com/hadley/devtools):
```r     
library(devtools)
install_github("rebird", "ropensci")
```
* download files from github and install using `$R CMD INSTALL` or, from within R, `install.packages(path,type='source', repos=NULL)'

## Working framework

The package works under a conceptual tripod of **Organizing**, **Analyzing** and **Visualizing** color data.

###Organize
* gather
	* general
	* interpolate
	* range of wavelengths
* metadata

(...)

###Analyze
* Trichromatic values (HSL)
	* following Montgomerie 2006
* FWHM
* Segment analysis

(...)

###Visualize
* plot with colors
* heatplots
* tetracolorspace

(...)