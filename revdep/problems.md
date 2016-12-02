# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.2 (2016-10-31) |
|system   |x86_64, darwin13.4.0         |
|ui       |AQUA                         |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/New_York             |
|date     |2016-12-02                   |

## Packages

|package |*  |version |date       |source                     |
|:-------|:--|:-------|:----------|:--------------------------|
|knitr   |   |1.15.1  |2016-11-22 |cran (@1.15.1)             |
|pavo    |   |1.0.0   |2016-12-02 |local (rmaia/pavo@a7ceb5c) |
|rgl     |   |0.96.0  |2016-08-25 |cran (@0.96.0)             |

# Check results
1 packages with problems

## photobiologyInOut (0.4.12)
Maintainer: Pedro J. Aphalo <pedro.aphalo@helsinki.fi>  
Bug reports: https://bitbucket.org/aphalo/photobiologyinout/

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Read 8 items
Read 8 items
Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Kumpula,%20Helsinki,%20Finland&sensor=false
Read 4 items
Read 18 items
Read 4 items
Read 18 items
... 8 lines ...
cols(
  W = col_double(),
  D = col_double(),
  S = col_double(),
  P = col_double()
)
Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Vikki,%2000790%20Helsinki,%20Finland&sensor=false
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
  Running 'texi2dvi' on 'user-guide.tex' failed.
Calls: buildVignettes -> texi2pdf -> texi2dvi
Execution halted
```

