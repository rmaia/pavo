## Resbumission

This is a resubmission to correct two points:

* We now reset the user's par() settings in all plotting functions, 
examples, and vignettes, by saving their settings with par(no.readonly = TRUE) 
and immediately calling on.exit() before making any changes.

* The package was previously archived for creating bin/phantomjs in home.
This was the result of a bug in a vignette chunk, which has now been
corrected.

## Test environments

* local macOS 10.15.7 install, R 4.0.3
* ubuntu 20.04 (on GitHub actions), R 3.6, R 3.5, R 4.0.3, R-devel
* Windows (on GitHub actions), R 4.0.3
* win-builder (R release and devel)
* rhub::check_for_cran()   
* rhub::check_on_solaris()  

## R CMD check results

0 errors | 0 warnings | 1 note

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-12-09 for policy violation.
  
As above, the package was archived for creating bin/phantomjs in home,
and this has now been corrected.