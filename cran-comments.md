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

Creates bin/phantomjs in home.
  
This is a resubmission to address the issue noted by Dr Uwe Ligges, that the package creates bin/phantomjs in the home dir, which necessitated it being archived. Our apologies, it was the result of a bug when conditionally evaluating some interactive plots in a vignette. We were unable to reproduce locally and it was not caught on our CI systems, so we now simply prevent the evaluation of possibly-problematic chunks in the relevant vignette to be sure of no further issues.


