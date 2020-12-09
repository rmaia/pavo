## Test environments
* local macOS 10.15.7 install, R 4.0.3
* ubuntu 20.04 (on GitHub actions), R 3.6, R 3.5, R 4.0.3, R-devel
* Windows (on GitHub actions), R 4.0.3
* win-builder (R release and devel)
* rhub::check_for_cran()   
* rhub::check_on_solaris()  

## R CMD check results

0 errors | 0 warnings | 3 note

  New submission
  
  Package was archived on CRAN
  
This is a resubmission to address the issue noted by Dr Uwe Ligges, that the package creates bin/phantomjs in the home dir, which necessitated it being archived. Our apologies, this appears to have been the result of a bug when conditionally evaluating some interactive plots in a vignette. We were unable to reproduce locally and it was not caught on our CI systems, so we now simply prevent the evaluation of possibly-problematic chunks in the relevant vignette to be sure of no further issues.

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1515/znc-1993-1-218
    From: inst/doc/pavo-3-analysing.html
    Status: Error
    Message: libcurl error code 35
  URL: https://doi.org/10.2307/3677129
    From: inst/doc/pavo-4-spectraldesc.html
    Status: 403
    Message: Forbidden

These are false positives, as both URLs are valid and resolve correctly on all broswers tested (Safari 14.0, Chrome 86.0.4240.111, Firefox 82.0).



