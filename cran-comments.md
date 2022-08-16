## Test environments

* local macOS 12.4 install, R 4.2.1
* macOS (on GitHub actions), R release
* ubuntu 20.04 (on GitHub actions), R 4.1.3, R 4.2.1, R-devel
* Windows (on GitHub actions), R 4.2.1
* win-builder (R release and devel)
* rhub::check_for_cran()   
* rhub::check_on_solaris()  

## R CMD check results

0 errors | 0 warnings | 0 note

## Comments

This release includes a fix for the math rendering problems identified by CRAN checks, as also kindly communicated by email from Kurt Hornik.