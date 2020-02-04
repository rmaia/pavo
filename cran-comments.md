## Test environments
* local macOS 10.15.3 install, R 3.6.2
* ubuntu 16.04.6 (on travis-ci), R 3.6.2, R 3.5.3, R devel
* win-builder (R 3.6.2 and devel)
* rhub::check_for_cran()

## R CMD check results

0 errors | 0 warnings | 1 note

* checking package dependencies ... NOTE Suggests orphaned package: ‘imager’

`imager` is only used conditionally in one convenience function for converting between image formats used by `pavo` and `imager`, and we understand there are plans to for `imager` to be actively maintained again in the near future.

