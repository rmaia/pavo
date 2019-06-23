## Test environments
* local OS X install, R 3.5.1
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.5.2 ('release'), R 3.4.4 ('oldrel') and R-devel (2019-03-03 r76191)
* win-builder (R 3.5.2 and R-devel 2018-09-18 r75325)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional notes
All existing problems from the check results have been fixed. In particular `procimg.R`  has been modified to fix the r-devel-linux-x86_64-debian-clang issue.

## Reverse dependencies

I ran R CMD check on the one package that depends on pavo (photobiologyInOut). There were no errors or warnings.

---
