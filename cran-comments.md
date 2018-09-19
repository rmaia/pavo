## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04.5 LTS (on travis-ci), R 3.5.0 ('release'), R 3.4.4 ('oldrelease') and R-devel (2018-09-18 r75325)
* win-builder (R-devel 2018-09-18 r75325)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional notes
This release is being submitted to address the issues resulting in errors under R-oldrelease and platform with long doubles. We have removed the hash tests that were resulting in those errors. We have also reduced the file sizes that were returning notes in solaris.

## Reverse dependencies

I ran R CMD check on the one package that depends on pavo (photobiologyInOut). There were no errors or warnings.

---
