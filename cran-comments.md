# Submission 2 

Previous submission found:
> * checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘revdep’  

I have now removed the directory, and added reference to it to .Rbuildignore. No errors or warnings or other notes found in the test environments listed below.

# Submision 1

## Test environments
* local OS X install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

I ran R CMD check on the one package that depends on pavo (photobiologyInOut). There were no errors or warnings.

---