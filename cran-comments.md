## Resubmission

This is a resubmission. In this version I have:

* Fixed a test that failed on R-devel because `str()` now consistently inserts a space after `:` (R PR#18719).
* Renamed the `DictS3` and `SetS3` man pages to match roxygen2 output.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is only about HTML Tidy not being installed in the local check
environment.

## Win-builder results

R-devel and R-release checks passed.
