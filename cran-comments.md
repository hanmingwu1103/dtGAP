## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Windows 10 x64, R 4.5.0

## Notes

* This is a new submission (first CRAN release).
* The package suggests `InteractiveComplexHeatmap` from Bioconductor, which
  is only used when `interactive = TRUE` and is checked via
  `requireNamespace()` at runtime.
