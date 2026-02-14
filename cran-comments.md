## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Windows 10 x64, R 4.5.0

## Notes

* This is an update from 0.0.1 to 0.0.2.
* Replaced vignette Rmd source with pre-built HTML in inst/doc to reduce
  build time. Removed knitr and rmarkdown from Suggests.
* The package suggests `InteractiveComplexHeatmap` from Bioconductor, which
  is only used when `interactive = TRUE` and is checked via
  `requireNamespace()` at runtime.
