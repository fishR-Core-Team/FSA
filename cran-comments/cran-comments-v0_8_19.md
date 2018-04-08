* This updates the existing FSA package on CRAN to address issues related to updates of the `car` package (to version 3.0.0).

## Notes
* There is a note about "fishR" being misspelled in the description. This is not a misspelling.
* On [CRAN](https://cran.r-project.org/web/checks/check_results_FSA.html), there is a warning in the previous version about `bootCase()` from the `car` package being deprecated. This version is primarily meant to address that issue (which it does under all methods of testing available to me ... see below).
* On [CRAN](https://cran.r-project.org/web/checks/check_results_FSA.html), there is a note in the previous version about a missing `RMark` package for the "r-patched-solaris-x86" flavor. Again (as with previous versions), I am not sure what to do about this note.
* In a previous submission there was a note about the `DescTools` package existing but not installed for R >2.10.0. My package only links to this package as a cross-reference in the documentation. I am not sure what to do about this note.

## Testing Environments
* My Windows machine.
* Win Builder -- old-release, release, and development.
* Travis-CI and AppVeyor.