* This updates the existing FSA package on CRAN by removing the dependency on the epitools package (related to e-mail from Kurt Hornik on 7-Mar-20), fixing errors related to changes in data.frame (per e-mail from Kurt Hornik on 27-Feb-20), and fixing persistent errors in tests that I should have fixed in v0.8.28 and 0.8.29. I apologize to the CRAN team for missing those errors in previous versions.

## Notes
* There may be a note about "fishR" being misspelled in the description. This is not a misspelling.
* On [CRAN](https://cran.r-project.org/web/checks/check_results_FSA.html), there is a note in the previous version about a missing `RMark` package for the "r-patched-solaris-x86" flavor. Again (as with previous versions), I am not sure what to do about this note.

## Testing Environments
* My Windows machine.
* Win Builder -- old-release, release, and development.
# R-hub using check_for_cran().