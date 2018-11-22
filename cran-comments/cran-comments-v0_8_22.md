* This updates the existing FSA package on CRAN by fixing errors that appeared and were caused by recent changes in the dependent fishmethods package. This address the directive sent to me from Kurt Hornik on 22-Nov-18.

## Notes
* There may be a note about "fishR" being misspelled in the description. This is not a misspelling.
* On [CRAN](https://cran.r-project.org/web/checks/check_results_FSA.html), there is a note in the previous version about a missing `RMark` package for the "r-patched-solaris-x86" flavor. Again (as with previous versions), I am not sure what to do about this note.

## Testing Environments
* My Windows machine.
* Win Builder -- old-release, release, and development.
* Travis-CI and AppVeyor.