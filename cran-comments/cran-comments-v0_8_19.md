* This updates the existing FSA package on CRAN. This largely address some issues related to the updating of the car package to version 3.0.0.

## Notes
* Some functions were updated to be compatible with the soon to be released car v3.0.0 package. I have checked these functions locally on my Windows machine with the new version of car and on Win Builder.
* There is a note about "fishR" being misspelled in the description. This is not a misspelling.
* On [CRAN](https://cran.rstudio.com/), there is a note in the previous version about a missing `RMark` package for the "r-patched-solaris-x86" flavor. Again (as with previous versions), I am not sure what to do about this note.

## Testing Environments
* My Windows machine.
* Win Builder -- old-release, release, and development.
* Travis-CI and AppVeyor.