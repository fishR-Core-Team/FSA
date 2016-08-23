* This updates the existing FSA package on CRAN.

## Notes
* This update largely addresses a concern about the use of exclude= in table() that will occur with R v3.4.0 (this was brought to my attention by CRAN (Martin Maechler)).  I believe that this will address the testing errors shown on the CRAN page for R-Devel (note that this update passed for R-Devel on Win Builder).

* There is a note about "fishR" being misspelled in the description.  This is not a misspelling.

* On CRAN, there were notes in the previous version about a missing `RMark` package for the "r-patched-solaris-sparc" and "r-patched-solaris-x86" flavors.  Again (as with the previous version), I am not sure what to do about this note.

## Testing Environments
* My Windows machine.
* Win Builder -- current and development.
* Travis-CI.