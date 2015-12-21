* This is an update to the existing FSA package.

* This update addresses the error with the development versions of R on CRAN that appeared to be related to my use of non-list `dimnames` in unit tests.

* Other changes in this update are noted in the NEWS.md file.

* This update passes the release and development versions on win-builder (though warnings suggest that some packages (psych, DescTools, and asbio) optionally used in the tests are not avaiable).

* Additionally there were notes about a missing RMark package for the "r-patched-solaris-sparc" and "r-patched-solaris-x86" flavors.  Again (as with the previous version), I am not sure what to do about these notes.
