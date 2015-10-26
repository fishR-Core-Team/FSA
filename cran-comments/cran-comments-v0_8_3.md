* This is a resubmission of the submission from earlier today.  This version has a remnant VignetteBuilder field in the DESCRIPTION file removed per your request.  Comments from the previous submission are below in case they are still relevant.

----

* This is an update to an existing package.

* It passed the release and development versions on win-builder (checked again).

* It appears that there was an error for previous version of this package on the "r-devel-windows-ix86+x86_64" flavor.  This error appears to be related to missing gdata and testthat packages.  I asked about this problem on the "r-pkg-devel" mailing list and it was suggested that this was "a temporary problem on the CRAN end, and if your package is ok on win builder, you should be ok to submit."

* Additionally there were notes about a missing RMark package for the "r-patched-solaris-sparc" and "r-patched-solaris-x86" flavors.  Again, I am not sure what to do about these notes.
