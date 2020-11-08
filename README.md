[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/18348400.svg)](https://zenodo.org/badge/latestdoi/18348400)
[![CRAN Version](http://www.r-pkg.org/badges/version/FSA)](http://www.r-pkg.org/pkg/FSA)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/droglenc/FSA?branch=master&svg=true)](https://ci.appveyor.com/project/droglenc/FSA)
[![Travis-CI Build Status](https://travis-ci.org/droglenc/FSA.svg?branch=master)](https://travis-ci.org/droglenc/FSA)
[![Coverage Status](https://img.shields.io/coveralls/droglenc/FSA.svg)](https://coveralls.io/r/droglenc/FSA?branch=master)
[![CRAN RStudio mirror downloads rate](http://cranlogs.r-pkg.org/badges/FSA)
![CRAN RSTudio mirror downloads total](http://cranlogs.r-pkg.org/badges/grand-total/FSA)](http://www.r-pkg.org/pkg/FSA)
[![Rdoc](http://www.rdocumentation.org/badges/version/FSA)](http://www.rdocumentation.org/packages/FSA)

## FSA (Fisheries Stock Assessment)  <img src="man/figures/logo.png" align="right" height="200" hspace="15" />

The **FSA** package provides R functions to conduct typical introductory fisheries analyses. Example analyses that use **FSA** can be found in the [Introductory Fisheries Analyses with R book](http://derekogle.com/IFAR/) and on [the *Examples* page](http://derekogle.com/fishR/examples/) of the [fishR website](http://derekogle.com/fishR/). You can browse documentation for functions in **FSA** under the *References* tab and recent changes under the *News* tab at the top of this page. Please [cite **FSA**](http://derekogle.com/FSA/authors.html) if you use it in a publication (and [send me a note](mailto:derek@derekogle.com)).

### Installation
The [most recent stable version (on CRAN)](https://cloud.r-project.org/package=FSA) of **FSA** may be installed with

```r
install.packages("FSA")
```

The most recent development version (on GitHub) may be installed with

```r
if (!require('remotes')) install.packages('remotes'); require('remotes')
remotes::install_github('droglenc/FSA')
```

You may need to have R Tools installed on your system to install the development version from GitHub. See the instructions for ([R Tools for Windows](https://cran.r-project.org/bin/windows/Rtools/) or [R Tools for Mac OS X](https://cran.r-project.org/bin/macosx/tools/)).


### Questions / Comments / Problems
Report questions, comments, or bug reports on the [issues page](https://github.com/droglenc/FSA/issues).


<!---
## Note About Using Macs
**FSA** uses **TCL/TK** for some interactive plots.  Some Mac users report problems with using **TCL/TK**.  I do not have access to a Mac to test these problems, some students have reported success installing the **TCL/TK** universal build [located here](http://cran.r-project.org/bin/macosx/tools/) (or [direct link to the file](http://cran.r-project.org/bin/macosx/tools/tcltk-8.5.5-x11.dmg)).  You may have to reinstall **FSA** after installing this file.

You should be able to use the vast majority of the functionality in **FSA** even if the problems with **TCL/TK** cannot be rectified.
--->
