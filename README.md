[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/18348400.svg)](https://zenodo.org/badge/latestdoi/18348400)
[![CRAN Version](http://www.r-pkg.org/badges/version/FSA)](http://www.r-pkg.org/pkg/FSA)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![R-CMD-check](https://github.com/fishR-Core-Team/FSA/workflows/R-CMD-check/badge.svg)](https://github.com/fishR-Core-Team/FSA/actions)
[![Codecov test coverage](https://codecov.io/gh/fishR-Core-Team/FSA/branch/master/graph/badge.svg)](https://codecov.io/gh/fishR-Core-Team/FSA?branch=master)
[![CRAN RStudio mirror downloads rate](http://cranlogs.r-pkg.org/badges/FSA)
![CRAN RSTudio mirror downloads total](http://cranlogs.r-pkg.org/badges/grand-total/FSA)](http://www.r-pkg.org/pkg/FSA)
[![Rdoc](http://www.rdocumentation.org/badges/version/FSA)](http://www.rdocumentation.org/packages/FSA)

## FSA (Fisheries Stock Assessment)  <img src="man/figures/logo.png" align="right" height="200" hspace="15" />

The **FSA** package provides R functions to conduct typical introductory fisheries analyses. Example analyses that use **FSA** can be found in the [Introductory Fisheries Analyses with R book](http://derekogle.com/IFAR/) (*see note below*) and on [the *Examples* page](http://derekogle.com/fishR/examples/) of the [fishR website](http://derekogle.com/fishR/). You can browse documentation for functions in **FSA** under the *References* tab and recent changes under the *News* tab at [this page](https://fishr-core-team.github.io/FSA/). Please [cite **FSA**](https://fishr-core-team.github.io/FSA//authors.html) if you use **FSA** in a publication.

### Installation
The [most recent stable version (on CRAN)](https://cloud.r-project.org/package=FSA) of **FSA** may be installed with

```r
install.packages("FSA")
```

The most recent development version may be installed from GitHub with

```r
if (!require('remotes')) install.packages('remotes'); require('remotes')
remotes::install_github('fishR-Core-Team/FSA')
```

You may need to have R Tools installed on your system to install the development version from GitHub. See the instructions for ([R Tools for Windows](https://cran.r-project.org/bin/windows/Rtools/) or [R Tools for Mac OS X](https://cran.r-project.org/bin/macosx/tools/)).


### Questions / Comments / Problems or Contributions
Report questions, comments, or bug reports on the [issues page](https://github.com/fishR-Core-Team/FSA/issues).

We are always looking for others to contribute to **FSA**. Please feel free to make a pull request via GitHub or to contact the maintainers.

Please adhere to the [Code of Conduct](https://fishr-core-team.github.io/FSA/CODE_OF_CONDUCT.html).


### Note about **FSA** and *Introduction to Fisheries Analysis with R* book
Versions of **FSA** beginning with v0.9.0 may no longer work as shown in the IFAR book. Many functions have not changed from when the book was published, but some have. Thus, you will need to install an **FSA** version before v0.9.0 to be assured that functions work as described in the IFAR book.
