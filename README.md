FSA
===
[![CRAN Version](http://www.r-pkg.org/badges/version/FSA)](http://www.r-pkg.org/pkg/FSA)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN RStudio mirror downloads rate](http://cranlogs.r-pkg.org/badges/FSA) ![CRAN RSTudio mirror downloads total](http://cranlogs.r-pkg.org/badges/grand-total/FSA)](http://www.r-pkg.org/pkg/FSA)
[![Travis-CI Build Status](https://travis-ci.org/droglenc/FSA.svg?branch=master)](https://travis-ci.org/droglenc/FSA)
[![Coverage Status](https://img.shields.io/coveralls/droglenc/FSA.svg)](https://coveralls.io/r/droglenc/FSA?branch=master)

## Introduction
The **FSA** (Fisheries Stock Assessment) package provides R functions to conduct typical introductory fisheries analyses.  Example analyses that use **FSA** can be found on [the *Examples* page](http://derekogle.com/fishR/examples/) of the [fishR website](http://derekogle.com/fishR).

You can [browse function help pages here](http://rforge.net/doc/packages/FSA/00Index.html).  Recent changes are described in this [News file](https://github.com/droglenc/FSA/blob/master/NEWS.md)

## Installation
The [CRAN version](https://cran.r-project.org/web/packages/FSA/index.html) of **FSA** may be installed with

```r
install.packages("FSA")
```

The development version may be installed from GitHub (requires the **devtools** package) with

```r
if (!require('devtools')) install.packages('devtools'); require('devtools')
devtools::install_github('droglenc/FSA')
```

This installs the package from the source, so you will need to have R Tools installed on your system.  [R Tools for Windows](https://cran.r-project.org/bin/windows/Rtools/) takes you to the download page for Windows.  [R Tools for Mac OS X](https://cran.r-project.org/bin/macosx/tools/) has the required programs for Mac OS X.

Alternatively, you may try to install from **RForge.net** with

```r
source("http://www.rforge.net/FSA/InstallFSA.R")
```


## Questions / Comments / Problems

Report questions, comments, or bug reports on the [issues page](https://github.com/droglenc/FSA/issues).

## Note About Using Macs
**FSA** uses **TCL/TK** for some interactive plots.  Some Mac users report problems with using **TCL/TK**.  I do not have access to a Mac to test these problems, some students have reported success installing the **TCL/TK** universal build [located here](http://cran.r-project.org/bin/macosx/tools/) (or [direct link to the file](http://cran.r-project.org/bin/macosx/tools/tcltk-8.5.5-x11.dmg)).  You may have to reinstall **FSA** after installing this file.

You should be able to use the vast majority of the functionality in **FSA** even if the problems with **TCL/TK** cannot be rectified.