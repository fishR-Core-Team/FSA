FSA
===
[![CRAN Version](http://www.r-pkg.org/badges/version/FSA)](http://www.r-pkg.org/pkg/FSA)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/FSA)](http://www.r-pkg.org/pkg/FSA)
[![Travis-CI Build Status](https://travis-ci.org/droglenc/FSA.svg?branch=master)](https://travis-ci.org/droglenc/FSA)
[![Coverage Status](https://img.shields.io/coveralls/droglenc/FSA.svg)](https://coveralls.io/r/droglenc/FSA?branch=master)


## Introduction
The **FSA** (Fisheries Stock Assessment) package provides R functions to conduct typical introductory fisheries analyses.  Example analyses that use **FSA** can be found on the *Examples* page of the [fishR website](http://derekogle.com/fishR).

You can [browse function help pages here](http://rforge.net/doc/packages/FSA/00Index.html).  Recent changes are described in this [News file](https://github.com/droglenc/FSA/blob/master/NEWS.md)

## Installation
**FSA** is available [on CRAN](https://cran.r-project.org/web/packages/FSA/index.html) and can be installed like any other package.

The development version can be installed from GitHub (requires the **devtools** package) with ...

```r
if (!require('devtools')) install.packages('devtools'); require('devtools')
devtools::install_github('droglenc/FSA')
```

Alternatively, **FSA** can be installed from **RForge.net** with ...

```r
source("http://www.rforge.net/FSA/InstallFSA.R")
```

## Questions / Comments / Problems

Report questions, comments, or bug reports on the [issues page](https://github.com/droglenc/FSA/issues).

## Note About Using Macs
**FSA** uses **TCL/TK** for some interactive plots.  Some Mac users report problems with using **TCL/TK**.  I do not have access to a Mac to test these problems, some students have reported success installing the **TCL/TK** universal build [located here](http://cran.r-project.org/bin/macosx/tools/) (or [direct link to the file](http://cran.r-project.org/bin/macosx/tools/tcltk-8.5.5-x11.dmg)).  You may have to reinstall **FSA** after installing this file.

You should be able to use the vast majority of the functionality in **FSA** even if the problems with **TCL/TK** cannot be rectified.