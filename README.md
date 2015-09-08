FSA
===
[![Travis-CI Build Status](https://travis-ci.org/droglenc/FSA.svg?branch=master)](https://travis-ci.org/droglenc/FSA)
[![Coverage Status](https://img.shields.io/coveralls/droglenc/FSA.svg)](https://coveralls.io/r/droglenc/FSA?branch=master)
[![](http://www.r-pkg.org/badges/version/FSA)](http://www.r-pkg.org/pkg/FSA)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/FSA)](http://www.r-pkg.org/pkg/FSA)


## Introduction
The **FSA** (Fisheries Stock Assessment) package provides R functions to conduct typical introductory fisheries analyses.  Example analyses that use **FSA** can be found on the *Examples* page of the [fishR website](http://derekogle.com/fishR).

You can [browse function help pages here](http://rforge.net/doc/packages/FSA/00Index.html).  Recent changes are described in the [News file](https://github.com/droglenc/FSA/blob/master/NEWS.md)

## Installation
**FSA** has not yet been released on CRAN.  The latest version can be installed from GitHub (requires the **devtools** package) with ...

```r
if (!require('devtools')) install.packages('devtools'); require('devtools')
devtools::install_github('droglenc/FSA')
```

Alternatively, **FSA** can be installed from **RForge.net** with ...

```r
source("http://www.rforge.net/FSA/InstallFSA.R")
```

These installations fail in a few situations.  [Send me an e-mail](mailto:fishr@derekogle.com?Subject=FSA%20Installation%20Question) if you experience difficulties installing **FSA**.
