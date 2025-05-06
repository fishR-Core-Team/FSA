# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(FSA))

# packages used in tests (load here instead of in individual files)
suppressPackageStartupMessages(library(FSAdata))
suppressPackageStartupMessages(library(fishmethods))
#suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(dunn.test))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(nlme))
#suppressPackageStartupMessages(library(psych))
#suppressPackageStartupMessages(library(tibble))

# Test the package
test_check("FSA")
