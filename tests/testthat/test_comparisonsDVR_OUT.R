context("DVR Model Comparisons OUTPUT")
source("EXS_comparisonsDVR.R")

test_that("Same results with compSlopes() & compIntercepts() if variables are reversed",{
  tmp1 <- lm(mirex~weight*fyear,data=Mirex)
  tmp2 <- lm(mirex~fyear*weight,data=Mirex)
  expect_identical(compSlopes(tmp1),compSlopes(tmp2))
  expect_identical(suppressWarnings(compIntercepts(tmp1)),
                   suppressWarnings(compIntercepts(tmp2)))
})  