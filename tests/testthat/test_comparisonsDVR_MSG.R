context("DVR Model Comparisons (slopes and intercepts) MESSAGES")
source("EXS_comparisonsDVR.R")

test_that("compSlopes() & compIntercepts() mesages",{
  ## Model not a DVR/IVR
  # SLR
  tmp <- lm(mirex~weight,data=Mirex)
  expect_error(compSlopes(tmp),"only works for dummy")
  expect_error(compIntercepts(tmp),"only works for dummy")
  # 1-way ANOVA
  tmp <- lm(mirex~fyear,data=Mirex)
  expect_error(compSlopes(tmp),"only works for dummy")
  expect_error(compIntercepts(tmp),"only works for dummy")
  # 2-way ANOVA
  tmp <- lm(mirex~fyear*species,data=Mirex)
  expect_error(compSlopes(tmp),"only works for dummy")
  expect_error(compIntercepts(tmp),"only works for dummy")
  # multiple linear regression (not DVR/IVR)
  tmp <- lm(mirex~year+weight,data=Mirex)
  expect_error(compSlopes(tmp),"only works for dummy")
  expect_error(compIntercepts(tmp),"only works for dummy")
  # 2-way IVR/DVR
  tmp <- lm(mirex~weight*year*species,data=Mirex)
  expect_error(compSlopes(tmp),"with one factor and one covariate")
  expect_error(compIntercepts(tmp),"with one factor and one covariate")
  
  ## Testing intercepts with an interaction in the model
  tmp <- lm(mirex~weight*fyear,data=Mirex)
  expect_warning(compIntercepts(tmp),"Removed an interaction")
  
  ## Only two groups/levels ... function not needed
  Mirex2 <- filterD(Mirex,year %in% c(1977,1982))
  tmp <- lm(mirex~weight*fyear,data=Mirex2)
  expect_warning(compSlopes(tmp),"Function not needed with fewer than three levels")
  expect_warning(compIntercepts(tmp),"Function not needed with fewer than three levels")
})
