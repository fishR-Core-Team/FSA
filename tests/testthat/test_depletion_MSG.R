context("depletion() MESSAGES")

test_that("depletion() messages",{
  ## wrong type
  expect_error(depletion(c(346,184,49),rep(7,3),method="Derek"),"should be one of")
  ## no efforts
  expect_error(depletion(c(346,184,49)),'"effort" is missing')
  ## bad types of data
  expect_error(depletion(c(346,184,"Derek"),rep(7,3)),"must be a numeric vector")
  expect_error(depletion(c(346,184,49),rep("Derek",3)),"must be a numeric vector")
  ## different vector sizes
  expect_error(depletion(c(346,184,49,57),rep(7,3)),"must be same length")
  expect_error(depletion(c(346,184,49),rep(7,4)),"must be same length")
  ## too few catches
  expect_error(depletion(c(346,184),rep(7,2)),"Must have at least 3 values")
  ## negative catchs or non-non-negative efforts
  expect_error(depletion(c(346,184,-49),rep(7,3)),"must be non-negative")
  expect_error(depletion(c(346,184,49),c(7,3,-1)),"must be positive")
  expect_error(depletion(c(346,184,49),c(7,3,0)),"must be positive")
  ## zero catches with DeLury method
  expect_error(depletion(c(346,184,0),rep(7,3),method="DeLury"),"Can't have zero")
  ## Bad regressions (i.e., have non-significant or positive slopes)
  expect_warning(depletion(c(49,184,346),rep(7,3)),"Estimates are suspect")
  expect_warning(depletion(c(346,144,341),rep(7,3)),"Estimates are suspect")
  expect_warning(depletion(c(49,184,346),rep(7,3),method="DeLury"),"Estimates are suspect")
  expect_warning(depletion(c(346,144,341),rep(7,3),method="DeLury"),"Estimates are suspect")
  
  suppressWarnings(ex1 <- depletion(c(346,184,49),rep(1,3)))
  ## wrong type in methods
  expect_error(summary(ex1,parm="Derek"),"should be one of")
  expect_error(coef(ex1,parm="Derek"),"should be one of")
  expect_error(confint(ex1,parm="Derek"),"should be one of")
  expect_error(confint(ex1,conf.level=0),"must be between 0 and 1")
  expect_error(confint(ex1,conf.level=1),"must be between 0 and 1")
})
