context("depletion() Messages")

test_that("depletion() errors and warnings",{
  ## wrong type
  expect_that(depletion(c(346,184,49),rep(7,3),method="Derek"),throws_error())
  ## no efforts
  expect_that(depletion(c(346,184,49)),throws_error())
  ## bad types of data
  expect_that(depletion(c(346,184,"Derek"),rep(7,3)),throws_error())
  expect_that(depletion(c(346,184,49),rep("Derek",3)),throws_error())
  ## different vector sizes
  expect_that(depletion(c(346,184,49,57),rep(7,3)),throws_error())
  expect_that(depletion(c(346,184,49),rep(7,4)),throws_error())
  ## too few catches
  expect_that(depletion(c(346,184),rep(7,2)),throws_error())
  ## negative catchs or non-non-negative efforts
  expect_that(depletion(c(346,184,-49),rep(7,3)),throws_error())
  expect_that(depletion(c(346,184,49),c(7,3,-1)),throws_error())
  expect_that(depletion(c(346,184,49),c(7,3,0)),throws_error())
  ## zero catches with DeLury method
  expect_that(depletion(c(346,184,0),rep(7,3),method="DeLury"),throws_error())
  ## Bad regressions (i.e., have non-significant or positive slopes)
  expect_that(depletion(c(49,184,346),rep(7,3)),gives_warning())
  expect_that(depletion(c(346,144,341),rep(7,3)),gives_warning())
  expect_that(depletion(c(49,184,346),rep(7,3),method="DeLury"),gives_warning())
  expect_that(depletion(c(346,144,341),rep(7,3),method="DeLury"),gives_warning())
  
  ex1 <- depletion(c(346,184,49),rep(1,3))
  ## wrong type in methods
  expect_that(summary(ex5,type="Derek"),throws_error())
  expect_that(coef(ex5,type="Derek"),throws_error())
  expect_that(confint(ex5,parm="Derek"),throws_error())
})  