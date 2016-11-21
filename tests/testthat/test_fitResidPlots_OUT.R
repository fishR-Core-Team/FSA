context("fit and residual plots OUTPUT")
source("EXS_fitResidPlots.R")

test_that("iGetMainTitle() returns",{
  tmp <- FSA:::iTypeoflm(lm(y1~x2,data=df))
  expect_equal(FSA:::iGetMainTitle(tmp,main=""),"")
  expect_equal(FSA:::iGetMainTitle(tmp,main="Derek"),"Derek")
  expect_equal(FSA:::iGetMainTitle(tmp,main="MODEL"),"y1~x2")
  tmp <- FSA:::iTypeoflm(lm(y1~x2*x1,data=df))
  expect_equal(FSA:::iGetMainTitle(tmp,main=""),"")
  expect_equal(FSA:::iGetMainTitle(tmp,main="Derek"),"Derek")
  expect_equal(FSA:::iGetMainTitle(tmp,main="MODEL"),"y1~x2*x1")
})