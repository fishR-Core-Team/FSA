context("nlsTracePlot() OUTPUT")
source("EXS_nlsTracePlot.R")

test_that("nlsTracePlot() test output",{
  # successful fit
  tmp <- nlsTracePlot(fit1,vb1,add=FALSE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(ncol(tmp),4)
  # unsuccessful fit
  tmp <- nlsTracePlot(trc,bh1,add=FALSE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(ncol(tmp),3)
})
  