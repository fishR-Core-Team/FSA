context("lwCompPred Messages")

test_that("lowCompPreds errors and warnings",{
  data(ChinookArg)
  ChinookArg$logtl <- log(ChinookArg$tl)
  ChinookArg$logwt <- log(ChinookArg$w)
  ## RHS is a factor (one-way ANOVA)
  tmp <- lm(logwt~loc,data=ChinookArg) 
  expect_error(lwCompPreds(tmp))
  ## RHS has only one variable (SLR)
  tmp <- lm(logwt~logtl,data=ChinookArg)
  expect_error(lwCompPreds(tmp))
  ## Two quantitative variables on RHS
  tmp <- lm(logwt~logtl+tl,data=ChinookArg)
  expect_error(lwCompPreds(tmp))
  ## Problems with the base value
  tmp <- lm(logwt~logtl*loc,data=ChinookArg)
  expect_error(lwCompPreds(tmp,base=-2))
  expect_error(lwCompPreds(tmp,base=0))
  expect_error(lwCompPreds(tmp,base=c(10,exp(1))))
  expect_error(lwCompPreds(tmp,base="derek"))
})
