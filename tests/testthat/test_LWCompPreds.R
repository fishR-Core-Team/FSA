context("lwCompPred Messages")

test_that("lowCompPreds errors and warnings",{
  data(ChinookArg)
  ChinookArg$logtl <- log(ChinookArg$tl)
  ChinookArg$logwt <- log(ChinookArg$w)
  ## RHS is a factor (one-way ANOVA)
  tmp <- lm(logwt~loc,data=ChinookArg) 
  expect_that(lwCompPreds(tmp),throws_error())
  ## RHS has only one variable (SLR)
  tmp <- lm(logwt~logtl,data=ChinookArg)
  expect_that(lwCompPreds(tmp),throws_error())
  ## Two quantitative variables on RHS
  tmp <- lm(logwt~logtl+tl,data=ChinookArg)
  expect_that(lwCompPreds(tmp),throws_error())
  
})
