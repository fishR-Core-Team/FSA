context("lwCompPred() MESSAGES")

test_that("lwCompPred() errors and warnings",{
  data(ChinookArg)
  ChinookArg$logtl <- log(ChinookArg$tl)
  ChinookArg$logwt <- log(ChinookArg$w)
  ChinookArg$loc2 <- rev(ChinookArg$loc)
  ## RHS is a factor (one-way ANOVA)
  ## RHS has only one variable (SLR)
  expect_error(lwCompPreds(lm(logwt~loc,data=ChinookArg)),"two and only two variables")
  expect_error(lwCompPreds(lm(logwt~logtl,data=ChinookArg)),"two and only two variables")
  ## Two quantitative variables on RHS
  expect_error(lwCompPreds(lm(logwt~logtl+tl,data=ChinookArg)),"one and only one numeric")
  ## Two factor variables on RHS
  expect_error(lwCompPreds(lm(logwt~loc+loc2,data=ChinookArg)),"one and only one numeric")
  ## Problems with the base value
  tmp <- lm(logwt~logtl*loc,data=ChinookArg)
  expect_error(lwCompPreds(tmp,base=-2),"must be a positive number")
  expect_error(lwCompPreds(tmp,base=0),"must be a positive number")
  expect_error(lwCompPreds(tmp,base=c(10,exp(1))),"must be a single numeric")
  expect_error(lwCompPreds(tmp,base="derek"),"must be a numeric")
})
