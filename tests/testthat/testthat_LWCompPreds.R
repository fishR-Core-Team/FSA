## Test Messages ----
test_that("lwCompPred() errors and warnings",{
  ChinookArg$logtl <- log(ChinookArg$tl)
  ChinookArg$logwt <- log(ChinookArg$w)
  ChinookArg$loc2 <- rev(ChinookArg$loc)
  ## RHS is a factor (one-way ANOVA)
  ## RHS has only one variable (SLR)
  expect_error(lwCompPreds(lm(logwt~loc,data=ChinookArg)),
               "two and only two variables")
  expect_error(lwCompPreds(lm(logwt~logtl,data=ChinookArg)),
               "two and only two variables")
  ## Two quantitative variables on RHS
  expect_error(lwCompPreds(lm(logwt~logtl+tl,data=ChinookArg)),
               "one and only one numeric")
  ## Two factor variables on RHS
  expect_error(lwCompPreds(lm(logwt~loc+loc2,data=ChinookArg)),
               "one and only one numeric")
  ## Problems with the base value
  tmp <- lm(logwt~logtl*loc,data=ChinookArg)
  expect_error(lwCompPreds(tmp,base=-2),
               "must be a positive number")
  expect_error(lwCompPreds(tmp,base=0),
               "must be a positive number")
  expect_error(lwCompPreds(tmp,base=c(10,exp(1))),
               "must be a single numeric")
  expect_error(lwCompPreds(tmp,base="derek"),
               "must be a numeric")
})


## Test Output Types ----
test_that("lwCompPred() returns",{
  # Fit a model
  ChinookArg$logtl <- log(ChinookArg$tl)
  ChinookArg$logwt <- log(ChinookArg$w)
  tmp <- lm(logwt~logtl*loc,data=ChinookArg)
  
  tmp2 <- FSA:::iMakeLWPred(tmp,lens=c(10,100,200),grps=levels(ChinookArg$loc),
                            vn=c("logtl","loc"),interval="confidence",
                            center.value=0,base=exp(1))
  expect_is(tmp2,"data.frame")
  expect_equal(mode(tmp2),"list")
  expect_equal(nrow(tmp2),3)
  expect_equal(ncol(tmp2),3)
  expect_equal(names(tmp2),c("pred","LCI","UCI"))
  expect_equal(rownames(tmp2),levels(ChinookArg$loc))
  
  tmp2 <- FSA:::iMakeLWPred(tmp,lens=c(10,100,200),grps=levels(ChinookArg$loc),
                            vn=c("logtl","loc"),interval="prediction",
                            center.value=0,base=exp(1))
  expect_is(tmp2,"data.frame")
  expect_equal(mode(tmp2),"list")
  expect_equal(nrow(tmp2),3)
  expect_equal(ncol(tmp2),3)
  expect_equal(names(tmp2),c("pred","LPI","UPI"))
  expect_equal(rownames(tmp2),levels(ChinookArg$loc))
  
  tmp2 <- FSA:::iMakeLWPred(tmp,lens=c(10,100,200),grps=levels(ChinookArg$loc),
                            vn=c("logtl","loc"),interval="both",
                            center.value=0,base=exp(1))
  expect_is(tmp2,"data.frame")
  expect_equal(mode(tmp2),"list")
  expect_equal(nrow(tmp2),3)
  expect_equal(ncol(tmp2),5)
  expect_equal(names(tmp2),c("pred","LPI","UPI","LCI","UCI"))
  expect_equal(rownames(tmp2),levels(ChinookArg$loc))
})


## Validate Results ----

