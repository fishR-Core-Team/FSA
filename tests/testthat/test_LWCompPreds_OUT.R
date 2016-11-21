context("lwCompPred Tests")

test_that("lwCompPred() returns",{
  # Fit a model
  data(ChinookArg)
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
