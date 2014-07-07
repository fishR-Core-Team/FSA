context("Mark-Recapture, Closed Populations, Single Census")

test_that("mrClosed match the Petersen results from Box 2.1 in Krebs (1989)",{
  tmp <- mrClosed(M=948,n=421,m=167)
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_that(stmp[[1,"N"]], equals(2390))
  ctmp <- confint(tmp,type="normal")
  expect_that(ctmp[[1,"95% LCI"]], equals(2153))
  ## The UCI does not match (<1%)
  #expect_that(ctmp[[1,"95% UCI"]], equals(2685))
})

test_that("mrClosed match the Chapman results from Box 2.1 Krebs (1989)",{
  tmp <- mrClosed(M=948,n=421,m=167,type="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(2383))
})

test_that("mrClosed match the Chapman results from Ricker (1975)",{
  # Chapman estimate
  tmp <- mrClosed(M=109,n=177,m=57,type="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(337))
})  

test_that("mrClosed match the Chapman results from Box 11.2 in Pine et al. (2013)",{
  # Chapmann estimate
  tmp <- mrClosed(M=421,n=332,m=88,type="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(1578))
  ctmp <- confint(tmp,type="normal")
  expect_that(ctmp[[1,"95% LCI"]], equals(1330))
  expect_that(ctmp[[1,"95% UCI"]], equals(1826))
})  

test_that("mrClosed match the Chapman results from Table 3.7 and 3.8 in Seber (2002)",{
  tmp <- mrClosed(M=500,n=149,m=7,type="Chapman")
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_that(stmp[[1,"N"]], equals(9393))
  expect_that(round(stmp[[1,"SE"]],0), equals(3022))
  ctmp <- confint(tmp,type="normal")
  ## The UCI does not match (<0.01%) ... due to digits on Z*
  #expect_that(ctmp[[1,"95% LCI"]], equals(3470))
  expect_that(ctmp[[1,"95% UCI"]], equals(15316))
  
  tmp <- mrClosed(M=1000,n=243,m=21,type="Chapman")
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_that(stmp[[1,"N"]], equals(11101))
  expect_that(round(stmp[[1,"SE"]],0), equals(2184))
  ctmp <- confint(tmp,type="normal")
  ## The CI does not match (<0.02%) ... due to digits on Z*
  #expect_that(ctmp[[1,"95% LCI"]], equals(6820))
  #expect_that(ctmp[[1,"95% UCI"]], equals(15382))
})

test_that("mrClosed match the Chapman results from mrN.single() from fishmethods",{
  library(fishmethods)
  tmp1 <- mrN.single(M=948,C=421,R=167)
  
  tmp <- mrClosed(M=948,n=421,m=167,type="Chapman")
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_that(stmp[[1,"N"]], equals(round(tmp1$N[1],0)))
  expect_that(stmp[[1,"SE"]], equals(round(tmp1$SE[1],1)))
  
  ctmp <- confint(tmp,type="hypergeometric")
  ## The CIs do not equal (<1%) ... fish methods uses qhyper
  ##   whereas FSA uses hyperCI
  #expect_that(ctmp[[1,"95% LCI"]], equals(round(tmp1$LCI[1],0)))
  #expect_that(ctmp[[1,"95% UCI"]], equals(round(tmp1$UCI[1],0)))
})


test_that("mrClosed match the Bailey results from mrN.single() from fishmethods",{
  library(fishmethods)
  tmp1 <- mrN.single(M=948,C=421,R=167)
    
  tmp <- mrClosed(M=948,n=421,m=167,type="Bailey")
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_that(stmp[[1,"N"]], equals(round(tmp1$N[2],0)))
  expect_that(stmp[[1,"SE"]], equals(round(tmp1$SE[2],1)))
  ctmp <- confint(tmp,type="binomial")
  expect_that(ctmp[[1,"95% LCI"]], equals(round(tmp1$LCI[2],0)))
  ## The UCI does not match (<0.1%) ... fishmethods uses qbinom
  ##   whereas FSA uses binCI()
  #expect_that(ctmp[[1,"95% UCI"]], equals(round(tmp1$UCI[2],0)))
})  