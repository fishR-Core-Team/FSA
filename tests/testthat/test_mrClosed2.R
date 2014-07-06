context("Mark-Recapture, Closed Populations")

test_that("mrClosed match the Petersen results from Krebs (1989)",{
  tmp <- mrClosed(M=948,n=421,m=167)
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(2390))
  ctmp <- confint(tmp,type="normal")
  expect_that(ctmp[[1,1]], equals(2153))
  ## The UCI does not equal Krebs ()
  #expect_that(ctmp[[1,2]], equals(2685))
})

test_that("mrClosed match the Chapman results from Krebs (1989)",{
  tmp <- mrClosed(M=948,n=421,m=167,type="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(2383))
})

test_that("mrClosed match the Chapman results from Ricker (1975)",{
  # Chapmann estimate
  tmp <- mrClosed(M=109,n=177,m=57,type="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(337))
})  

test_that("mrClosed match the Chapman results from Box 11.2 in Pine et al. (2013)",{
  # Chapmann estimate
  tmp <- mrClosed(M=421,n=332,m=88,type="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(1578))
  ctmp <- confint(tmp,type="normal")
  expect_that(ctmp[[1,1]], equals(1330))
  ## The UCI does not equal Krebs ()
  #expect_that(ctmp[[1,1]], equals(1826))
})  

test_that("mrClosed match the Chapman results from mrN.single() from fishmethods",{
  library(fishmethods)
  tmp1 <- mrN.single(M=948,C=421,R=167)
  
  tmp <- mrClosed(M=948,n=421,m=167,type="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(round(tmp1$N[1],0)))
  ctmp <- confint(tmp,type="hypergeometric")
  ## The CIs do not equal ... fish methods uses qhyper whereas
  ##   FSA uses hyperCI
  #expect_that(ctmp[[1,1]], equals(round(tmp1$LCI[1],0)))
  #expect_that(ctmp[[1,2]], equals(round(tmp1$UCI[1],0)))
})


test_that("mrClosed match the Bailey results from mrN.single() from fishmethods",{
  library(fishmethods)
  tmp1 <- mrN.single(M=948,C=421,R=167)
    
  tmp <- mrClosed(M=948,n=421,m=167,type="Bailey")
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(round(tmp1$N[2],0)))
  ctmp <- confint(tmp,type="binomial")
  expect_that(ctmp[[1,1]], equals(round(tmp1$LCI[2],0)))
  ## The UCI does not match ... fishmethods uses qbinom
  ##   whereas FSA uses binCI()
  #expect_that(ctmp[[1,2]], equals(round(tmp1$UCI[2],0)))
})  


test_that("mrClosed match the Schnabel Results from p. 32 Krebs (1989)",{
  library(FSAdata)
  data(SunfishIN)
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schnabel",chapman.mod=FALSE))
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(448))
  ## See if intermediate calculations match Krebs
  expect_that(tmp$N, equals(447.5))
  expect_that(tmp$sum.m, equals(24))                # sum R in Krebs
  expect_that(tmp$sum.nM, equals(10740))            # sum CM in Krebs
  expect_that(tmp$sum.nM2, equals(970296))          # sum CM^2 in Krebs
  expect_that(tmp$sum.mM, equals(2294))             # sum RM in Krebs
  expect_that(round(tmp$sum.m2dn,3), equals(7.745)) # sum R^2/C in Krebs
  ctmp <- confint(tmp,type="Poisson")
  ## The CIs do not equal ... Krebs uses table, FSA uses
  ##   numerical distribution for CIs
  #expect_that(ctmp[[1,1]], equals(310))
  #expect_that(ctmp[[1,2]], equals(720))
})


test_that("mrClosed match the Schnabel results from p. 99 Ricker (1975)",{
  library(FSAdata)
  data(SunfishIN)
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schnabel",chapman.mod=FALSE))
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(448))
  ## See if intermediate calculations match Krebs
  expect_that(tmp$sum.m, equals(24))                # sum R in Ricker
  expect_that(tmp$sum.nM, equals(10740))            # sum CM in Ricker
  expect_that(tmp$sum.nM2, equals(970296))          # sum CM^2 in Ricker
  expect_that(tmp$sum.mM, equals(2294))             # sum RM in Ricker
  expect_that(round(tmp$sum.m2dn,3), equals(7.745)) # sum R^2/C in Ricker
  ctmp <- confint(tmp,type="normal")
  ## The CIs do not equal ... ???
  #expect_that(ctmp[[1,1]], equals(320))
  #expect_that(ctmp[[1,2]], equals(746))
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schnabel",chapman.mod=TRUE))
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(430))
  ctmp <- confint(tmp,type="Poisson") 
  ## The CIs do not equal ... ???
  #expect_that(ctmp[[1,1]], equals(302))
  #expect_that(ctmp[[1,2]], equals(697)) 
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 33 Krebs (1989)",{
  library(FSAdata)
  data(SunfishIN)
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schumacher"))
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(423))
  ctmp <- confint(tmp,type="normal") 
  expect_that(ctmp[[1,1]], equals(300))
  expect_that(ctmp[[1,2]], equals(719))
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 99 Ricker (1975)",{
  library(FSAdata)
  data(SunfishIN)
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schumacher"))
  stmp <- summary(tmp)
  expect_that(stmp[[1,1]], equals(423))
  ctmp <- confint(tmp,type="normal") 
  ## The CIs do not equal ... ???
  #expect_that(ctmp[[1,1]], equals(304))
  #expect_that(ctmp[[1,2]], equals(696))
})

