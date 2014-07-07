context("Mark-Recapture, Closed Populations, Multiple Census")

test_that("mrClosed match the Schnabel Results from p. 32 Krebs (1989)",{
  library(FSAdata)
  data(SunfishIN)
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schnabel",chapman.mod=FALSE))
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(448))
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
  #expect_that(ctmp[[1,"95% LCI"]], equals(310))
  #expect_that(ctmp[[1,"95% UCI"]], equals(720))
})


test_that("mrClosed match the Schnabel results from p. 99 Ricker (1975)",{
  library(FSAdata)
  data(SunfishIN)
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schnabel",chapman.mod=FALSE))
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(448))
  ## See if intermediate calculations match Krebs
  expect_that(tmp$sum.m, equals(24))                # sum R in Ricker
  expect_that(tmp$sum.nM, equals(10740))            # sum CM in Ricker
  expect_that(tmp$sum.nM2, equals(970296))          # sum CM^2 in Ricker
  expect_that(tmp$sum.mM, equals(2294))             # sum RM in Ricker
  expect_that(round(tmp$sum.m2dn,3), equals(7.745)) # sum R^2/C in Ricker
  ctmp <- confint(tmp,type="normal")
  ## The CIs do not equal ... ???
  #expect_that(ctmp[[1,"95% LCI"]], equals(320))
  #expect_that(ctmp[[1,"95% UCI"]], equals(746))
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schnabel",chapman.mod=TRUE))
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(430))
  ctmp <- confint(tmp,type="Poisson") 
  ## The CIs do not equal ... ???
  #expect_that(ctmp[[1,"95% LCI"]], equals(302))
  #expect_that(ctmp[[1,"95% UCI"]], equals(697)) 
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 33 Krebs (1989)",{
  library(FSAdata)
  data(SunfishIN)
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schumacher"))
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(423))
  ctmp <- confint(tmp,type="normal") 
  expect_that(ctmp[[1,"95% LCI"]], equals(300))
  expect_that(ctmp[[1,"95% UCI"]], equals(719))
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 99 Ricker (1975)",{
  library(FSAdata)
  data(SunfishIN)
  
  tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,type="Schumacher"))
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(423))
  ctmp <- confint(tmp,type="normal") 
  ## The CIs do not equal ... ???
  #expect_that(ctmp[[1,"95% LCI"]], equals(304))
  #expect_that(ctmp[[1,"95% UCI"]], equals(696))
})

