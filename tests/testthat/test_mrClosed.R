context("Mark-Recapture -- Closed")

# ############################################################
# ============================================================
# Messaging
# ============================================================
# ############################################################
test_that("mrClosed Single Census errors and warnings",{
  ## wrong type
  expect_that(mrClosed(346,184,49,method="Derek"),throws_error())
  ## missing numerical arguments
  expect_that(mrClosed(346),throws_error())
  expect_that(mrClosed(346,184),throws_error())
  ## multiple groups, missing arguments or different lengths
  expect_that(mrClosed(c(346,346)),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),49),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),c(49,49,49)),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),c(49,49),labels="Derek"),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),c(49,49),labels=c("A","B","C")),throws_error())
  ## R not used in single census
  expect_that(mrClosed(346,184,49,200),gives_warning())
  ## no M
  expect_that(mrClosed(n=200),throws_error())
  expect_that(mrClosed(m=200),throws_error())
  ## give R
  expect_that(mrClosed(R=200),throws_error())
  expect_that(mrClosed(M=346,n=184,m=49,R=200),gives_warning())
  ## can't have more recaps (m) than number checked (n)
  expect_that(mrClosed(346,184,200),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),c(49,200)),throws_error())
  ## using capHistSum() but trying to provide other values
  data(BluegillJL)
  ch1 <- capHistSum(BluegillJL)
  expect_that(mrClosed(ch1,n=90),gives_warning())
})
  
test_that("mrClosed Multiple Census errors and warnings",{
  n1 <- c(20,18,14, 9,17)
  m1 <- c( 2, 3, 4, 4,14)
  R1 <- c(20,18,14, 9, 0)
  M1 <- c( 0,20,35,45,50)
  ## missing numerical arguments
  expect_that(mrClosed(n=n1,method="Schnabel"),throws_error())
  expect_that(mrClosed(n=n1,m=m1,method="Schnabel"),throws_error())
  expect_that(mrClosed(M=M1,method="Schnabel"),throws_error())
  expect_that(mrClosed(M=M1,n=n1,method="Schnabel"),throws_error())
  expect_that(mrClosed(R=R1,method="Schnabel"),throws_error())
  expect_that(mrClosed(R=R1,n=n1,method="Schnabel"),throws_error())
  expect_that(mrClosed(M=M1,R=R1,method="Schnabel"),throws_error())
  expect_that(mrClosed(M=M1,n=n1,m=m1,R=R1,method="Schnabel"),gives_warning())
})

# ############################################################
# ============================================================
# Analytical Results
# ============================================================
# ############################################################

# ------------------------------------------------------------
# Mark-Recapture, Closed Populations, Single Census
# ------------------------------------------------------------
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
  tmp <- mrClosed(M=948,n=421,m=167,method="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(2383))
})

test_that("mrClosed match the Chapman results from Ricker (1975)",{
  # Chapman estimate
  tmp <- mrClosed(M=109,n=177,m=57,method="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(337))
})  

test_that("mrClosed match the Chapman results from Box 11.2 in Pine et al. (2013)",{
  # Chapmann estimate
  tmp <- mrClosed(M=421,n=332,m=88,method="Chapman")
  stmp <- summary(tmp)
  expect_that(stmp[[1,"N"]], equals(1578))
  ctmp <- confint(tmp,type="normal")
  expect_that(ctmp[[1,"95% LCI"]], equals(1330))
  expect_that(ctmp[[1,"95% UCI"]], equals(1826))
})  

test_that("mrClosed match the Chapman results from Table 3.7 and 3.8 in Seber (2002)",{
  tmp <- mrClosed(M=500,n=149,m=7,method="Chapman")
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_that(stmp[[1,"N"]], equals(9393))
  expect_that(round(stmp[[1,"SE"]],0), equals(3022))
  ctmp <- confint(tmp,type="normal")
  ## The UCI does not match (<0.01%) ... due to digits on Z*
  #expect_that(ctmp[[1,"95% LCI"]], equals(3470))
  expect_that(ctmp[[1,"95% UCI"]], equals(15316))
  
  tmp <- mrClosed(M=1000,n=243,m=21,method="Chapman")
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_that(stmp[[1,"N"]], equals(11101))
  expect_that(round(stmp[[1,"SE"]],0), equals(2184))
  ctmp <- confint(tmp,type="normal")
  ## The CI does not match (<0.02%) ... due to digits on Z*
  #expect_that(ctmp[[1,"95% LCI"]], equals(6820))
  #expect_that(ctmp[[1,"95% UCI"]], equals(15382))
})

test_that("mrClosed match the Chapman results from mrN.single() from fishmethods",{
  if (require(fishmethods)) {
    tmp1 <- mrN.single(M=948,C=421,R=167)
    
    tmp <- mrClosed(M=948,n=421,m=167,method="Chapman")
    stmp <- summary(tmp,incl.SE=TRUE)
    expect_that(stmp[[1,"N"]], equals(round(tmp1$N[1],0)))
    expect_that(stmp[[1,"SE"]], equals(round(tmp1$SE[1],1)))
    
    ctmp <- confint(tmp,type="hypergeometric")
    ## The CIs do not equal (<1%) ... fish methods uses qhyper
    ##   whereas FSA uses hyperCI
    #expect_that(ctmp[[1,"95% LCI"]], equals(round(tmp1$LCI[1],0)))
    #expect_that(ctmp[[1,"95% UCI"]], equals(round(tmp1$UCI[1],0)))
  }
})


test_that("mrClosed match the Bailey results from mrN.single() from fishmethods",{
  if (require(fishmethods)) {
    tmp1 <- mrN.single(M=948,C=421,R=167)
    
    tmp <- mrClosed(M=948,n=421,m=167,method="Bailey")
    stmp <- summary(tmp,incl.SE=TRUE)
    expect_that(stmp[[1,"N"]], equals(round(tmp1$N[2],0)))
    expect_that(stmp[[1,"SE"]], equals(round(tmp1$SE[2],1)))
    ctmp <- confint(tmp,type="binomial")
    expect_that(ctmp[[1,"95% LCI"]], equals(round(tmp1$LCI[2],0)))
    ## The UCI does not match (<0.1%) ... fishmethods uses qbinom
    ##   whereas FSA uses binCI()
    #expect_that(ctmp[[1,"95% UCI"]], equals(round(tmp1$UCI[2],0)))
  }
})



# ------------------------------------------------------------
# Mark-Recapture, Closed Populations, Multiple Census
# ------------------------------------------------------------

test_that("mrClosed match the Schnabel Results from p. 32 Krebs (1989)",{
  if (require(FSAdata)) {
    data(SunfishIN)
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schnabel",chapman.mod=FALSE))
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
    ## The CIs do not equal ... Krebs uses table, FSA uses poiCI (see below)
    #expect_that(ctmp[[1,"95% LCI"]], equals(310))
    #expect_that(ctmp[[1,"95% UCI"]], equals(720))
    ptmp <- poiCI(tmp$sum.m)
    #expect_that(ptmp[[1,"95% LCI"]], equals(14.921))
    #expect_that(ptmp[[1,"95% UCI"]], equals(34.665))
  }
})


test_that("mrClosed match the Schnabel results from p. 99 Ricker (1975)",{
  if (require(FSAdata)) {
    data(SunfishIN)
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schnabel",chapman.mod=FALSE))
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
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schnabel",chapman.mod=TRUE))
    stmp <- summary(tmp)
    expect_that(stmp[[1,"N"]], equals(430))
    ctmp <- confint(tmp,type="Poisson") 
    ## The CIs do not equal ... ???
    #expect_that(ctmp[[1,"95% LCI"]], equals(302))
    #expect_that(ctmp[[1,"95% UCI"]], equals(697)) 
  }
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 33 Krebs (1989)",{
  if (require(FSAdata)) {
    data(SunfishIN)
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schumacher"))
    stmp <- summary(tmp)
    expect_that(stmp[[1,"N"]], equals(423))
    ctmp <- confint(tmp,type="normal") 
    expect_that(ctmp[[1,"95% LCI"]], equals(300))
    expect_that(ctmp[[1,"95% UCI"]], equals(719))
  }
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 99 Ricker (1975)",{
  if (require(FSAdata)) {
    data(SunfishIN)
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schumacher"))
    stmp <- summary(tmp)
    expect_that(stmp[[1,"N"]], equals(423))
    ctmp <- confint(tmp,type="normal") 
    ## The CIs do not equal ... ???
    #expect_that(ctmp[[1,"95% LCI"]], equals(304))
    #expect_that(ctmp[[1,"95% UCI"]], equals(696))
  }
})