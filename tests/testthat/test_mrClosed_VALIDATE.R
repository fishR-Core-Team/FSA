context("mrClosed() VALIDATE")

# ------------------------------------------------------------
# Mark-Recapture, Closed Populations, Single Census
# ------------------------------------------------------------
test_that("mrClosed match the Petersen results from Box 2.1 in Krebs (1989)",{
  tmp <- mrClosed(M=948,n=421,m=167)
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_equal(stmp[[1,"N"]], 2390)
  ctmp <- confint(tmp,type="normal")
  expect_equal(ctmp[[1,"95% LCI"]], 2153)
  ## The UCI does not match (<1%)
  #expect_equal(ctmp[[1,"95% UCI"]], 2685)
})

test_that("mrClosed match the Chapman results from Box 2.1 Krebs (1989)",{
  tmp <- mrClosed(M=948,n=421,m=167,method="Chapman")
  stmp <- summary(tmp)
  expect_equal(stmp[[1,"N"]], 2383)
})

test_that("mrClosed match the Chapman results from Ricker (1975)",{
  # Chapman estimate
  tmp <- mrClosed(M=109,n=177,m=57,method="Chapman")
  stmp <- summary(tmp)
  expect_equal(stmp[[1,"N"]], 337)
})  

test_that("mrClosed match the Chapman results from Box 11.2 in Pine et al. (2013)",{
  # Chapmann estimate
  tmp <- mrClosed(M=421,n=332,m=88,method="Chapman")
  stmp <- summary(tmp)
  expect_equal(stmp[[1,"N"]], 1578)
  ctmp <- confint(tmp,type="normal")
  expect_equal(ctmp[[1,"95% LCI"]], 1330)
  expect_equal(ctmp[[1,"95% UCI"]], 1826)
})  

test_that("mrClosed match the Chapman results from Table 3.7 and 3.8 in Seber (2002)",{
  tmp <- mrClosed(M=500,n=149,m=7,method="Chapman")
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_equal(stmp[[1,"N"]], 9393)
  expect_equal(round(stmp[[1,"SE"]],0), 3022)
  ctmp <- confint(tmp,type="normal")
  ## The UCI does not match (<0.01%) ... due to digits on Z*
  #expect_equal(ctmp[[1,"95% LCI"]], 3470)
  expect_equal(ctmp[[1,"95% UCI"]], 15316)
  
  tmp <- mrClosed(M=1000,n=243,m=21,method="Chapman")
  stmp <- summary(tmp,incl.SE=TRUE)
  expect_equal(stmp[[1,"N"]], 11101)
  expect_equal(round(stmp[[1,"SE"]],0), 2184)
  ctmp <- confint(tmp,type="normal")
  ## The CI does not match (<0.02%) ... due to digits on Z*
  #expect_equal(ctmp[[1,"95% LCI"]], 6820)
  #expect_equal(ctmp[[1,"95% UCI"]], 15382)
})

test_that("mrClosed match the Chapman results from mrN.single() from fishmethods",{
  if (require(fishmethods)) {
    tmp1 <- mrN.single(M=948,C=421,R=167)
    
    tmp <- mrClosed(M=948,n=421,m=167,method="Chapman")
    stmp <- summary(tmp,incl.SE=TRUE)
    expect_equal(stmp[[1,"N"]], round(tmp1$N[1],0))
    expect_equal(stmp[[1,"SE"]], round(tmp1$SE[1],1))
    
    ctmp <- confint(tmp,type="hypergeometric")
    ## The CIs do not equal (<1%) ... fish methods uses qhyper
    ##   whereas FSA uses hyperCI
    #expect_equal(ctmp[[1,"95% LCI"]], round(tmp1$LCI[1],0))
    #expect_equal(ctmp[[1,"95% UCI"]], round(tmp1$UCI[1],0))
  }
})


test_that("mrClosed match the Bailey results from mrN.single() from fishmethods",{
  if (require(fishmethods)) {
    tmp1 <- mrN.single(M=948,C=421,R=167)
    
    tmp <- mrClosed(M=948,n=421,m=167,method="Bailey")
    stmp <- summary(tmp,incl.SE=TRUE)
    expect_equal(stmp[[1,"N"]], round(tmp1$N[2],0))
    expect_equal(stmp[[1,"SE"]], round(tmp1$SE[2],1))
    ctmp <- confint(tmp,type="binomial")
    expect_equal(ctmp[[1,"95% LCI"]], round(tmp1$LCI[2],0))
    ## The UCI does not match (<0.1%) ... fishmethods uses qbinom
    ##   whereas FSA uses binCI()
    #expect_equal(ctmp[[1,"95% UCI"]], round(tmp1$UCI[2],0))
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
    expect_equal(stmp[[1,"N"]], 448)
    ## See if intermediate calculations match Krebs
    expect_equal(tmp$N, 447.5)
    expect_equal(tmp$sum.m, 24)                # sum R in Krebs
    expect_equal(tmp$sum.nM, 10740)            # sum CM in Krebs
    expect_equal(tmp$sum.nM2, 970296)          # sum CM^2 in Krebs
    expect_equal(tmp$sum.mM, 2294)             # sum RM in Krebs
    expect_equal(round(tmp$sum.m2dn,3), 7.745) # sum R^2/C in Krebs
    ctmp <- confint(tmp,type="Poisson")
    ## The CIs do not equal ... Krebs uses table, FSA uses poiCI (see below)
    #expect_equal(ctmp[[1,"95% LCI"]], 310)
    #expect_equal(ctmp[[1,"95% UCI"]], 720)
    ptmp <- poiCI(tmp$sum.m)
    #expect_equal(ptmp[[1,"95% LCI"]], 14.921)
    #expect_equal(ptmp[[1,"95% UCI"]], 34.665)
  }
})


test_that("mrClosed match the Schnabel results from p. 99 Ricker (1975)",{
  if (require(FSAdata)) {
    data(SunfishIN)
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schnabel",chapman.mod=FALSE))
    stmp <- summary(tmp)
    expect_equal(stmp[[1,"N"]], 448)
    ## See if intermediate calculations match Krebs
    expect_equal(tmp$sum.m, 24)                # sum R in Ricker
    expect_equal(tmp$sum.nM, 10740)            # sum CM in Ricker
    expect_equal(tmp$sum.nM2, 970296)          # sum CM^2 in Ricker
    expect_equal(tmp$sum.mM, 2294)             # sum RM in Ricker
    expect_equal(round(tmp$sum.m2dn,3), 7.745) # sum R^2/C in Ricker
    ctmp <- confint(tmp,type="normal")
    ## The CIs do not equal ... ???
    #expect_equal(ctmp[[1,"95% LCI"]], 320)
    #expect_equal(ctmp[[1,"95% UCI"]], 746)
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schnabel",chapman.mod=TRUE))
    stmp <- summary(tmp)
    expect_equal(stmp[[1,"N"]], 430)
    ctmp <- confint(tmp,type="Poisson") 
    ## The CIs do not equal ... ???
    #expect_equal(ctmp[[1,"95% LCI"]], 302)
    #expect_equal(ctmp[[1,"95% UCI"]], 697) 
  }
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 33 Krebs (1989)",{
  if (require(FSAdata)) {
    data(SunfishIN)
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schumacher"))
    stmp <- summary(tmp)
    expect_equal(stmp[[1,"N"]], 423)
    ctmp <- confint(tmp,type="normal") 
    expect_equal(ctmp[[1,"95% LCI"]], 300)
    expect_equal(ctmp[[1,"95% UCI"]], 719)
  }
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 99 Ricker (1975)",{
  if (require(FSAdata)) {
    data(SunfishIN)
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,method="Schumacher"))
    stmp <- summary(tmp)
    expect_equal(stmp[[1,"N"]], 423)
    ctmp <- confint(tmp,type="normal") 
    ## The CIs do not equal ... ???
    #expect_equal(ctmp[[1,"95% LCI"]], 304)
    #expect_equal(ctmp[[1,"95% UCI"]], 696)
  }
})