## Test Messages ----
test_that("mrClosed() Single Census messages",{
  ## wrong type
  expect_error(mrClosed(346,184,49,method="Derek"),
               "should be one of")
  ## missing numerical arguments
  expect_error(mrClosed(346),
               "One or both of 'n' or 'm' is missing")
  expect_error(mrClosed(346,184),
               "One or both of 'n' or 'm' is missing")
  ## multiple groups, missing arguments or different lengths
  expect_error(mrClosed(c(346,346)),
               "One or both of 'n' or 'm' is missing")
  expect_error(mrClosed(c(346,346),c(184,184),49),
               "vectors must be same length")
  expect_error(mrClosed(c(346,346),c(184,184),c(49,49,49)),
               "vectors must be same length")
  expect_error(mrClosed(c(346,346),c(184,184),c(49,49),labels="Derek"),
               "'labels' must be same length as")
  expect_error(mrClosed(c(346,346),c(184,184),c(49,49),labels=c("A","B","C")),
               "'labels' must be same length as")
  ## R not used in single census
  expect_warning(mrClosed(346,184,49,200),
                 "'R' not used in single census methods")
  ## no M
  expect_error(mrClosed(n=200),
               "'M' is missing")
  expect_error(mrClosed(m=200),
               "'M' is missing")
  ## give R
  expect_error(mrClosed(R=200),
               "'R' not used in single census methods")
  expect_warning(mrClosed(M=346,n=184,m=49,R=200),
                 "'R' not used in single census methods")
  ## can't have more recaps (m) than number checked (n)
  expect_error(mrClosed(346,184,200),
               "Can't have more recaptures")
  expect_error(mrClosed(c(346,346),c(184,184),c(49,200)),
               "has more recaptures")
  ## using capHistSum() but trying to provide other values
  ch1 <- capHistSum(BluegillJL)
  expect_warning(mrClosed(ch1,n=90),
                 "ignored when 'M' from")
  ## confint problems
  mr1 <- mrClosed(ch1)
  expect_error(confint(mr1,conf.level=0),
               "must be between 0 and 1")
  expect_error(confint(mr1,conf.level=1),
               "must be between 0 and 1")
  expect_error(confint(mr1,type="derek"),
               "should be one of")
  expect_error(confint(mr1,type="binomial",bin.type="derek"),
               "should be one of")
  expect_error(confint(mr1,type="poisson",poi.type="derek"),
               "should be one of")
  expect_warning(confint(mr1,parm="N"),
                 "is meaningless")
})

test_that("mrClosed Multiple Census errors and warnings",{
  n1 <- c(20,18,14, 9,17)
  m1 <- c( 0, 3, 4, 4,14)
  R1 <- c(20,18,14, 9, 0)
  M1 <- c( 0,20,35,45,50)
  ## missing numerical arguments
  expect_error(mrClosed(n=n1,method="Schnabel"),
               "must be supplied")
  expect_error(mrClosed(n=n1,m=m1,method="Schnabel"),
               "must be supplied")
  expect_error(mrClosed(M=M1,method="Schnabel"),
               "missing without 'M'")
  expect_error(mrClosed(M=M1,n=n1,method="Schnabel"),
               "missing without 'M'")
  expect_error(mrClosed(R=R1,method="Schnabel"),
               "One or both of")
  expect_error(mrClosed(R=R1,n=n1,method="Schnabel"),
               "One or both of")
  expect_error(mrClosed(M=M1,R=R1,method="Schnabel"),
               "One or both of")
  expect_warning(mrClosed(M=M1,n=n1,m=m1,R=R1,method="Schnabel"),
                 "Only need one of")
  ## NAs in some of the vectors
  m1x <- c(NA, 3, 4, 4,14)
  R1x <- c(20,18,14, 9,NA)
  M1x <- c(NA,20,35,45,50)
  expect_warning(mrClosed(n=n1,m=m1x,R=R1,method="Schnabel"),
                 "'m' was ignored")
  expect_warning(mrClosed(n=n1,m=m1,R=R1x,method="Schnabel"),
                 "'R' was ignored")
  expect_warning(mrClosed(n=n1,m=m1x,M=M1x,method="Schnabel"),
                 "'M' was ignored")
  ## confint problems
  mr1 <- mrClosed(M1,n1,m1)
  expect_error(confint(mr1,conf.level=0),
               "must be between 0 and 1")
  expect_error(confint(mr1,conf.level=1),
               "must be between 0 and 1")
  expect_error(confint(mr1,poi.type="derek"),
               "should be one of")
  mr1 <- mrClosed(M1,n1,m1,method="Schumacher")
  expect_error(confint(mr1,conf.level=0),
               "must be between 0 and 1")
  expect_error(confint(mr1,conf.level=1),
               "must be between 0 and 1")
  expect_warning(confint(mr1,type="Poisson"),
                 "changed to")
  expect_warning(confint(mr1,parm="N"),
                 "is meaningless")
})


## Test Output Types ----
test_that("mrClosed() Single Census output",{
  ch1 <- capHistSum(BluegillJL)
  mr1 <- mrClosed(ch1)
  expect_is(mr1,"mrClosed1")
  expect_equal(mode(mr1),"list")
  expect_equal(mr1$method,"Petersen")
  mr2 <- mrClosed(ch1,method="Chapman")
  expect_is(mr2,"mrClosed1")
  expect_equal(mode(mr2),"list")
  expect_equal(mr2$method,"Chapman")
  mr3 <- mrClosed(ch1,method="Ricker")
  expect_is(mr3,"mrClosed1")
  expect_equal(mode(mr3),"list")
  expect_equal(mr3$method,"Ricker")
  mr4 <- mrClosed(ch1,method="Bailey")
  expect_is(mr4,"mrClosed1")
  expect_equal(mode(mr4),"list")
  expect_equal(mr4$method,"Bailey")
  
  tmp <- summary(mr1)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  tmp <- summary(mr1,incl.SE=TRUE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("N","SE"))
  expect_message(summary(mr1,verbose=TRUE),"Petersen")
  tmp <- summary(mr2)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  tmp <- summary(mr2,incl.SE=TRUE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("N","SE"))
  expect_message(summary(mr1,verbose=TRUE),"Petersen")
  tmp <- summary(mr3)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  tmp <- summary(mr3,incl.SE=TRUE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("N","SE"))
  expect_message(summary(mr1,verbose=TRUE),"Petersen")
  tmp <- summary(mr4)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  tmp <- summary(mr4,incl.SE=TRUE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("N","SE"))
  expect_message(summary(mr1,verbose=TRUE),"Petersen")
  
  expect_message(tmp <- confint(mr1,verbose=TRUE),"Poisson")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_message(tmp <- confint(mr1,verbose=TRUE,type="binomial"),"binomial")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_message(tmp <- confint(mr1,verbose=TRUE,type="hypergeometric"),
                 "hypergeometric")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_message(tmp <- confint(mr2,verbose=TRUE),"Poisson")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_message(tmp <- confint(mr3,verbose=TRUE),"Poisson")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_message(tmp <- confint(mr4,verbose=TRUE),"Poisson")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
})

test_that("mrClosed() Single Census with subgroups output",{
  marked <- c(93,35,72,16,46,20)
  captured <- c(103,30,73,17,39,18)
  recaps <- c(20,23,52,15,35,16)
  lbls <- c("YOY","Juvenile","Stock","Quality","Preferred","Memorable")
  mr1 <- mrClosed(marked,captured,recaps)
  mr2 <- mrClosed(marked,captured,recaps,labels=lbls)
  expect_is(mr1,"mrClosed1")
  expect_equal(mode(mr1),"list")
  expect_is(mr2,"mrClosed1")
  expect_equal(mode(mr2),"list")
  
  expect_message(tmp <- summary(mr1,verbose=TRUE),"Petersen")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),length(marked)+1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  expect_equal(rownames(tmp),c(LETTERS[seq_along(marked)],"All"))
  tmp <- summary(mr1,incl.SE=TRUE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),length(marked)+1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("N","SE"))
  expect_equal(rownames(tmp),c(LETTERS[seq_along(marked)],"All"))
  tmp <- summary(mr1,incl.SE=TRUE,incl.all=FALSE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),length(marked))
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("N","SE"))
  expect_equal(rownames(tmp),LETTERS[seq_along(marked)])
  tmp <- summary(mr2,incl.SE=TRUE,incl.all=FALSE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),length(marked))
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("N","SE"))
  expect_equal(rownames(tmp),lbls)
  
  expect_message(tmp <- confint(mr1,verbose=TRUE,type="binomial"),"binomial")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),length(marked)+1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_equal(rownames(tmp),c(LETTERS[seq_along(marked)],"All"))
  tmp <- confint(mr1,incl.all=FALSE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),length(marked))
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_equal(rownames(tmp),LETTERS[seq_along(marked)])
  tmp <- confint(mr2,incl.all=FALSE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),length(marked))
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_equal(rownames(tmp),lbls)
})

test_that("mrClosed() Schnabel output",{
  mr1 <- with(PikeNY,mrClosed(n=n,m=m,R=R,method="Schnabel"))
  mr2 <- with(PikeNY,mrClosed(n=n,m=m,R=R,method="Schnabel",chapman.mod=FALSE))
  
  expect_is(mr1,"mrClosed2")
  expect_equal(mode(mr1),"list")
  expect_equal(mr1$method,"Schnabel")
  expect_true(mr1$chapman.mod)
  expect_is(mr2,"mrClosed2")
  expect_equal(mode(mr2),"list")
  expect_equal(mr2$method,"Schnabel")
  expect_false(mr2$chapman.mod)
  
  expect_message(tmp <- summary(mr1,verbose=TRUE),"Schnabel")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  
  expect_message(tmp <- confint(mr1,verbose=TRUE),"normal")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_message(tmp <- confint(mr1,verbose=TRUE,type="Poisson"),"Poisson")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_message(tmp <- confint(mr1,verbose=TRUE,type="normal"),"normal")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
})

test_that("mrClosed() Schnabel with capHistSum() output",{
  ch <- capHistSum(PikeNYPartial1,cols2ignore="id")
  
  mr1 <- mrClosed(ch,method="Schnabel")
  mr2 <- mrClosed(ch,method="Schnabel",chapman.mod=FALSE)
  
  expect_is(mr1,"mrClosed2")
  expect_equal(mode(mr1),"list")
  expect_equal(mr1$method,"Schnabel")
  expect_true(mr1$chapman.mod)
  expect_is(mr2,"mrClosed2")
  expect_equal(mode(mr2),"list")
  expect_equal(mr2$method,"Schnabel")
  expect_false(mr2$chapman.mod)
  
  expect_message(tmp <- summary(mr1,verbose=TRUE),"Schnabel")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  
  expect_message(tmp <- confint(mr1,verbose=TRUE,type="Poisson"),"Poisson")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_message(tmp <- confint(mr1,verbose=TRUE,type="normal"),"normal")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
})

test_that("mrClosed() Schumacher-Eschmeyer output",{
  mr1 <- with(PikeNY,mrClosed(n=n,m=m,R=R,method="Schumacher"))
  
  expect_is(mr1,"mrClosed2")
  expect_equal(mode(mr1),"list")
  
  expect_message(tmp <- summary(mr1,verbose=TRUE),"Schumacher")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  
  expect_message(tmp <- confint(mr1,verbose=TRUE),"normal")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
})

test_that("mrClosed() Schumacher-Eschmeyer capHistSum() output",{
  ch <- capHistSum(PikeNYPartial1,cols2ignore="id")
  mr1 <- mrClosed(ch,method="Schumacher")
  
  expect_is(mr1,"mrClosed2")
  expect_equal(mode(mr1),"list")
  
  expect_message(tmp <- summary(mr1,verbose=TRUE),"Schumacher")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(colnames(tmp),"N")
  
  expect_message(tmp <- confint(mr1,verbose=TRUE),"normal")
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
})


## Validate Results ----
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
    ctmp <- confint(tmp,type="binomial",bin.type="wilson")
    ## CI does not match (<0.1%) ... fishmethods uses qbinom, FSA uses binCI()
    #expect_equal(ctmp[[1,"95% LCI"]], round(tmp1$LCI[2],0))
    #expect_equal(ctmp[[1,"95% UCI"]], round(tmp1$UCI[2],0))
  }
})



test_that("mrClosed match the Schnabel Results from p. 32 Krebs (1989)",{
  if (require(FSAdata)) {
    data(SunfishIN,package="FSAdata")
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,
                                   method="Schnabel",chapman.mod=FALSE))
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
    data(SunfishIN,package="FSAdata")
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,
                                   method="Schnabel",chapman.mod=FALSE))
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
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,
                                   method="Schnabel",chapman.mod=TRUE))
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
    data(SunfishIN,package="FSAdata")
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,
                                   method="Schumacher"))
    stmp <- summary(tmp)
    expect_equal(stmp[[1,"N"]], 423)
    ctmp <- confint(tmp,type="normal") 
    expect_equal(ctmp[[1,"95% LCI"]], 300)
    expect_equal(ctmp[[1,"95% UCI"]], 719)
  }
})


test_that("mrClosed match the Schumacher-Eschmeyer results from p. 99 Ricker (1975)",{
  if (require(FSAdata)) {
    data(SunfishIN,package="FSAdata")
    
    tmp <- with(SunfishIN,mrClosed(n=caught,m=recaps,R=retmarks,
                                   method="Schumacher"))
    stmp <- summary(tmp)
    expect_equal(stmp[[1,"N"]], 423)
    ctmp <- confint(tmp,type="normal") 
    ## The CIs do not equal ... ???
    #expect_equal(ctmp[[1,"95% LCI"]], 304)
    #expect_equal(ctmp[[1,"95% UCI"]], 696)
  }
})
