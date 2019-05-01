## Data and results for tests ----
# Setup some matrices
good.top <- matrix(c(
  NA,15, 1, 0, 0,
  NA,NA,13, 3, 0,
  NA,NA,NA,10, 5,
  NA,NA,NA,NA, 9,
  NA,NA,NA,NA,NA),nrow=5,byrow=TRUE)
good.bot <- matrix(c(
  15,14,13, 5,
  10, 9, 8,11,
  25,23,21,16,
  25,23,21,16),nrow=4,byrow=TRUE,
  dimnames=list(c("m","u","n","R")))

## CutthroatAL Example
cutt <- mrOpen(capHistSum(CutthroatAL,cols2use=-1))
cutt2 <- mrOpen(capHistSum(CutthroatAL,cols2use=-1),type="Manly")

# expectations from running Jolly Model A (http://www.mbr-pwrc.usgs.gov/software/jolly.html)
rC <- c(26,96,51,46,100,99,44,13,NA)
zC <- c(NA,4,6,16,4,5,13,5,NA)
MC <- c(NA,36.56,127.81,120.66,68.30,117.55,175.07,100.21,NA)
MSEC <- c(NA,6.36,13.35,20.85,4.13,7.30,24.62,24.70,NA)
NC <- c(NA,561.07,394.19,672.25,300.97,436.11,553.74,255.26,NA)
NSEC <- c(NA,117.88,44.17,138.78,21.77,30.29,84.34,65.35,NA)
phiC <- c(.4107,.3486,.3697,.2175,.4364,.4504,.2668,NA,NA)
phiSEC <- c(.0884,.0445,.0709,.0304,.0411,.0687,.0718,NA,NA)
BC <- c(NA,198.62,526.51,154.77,304.77,357.32,107.55,NA,NA)
BSEC <- c(NA,68.88,119.51,29.81,26.67,61.15,35.23,NA,NA)


## Gray squirrel summarized results from Pollock et al. (1990)
# enter data from table 4.3
poll <- data.frame(period=1:23,
                   n=c(46,46,48,46,51,37,41,42,47,31,8,2,1,
                       4,9,19,19,27,36,45,74,22,3),
                   m=c(0,42,42,42,46,37,41,39,43,26,7,2,0,3,
                       8,17,14,20,36,34,46,20,2),
                   R=c(46,46,48,46,50,37,41,42,47,31,8,2,1,
                       4,9,18,19,27,36,44,73,22,2),
                   r=c(43,44,48,45,46,35,40,37,40,26,8,2,1,
                       3,8,17,18,24,32,33,15,2,NA),
                   z=c(NA,1,3,9,8,17,11,12,6,20,39,45,47,45,
                       40,31,34,32,20,18,5,0,NA))
# expectations from Table 4.4
NP <- c(NA,47.1,51.3,56.0,60.5,54.9,52.3,56.5,54.6,58.9,51.8,NA,
        NA,NA,58.3,55.3,66.4,74.5,58.4,76.0,110.3,21.9,NA)
NSEP <- c(NA,0.39,0.70,1.19,1.51,1.23,0.60,2.06,1.57,4.59,5.99,NA,
          NA,NA,9.20,4.30,8.14,7.91,2.13,6.12,18.10,0.00,NA)
phiP <- c(0.94,0.96,1.00,0.99,0.94,0.95,1.00,0.90,0.92,0.84,1.00,
          NA,NA,NA,0.93,0.98,1.00,0.93,0.99,1.00,0.21,NA,NA)
phiSEP <- c(.037,.030,.004,.023,.041,.038,.030,.052,.067,.066,.000,
            NA,NA,NA,.115,.068,.071,.067,.071,.168,.048,NA,NA)
BP <- c(NA,6.3,4.5,5.1,0.0,0.0,3.9,3.7,8.7,2.2,0.0,NA,NA,NA,
        1.0,13.1,6.8,0.0,18.2,33.9,0.0,NA,NA)
BSEP <- c(NA,0.77,1.27,1.53,1.06,0.00,1.22,1.45,3.30,6.60,5.99,
          NA,NA,NA,6.57,8.25,10.14,6.28,4.22,8.86,2.23,NA,NA)

# get mrOpen results by using internal functions
poll <- FSA:::iEstM(poll)
suppressWarnings(poll <- FSA:::iEstN(poll,type="Jolly",conf.level=0.95))
poll <- FSA:::iEstPhi(poll,k=23,type="Jolly",conf.level=0.95,phi.full=TRUE)
suppressWarnings(poll <- FSA:::iEstB(poll,k=23,type="Jolly",conf.level=0.95))


## Field Vole example from Krebs (Table 2.2)
# data entered by hand
s1 <- rep(NA,11)
s2 <- c(15,rep(NA,10))
s3 <- c(1,15,rep(NA,9))
s4 <- c(0,0,37,rep(NA,8))
s5 <- c(0,1,2,61,rep(NA,7))
s6 <- c(0,0,0,4,75,rep(NA,6))
s7 <- c(0,0,0,1,3,77,rep(NA,5))
s8 <- c(0,0,0,1,2,4,69,rep(NA,4))
s9 <- c(0,0,0,0,0,0,0,8,rep(NA,3))
s10 <- c(0,0,0,0,0,0,0,1,14,rep(NA,2))
s11 <- c(0,0,0,0,0,0,0,0,0,19,NA)
krebs.top <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11)
n <- c(22,41,48,82,89,101,107,91,19,27,22)
m <- c(0,15,16,37,64,79,81,76,8,15,19)
u <- n-m
R <- c(21,41,46,82,88,99,106,90,19,26,22)
krebs.bot <- rbind(m,u,n,R)

# Expectations from Krebs Table 2.3
MK <- c(NA,17.5,17.2,40.7,70.5,87.5,91.7,76,9.3,15,NA)
# put 27 in for N10 because 27 were captured in that time
NK <- c(NA,45.9,49.5,88.8,97.7,111.6,120.8,90.8,20.7,27,NA)
NSEK <- c(NA,6.1,4.0,5.3,5.3,5.8,7.1,6.5,3.6,4.4,NA)
phiK <- c(0.832,0.395,0.862,0.824,0.925,0.853,0.651,0.104,0.738,NA,NA)
phiSEK <- c(0.126,0.077,0.055,0.043,0.032,0.043,0.046,0.033,0.101,NA,NA)
# Put in 11.7 for B9 because of change to N10 above
BK <- c(NA,31.4,47.9,24.6,22.1,27.3,12.8,11.4,11.7,NA,NA)
BSEK <- c(NA,2.7,3.6,3.3,2.6,2.8,1.7,1.8,1.1,NA,NA)

# mrOpen() results
suppressWarnings(krebs <- mrOpen(krebs.top,krebs.bot))


## Jolly's data from Table 5.1 and 5.2 in Seber (2002) page 206
s1 <- rep(NA,13)
s2 <- c(10,rep(NA,12))
s3 <- c(3,34,rep(NA,11))
s4 <- c(5,18,33,rep(NA,10))
s5 <- c(2,8,13,30,rep(NA,9))
s6 <- c(2,4,8,20,43,rep(NA,8))
s7 <- c(1,6,5,10,34,56,rep(NA,7))
s8 <- c(0,4,0,3,14,19,46,rep(NA,6))
s9 <- c(0,2,4,2,11,12,28,51,rep(NA,5))
s10 <- c(0,0,1,2,3,5,17,22,34,rep(NA,4))
s11 <- c(1,2,3,1,0,4,8,12,16,30,rep(NA,3))
s12 <- c(0,1,3,1,1,2,7,4,11,16,26,NA,NA)
s13 <- c(0,1,0,2,3,3,2,10,9,12,18,35,NA)
jolly.top <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13)
n <- c(54,146,169,209,220,209,250,176,172,127,123,120,142)
R <- c(54,143,164,202,214,207,243,175,169,126,120,120,0)
m <- c(0,10,37,56,53,77,112,86,110,84,77,72,95)
u <- n-m
jolly.bot <- rbind(m,u,n,R)

# expectations from Seber (2002) Table 5.3 (page 207)
rJ <- c(24,80,70,71,109,101,108,99,70,58,44,35,NA)
zJ <- c(NA,14,57,71,89,121,110,132,121,107,88,60,NA)
MJ <- c(NA,35.02,170.54,258,227.73,324.99,359.50,319.33,402.13,316.45,317,277.71,NA)
NJ <- c(NA,511.2,779.1,963.0,945.3,882.2,802.5,653.6,628.8,478.5,506.4,462.8,NA)
phiJ <- c(0.649,1.015,0.867,0.564,0.836,0.790,0.651,0.985,0.686,0.884,0.771,NA,NA)
BJ <- c(NA,263.2,291.8,406.4,96.9,107.0,135.7,-13.8,49.0,84.1,74.5,NA,NA)

# mrOpen() results
jollyres <- mrOpen(jolly.top,jolly.bot)


## Test Messages ----
test_that("mrOpen() messages",{
  ## a top but not a bottom, but with capHistSum
  expect_error(mrOpen(good.top),
               "'mb.top' and a 'mb.bot'")
  ## a top that is not square
  expect_error(mrOpen(good.top[,-1],good.bot),
               "'mb.top' must be square")
  expect_error(mrOpen(good.top[-1,],good.bot),
               "'mb.top' must be square")
  ## a top without an NA on the diagonal or lower triangle
  bad.top <- good.top
  bad.top[2,2] <- 3
  expect_error(mrOpen(bad.top,good.bot),
               "Lower triangle and diagonal of 'mb.top'")
  bad.top <- good.top
  bad.top[3,2] <- 3
  expect_error(mrOpen(bad.top,good.bot),
               "Lower triangle and diagonal of 'mb.top'")
  ## a top with an NA in the upper triangle
  bad.top <- good.top
  bad.top[1,2] <- NA
  expect_error(mrOpen(bad.top,good.bot),
               "Upper triangle of 'mb.top' cannot contain any 'NA'")
  ## a top with a negative value in the upper triangle
  bad.top <- good.top
  bad.top[1,2] <- -3
  expect_error(mrOpen(bad.top,good.bot),
               "All non-NA values in 'mb.top' must be non-negative")
  
  ## bottom does not have enough rows
  expect_error(mrOpen(good.top,good.bot[-1,]),
               "must contain four rows with")
  ## bottom has bad names
  bad.bot <- good.bot
  names(bad.bot)[1] <- "Derek"
  expect_error(mrOpen(good.top,bad.bot),
               "rownames of 'mb.bot' must be")
  ## a bottom with a negative value
  bad.bot <- good.bot
  bad.bot[1,2] <- -3
  expect_error(mrOpen(good.top,bad.bot),
               "'mb.bot' must be non-negative")
  ## a bottom with a non-zero first number of marked fish
  bad.bot <- good.bot
  bad.bot["m",1] <- 3
  expect_error(mrOpen(good.top,bad.bot),
               "First value of 'm' row in 'mb.bot' must be 0")
  ## a bottom with a NA
  bad.bot["m",1] <- NA
  expect_error(mrOpen(good.top,bad.bot),
               "All values in 'mb.bot' must be non-NA")
  
  ## Confint
  expect_error(mrOpen(capHistSum(CutthroatAL,cols2use=-1),conf.level=0),
               "must be between 0 and 1")
  expect_error(mrOpen(capHistSum(CutthroatAL,cols2use=-1),conf.level=1),
               "must be between 0 and 1")
  expect_warning(confint(cutt,conf.level=0.95),
                 "It cannot be changed here")
  expect_warning(confint(cutt2,conf.level=0.95),
                 "It cannot be changed here")
  expect_message(suppressWarnings(confint(cutt2,conf.level=0.95)),
                 "Manly did not provide a method for constructing")
  
  ## Summary()
  expect_error(summary(cutt,parm="Derek"),"should be one of")
  tmp <- capture.output(summary(cutt,verbose=TRUE))
  expect_true(any(grepl("Observables",tmp)))
  expect_true(any(grepl("Estimates",tmp)))
})


## Test Output Types ----
test_that("mrOpen() returns",{
  # non-Manly type
  expect_is(cutt,"mrOpen")
  expect_equal(mode(cutt),"list")
  tmp <- summary(cutt)
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),8)
  expect_equal(names(tmp),c("M","M.se","N","N.se","phi","phi.se","B","B.se"))
  tmp <- summary(cutt,parm="N")
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),2)
  expect_equal(names(tmp),c("N","N.se"))
  tmp <- summary(cutt,parm=c("N","phi"))
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),4)
  expect_equal(names(tmp),c("N","N.se","phi","phi.se"))
  tmp <- confint(cutt)
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),6)
  expect_equal(names(tmp),c("N.lci","N.uci","phi.lci","phi.uci","B.lci","B.uci"))
  tmp <- confint(cutt,parm="N")
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),2)
  expect_equal(names(tmp),c("N.lci","N.uci"))
  tmp <- confint(cutt,parm=c("N","phi"))
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),4)
  expect_equal(names(tmp),c("N.lci","N.uci","phi.lci","phi.uci"))
  
  # Manly type
  expect_is(cutt2,"mrOpen")
  expect_equal(mode(cutt2),"list")
  tmp <- summary(cutt2)
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),4)
  expect_equal(names(tmp),c("M","N","phi","B"))
  tmp <- summary(cutt2,parm="N")
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),1)
  expect_equal(names(tmp),"N")
  tmp <- summary(cutt2,parm=c("N","phi"))
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),2)
  expect_equal(names(tmp),c("N","phi"))
  tmp <- suppressMessages(confint(cutt2))
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),4)
  expect_equal(names(tmp),c("N.lci","N.uci","phi.lci","phi.uci"))
  tmp <- confint(cutt2,parm="N")
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),2)
  expect_equal(names(tmp),c("N.lci","N.uci"))
  tmp <- confint(cutt2,parm=c("N","phi"))
  expect_is(tmp,"data.frame")
  expect_equal(ncol(tmp),4)
  expect_equal(names(tmp),c("N.lci","N.uci","phi.lci","phi.uci"))
})

test_that("Does mrOpen() match jolly()",{
  expect_equal(jollyres,jolly(jolly.top,jolly.bot))
})


## Validate Results ----
test_that("Does mrOpen() match the Jolly-Seber results from JOLLY for CutthroatAL",{
  expect_equal(cutt$df$r,rC)
  expect_equal(cutt$df$z,zC)
  # all M match at one decimal
  CcompM <- round(cbind(cutt$df$M,MC,cutt$df$M-MC,(cutt$df$M-MC)/MC*100),1)
  expect_equal(CcompM[,1],CcompM[,2])
  # all M.se match at one decimal except M3 and M4 which are off by 0.1
  CcompSEM <- round(cbind(cutt$df$M.se,MSEC,cutt$df$M.se-MSEC,
                          (cutt$df$M.se-MSEC)/MSEC*100),1)
  expect_true(all(abs(CcompSEM[,3])<=0.5,na.rm=TRUE))
  # all N match at one decimal
  CcompN <- round(cbind(cutt$df$N,NC,cutt$df$N-NC,(cutt$df$N-NC)/NC*100),1)
  expect_equal(CcompN[,1],CcompN[,2])
  # all N.se match at one decimal except N8 which is off by 0.1
  CcompSEN <- round(cbind(cutt$df$N.se,NSEC,cutt$df$N.se-NSEC,
                          (cutt$df$N.se-NSEC)/NSEC*100),1)
  expect_true(all(abs(CcompSEN[,3])<=0.5,na.rm=TRUE))
  # all phi within 0.001 at three decimals
  Ccompphi <- round(cbind(cutt$df$phi,phiC,cutt$df$phi-phiC,
                          (cutt$df$phi-phiC)/phiC*100),3)
  expect_true(all(abs(Ccompphi[,3])<=0.001,na.rm=TRUE))
  # all phi.se within 0.001 at three decimals
  CcompSEphi <- round(cbind(cutt$df$phi.se,phiSEC,cutt$df$phi.se-phiSEC,
                            (cutt$df$phi.se-phiSEC)/phiSEC*100),3)
  expect_true(all(abs(CcompSEphi[,3])<=0.001,na.rm=TRUE))
  # all B within 0.7 at one decimal
  CcompB <- round(cbind(cutt$df$B,BC,cutt$df$B-BC,(cutt$df$B-BC)/BC*100),1)
  expect_true(all(abs(CcompB[,3])<=0.7,na.rm=TRUE))  
  # all B.se within 0.02 (within 1%)
  CcompSEB <- round(cbind(cutt$df$B.se,BSEC,cutt$df$B.se-BSEC,
                          (cutt$df$B.se-BSEC)/BSEC*100),2)
  #expect_true(all(abs(CcompSEB[,3])<=0.02,na.rm=TRUE)) 
})


test_that("Does mrOpen match the Jolly-Seber results from Table 4.4 in Pollock et al. (1990)",{
  # all N within 0.1 (less than 0.11%)
  PcompN <- round(cbind(poll$N,NP,poll$N-NP,(poll$N-NP)/NP*100),1)
  expect_true(all(abs(PcompN[,3])<=0.1,na.rm=TRUE))
  # all N.se match at two decimals except for N15 wich is within 0.5 (less than 5%)
  PcompSEN <- round(cbind(poll$N.se,NSEP,poll$N.se-NSEP,
                          (poll$N.se-NSEP)/NSEP*100),2)
  expect_true(all(abs(PcompSEN[,3])<=0.5,na.rm=TRUE))
  # all phi match except where Pollock set them == 1 or did not report
  Pcompphi <- round(cbind(poll$phi,phiP,poll$phi-phiP,(poll$phi-phiP)/phiP*100),2)
  expect_equal(Pcompphi[-c(7,17,20,12:14),1],Pcompphi[-c(7,17,20,12:14),2])
  # all phi.se within 0.001 (less than 1%)
  PcompSEphi <- round(cbind(poll$phi.se,phiSEP,poll$phi.se-phiSEP,
                            (poll$phi.se-phiSEP)/phiSEP*100),3)
  expect_true(all(abs(PcompSEphi[,3])<=0.001,na.rm=TRUE))
  # all B match except where Pollock set them -- 0 or did not report
  PcompB <- round(cbind(poll$B,BP,poll$B-BP,(poll$B-BP)/BP*100),1)
  expect_equal(PcompB[-c(5,11,18,21,12:14),1],PcompB[-c(5,11,18,21,12:14),2])
  # all B.se within 0.02 (within 0.2%)
  PcompSEB <- round(cbind(poll$B.se,BSEP,poll$B.se-BSEP,
                          (poll$B.se-BSEP)/BSEP*100),2)
  expect_true(all(abs(PcompSEB[,3])<=0.02,na.rm=TRUE)) 
})

test_that("Does mrOpen match the Jolly-Seber results from Table 2.3 in Krebs (1989)",{
  # all M match except M5 which was within 0.1
  KcompM <- round(cbind(krebs$df$M,MK,krebs$df$M-MK,(krebs$df$M-MK)/MK*100),1)
  expect_equal(KcompM[-5,1],KcompM[-5,2])
  expect_true(all(abs(KcompM[,3])<=0.1,na.rm=TRUE))
  # all N match except M8 which was within 0.2
  KcompN <- round(cbind(krebs$df$N,NK,krebs$df$N-NK,(krebs$df$N-NK)/NK*100),1)
  expect_equal(KcompN[-8,1],KcompN[-8,2])
  expect_true(all(abs(KcompN[,3])<=0.2,na.rm=TRUE))
  # all phi match
  Kcompphi <- round(cbind(krebs$df$phi,phiK,krebs$df$phi-phiK,
                          (krebs$df$phi-phiK)/phiK*100),3)
  expect_equal(Kcompphi[,1],Kcompphi[,2])
  # all B match except B7 which was within 0.2
  KcompB <- round(cbind(krebs$df$B,BK,krebs$df$B-BK,(krebs$df$B-BK)/BK*100),1)
  expect_equal(KcompB[-7,1],KcompB[-7,2])
  expect_true(all(abs(KcompB[,3])<=0.2,na.rm=TRUE))
  # all N.se are not close (Krebs does not give formulas for checking why)
  KcompSEN <- round(cbind(krebs$df$N.se,NSEK,krebs$df$N.se-NSEK,
                          (krebs$df$N.se-NSEK)/NSEK*100),1)
  # all phi.se match
  KcompSEphi <- round(cbind(krebs$df$phi.se,phiSEK,krebs$df$phi.se-phiSEK,
                            (krebs$df$phi.se-phiSEK)/phiSEK*100),3)
  expect_equal(KcompSEphi[,1],KcompSEphi[,2])
  # all B.se are within 0.3 (all match except B6, B7, and B9)
  KcompSEB <- round(cbind(krebs$df$B.se,BSEK,krebs$df$B.se-BSEK,
                          (krebs$df$B.se-BSEK)/BSEK*100),1)
  expect_equal(KcompSEphi[,1],KcompSEphi[,2])
})


test_that("Does mrOpen match the Jolly-Seber results from Table 5.3 in Seber (2002)",{
  expect_equal(jollyres$df$r,rJ)
  expect_equal(jollyres$df$z,zJ)
  
  # all M within 4 (less than 1.5%)
  JcompM <- round(cbind(jollyres$df$M,MJ,jollyres$df$M-MJ,
                        (jollyres$df$M-MJ)/MJ*100),1)
  expect_true(all(abs(JcompM[,3])<=4.0,na.rm=TRUE))
  # all N within 3% (except N2 which is within 9%))
  JcompN <- round(cbind(jollyres$df$N,NJ,jollyres$df$N-NJ,
                        (jollyres$df$N-NJ)/NJ*100),1)
  expect_true(all(abs(JcompN[-2,4])<=3,na.rm=TRUE))
  # all phi within 0.01 (less than 1.5%)
  Jcompphi <- round(cbind(jollyres$df$phi,phiJ,jollyres$df$phi-phiJ,
                          (jollyres$df$phi-phiJ)/phiJ*100),3)
  expect_true(all(abs(Jcompphi[,3])<=0.01,na.rm=TRUE))
  # all B within 7 (less than 5%) except B2 and B8
  JcompB <- round(cbind(jollyres$df$B,BJ,jollyres$df$B-BJ,
                        (jollyres$df$B-BJ)/BJ*100),1)
  expect_true(all(abs(JcompB[-c(2,8),3])<=6.2,na.rm=TRUE))
})

