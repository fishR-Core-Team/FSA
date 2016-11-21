context("mrOpen() VALIDATE")
source("EXS_mrOpen.R")

test_that("Does mrOpen() match the Jolly-Seber results from JOLLY for CutthroatAL",{
  expect_equal(cutt$df$r,rC)
  expect_equal(cutt$df$z,zC)
  # all M match at one decimal
  CcompM <- round(cbind(cutt$df$M,MC,cutt$df$M-MC,(cutt$df$M-MC)/MC*100),1)
  expect_equal(CcompM[,1],CcompM[,2])
  # all M.se match at one decimal except M3 and M4 which are off by 0.1
  CcompSEM <- round(cbind(cutt$df$M.se,MSEC,cutt$df$M.se-MSEC,(cutt$df$M.se-MSEC)/MSEC*100),1)
  expect_true(all(abs(CcompSEM[,3])<=0.5,na.rm=TRUE))
  # all N match at one decimal
  CcompN <- round(cbind(cutt$df$N,NC,cutt$df$N-NC,(cutt$df$N-NC)/NC*100),1)
  expect_equal(CcompN[,1],CcompN[,2])
  # all N.se match at one decimal except N8 which is off by 0.1
  CcompSEN <- round(cbind(cutt$df$N.se,NSEC,cutt$df$N.se-NSEC,(cutt$df$N.se-NSEC)/NSEC*100),1)
  expect_true(all(abs(CcompSEN[,3])<=0.5,na.rm=TRUE))
  # all phi within 0.001 at three decimals
  Ccompphi <- round(cbind(cutt$df$phi,phiC,cutt$df$phi-phiC,(cutt$df$phi-phiC)/phiC*100),3)
  expect_true(all(abs(Ccompphi[,3])<=0.001,na.rm=TRUE))
  # all phi.se within 0.001 at three decimals
  CcompSEphi <- round(cbind(cutt$df$phi.se,phiSEC,cutt$df$phi.se-phiSEC,(cutt$df$phi.se-phiSEC)/phiSEC*100),3)
  expect_true(all(abs(CcompSEphi[,3])<=0.001,na.rm=TRUE))
  # all B within 0.7 at one decimal
  CcompB <- round(cbind(cutt$df$B,BC,cutt$df$B-BC,(cutt$df$B-BC)/BC*100),1)
  expect_true(all(abs(CcompB[,3])<=0.7,na.rm=TRUE))  
  # all B.se within 0.02 (within 1%)
  CcompSEB <- round(cbind(cutt$df$B.se,BSEC,cutt$df$B.se-BSEC,(cutt$df$B.se-BSEC)/BSEC*100),2)
  #expect_true(all(abs(CcompSEB[,3])<=0.02,na.rm=TRUE)) 
})


test_that("Does mrOpen match the Jolly-Seber results from Table 4.4 in Pollock et al. (1990)",{
  # all N within 0.1 (less than 0.11%)
  PcompN <- round(cbind(poll$N,NP,poll$N-NP,(poll$N-NP)/NP*100),1)
  expect_true(all(abs(PcompN[,3])<=0.1,na.rm=TRUE))
  # all N.se match at two decimals except for N15 wich is within 0.5 (less than 5%)
  PcompSEN <- round(cbind(poll$N.se,NSEP,poll$N.se-NSEP,(poll$N.se-NSEP)/NSEP*100),2)
  expect_true(all(abs(PcompSEN[,3])<=0.5,na.rm=TRUE))
  # all phi match except where Pollock set them == 1 or did not report
  Pcompphi <- round(cbind(poll$phi,phiP,poll$phi-phiP,(poll$phi-phiP)/phiP*100),2)
  expect_equal(Pcompphi[-c(7,17,20,12:14),1],Pcompphi[-c(7,17,20,12:14),2])
  # all phi.se within 0.001 (less than 1%)
  PcompSEphi <- round(cbind(poll$phi.se,phiSEP,poll$phi.se-phiSEP,(poll$phi.se-phiSEP)/phiSEP*100),3)
  expect_true(all(abs(PcompSEphi[,3])<=0.001,na.rm=TRUE))
  # all B match except where Pollock set them -- 0 or did not report
  PcompB <- round(cbind(poll$B,BP,poll$B-BP,(poll$B-BP)/BP*100),1)
  expect_equal(PcompB[-c(5,11,18,21,12:14),1],PcompB[-c(5,11,18,21,12:14),2])
  # all B.se within 0.02 (within 0.2%)
  PcompSEB <- round(cbind(poll$B.se,BSEP,poll$B.se-BSEP,(poll$B.se-BSEP)/BSEP*100),2)
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
  Kcompphi <- round(cbind(krebs$df$phi,phiK,krebs$df$phi-phiK,(krebs$df$phi-phiK)/phiK*100),3)
  expect_equal(Kcompphi[,1],Kcompphi[,2])
  # all B match except B7 which was within 0.2
  KcompB <- round(cbind(krebs$df$B,BK,krebs$df$B-BK,(krebs$df$B-BK)/BK*100),1)
  expect_equal(KcompB[-7,1],KcompB[-7,2])
  expect_true(all(abs(KcompB[,3])<=0.2,na.rm=TRUE))
  # all N.se are not close (Krebs does not give formulas for checking why)
  KcompSEN <- round(cbind(krebs$df$N.se,NSEK,krebs$df$N.se-NSEK,(krebs$df$N.se-NSEK)/NSEK*100),1)
  # all phi.se match
  KcompSEphi <- round(cbind(krebs$df$phi.se,phiSEK,krebs$df$phi.se-phiSEK,(krebs$df$phi.se-phiSEK)/phiSEK*100),3)
  expect_equal(KcompSEphi[,1],KcompSEphi[,2])
  # all B.se are within 0.3 (all match except B6, B7, and B9)
  KcompSEB <- round(cbind(krebs$df$B.se,BSEK,krebs$df$B.se-BSEK,(krebs$df$B.se-BSEK)/BSEK*100),1)
  expect_equal(KcompSEphi[,1],KcompSEphi[,2])
})


test_that("Does mrOpen match the Jolly-Seber results from Table 5.3 in Seber (2002)",{
  expect_equal(jollyres$df$r,rJ)
  expect_equal(jollyres$df$z,zJ)
  
  # all M within 4 (less than 1.5%)
  JcompM <- round(cbind(jollyres$df$M,MJ,jollyres$df$M-MJ,(jollyres$df$M-MJ)/MJ*100),1)
  expect_true(all(abs(JcompM[,3])<=4.0,na.rm=TRUE))
  # all N within 3% (except N2 which is within 9%))
  JcompN <- round(cbind(jollyres$df$N,NJ,jollyres$df$N-NJ,(jollyres$df$N-NJ)/NJ*100),1)
  expect_true(all(abs(JcompN[-2,4])<=3,na.rm=TRUE))
  # all phi within 0.01 (less than 1.5%)
  Jcompphi <- round(cbind(jollyres$df$phi,phiJ,jollyres$df$phi-phiJ,(jollyres$df$phi-phiJ)/phiJ*100),3)
  expect_true(all(abs(Jcompphi[,3])<=0.01,na.rm=TRUE))
  # all B within 7 (less than 5%) except B2 and B8
  JcompB <- round(cbind(jollyres$df$B,BJ,jollyres$df$B-BJ,(jollyres$df$B-BJ)/BJ*100),1)
  expect_true(all(abs(JcompB[-c(2,8),3])<=6.2,na.rm=TRUE))
})
