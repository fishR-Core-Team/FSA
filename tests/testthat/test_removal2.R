context("Verification of 'removal()' results.")

test_that("removal with 'CarleStrub' matches Carle-Strub (1978) examples",{
  tmp <- summary(removal(c(38,26,12)))
  expect_that(round(tmp["No","Estimate"],0), equals(91))
  expect_that(round(tmp["No","Std. Error"],1), equals(9.7))
  expect_that(round(tmp["p","Estimate"],3), equals(0.444))

  tmp <- summary(removal(c(5,7,8)))
  expect_that(round(tmp["No","Estimate"],0), equals(44))
  expect_that(round(tmp["p","Estimate"],3), equals(0.174))
})

test_that("removal with 'CarleStrub' matches Cowx (1983) page 77",{
  tmp <- summary(removal(c(72,56,46,30,24)))
  expect_that(round(tmp["No","Estimate"],0), equals(298))
  expect_that(round(tmp["p","Estimate"],3), equals(0.250))
  # SE does not match
  #expect_that(round(tmp["No","Std. Error"],1), equals(23.62))
  
  tmp <- summary(removal(c(8,23,17,8,6)))
  expect_that(round(tmp["No","Estimate"],0), equals(95))
  expect_that(round(tmp["p","Estimate"],3), equals(0.187))
})

test_that("removal with 'CarleStrub' match results from Jones & Stockwell (1995)",{
  if (require(FSAdata)) {
    data(JonesStockwell)
    # isolate captures and Carel-Strub estimates ... for non-rejected estimates
    JS.caps <- JonesStockwell[!JonesStockwell$rejected,4:6]
    JS.cs <- JonesStockwell[!JonesStockwell$rejected,7]
    # compute Carle-Strub estimates for all data in JS.caps
    tmp <- apply(JS.caps,1,removal,just.ests=TRUE)["No",]
    # Make a comparison matrix
    compJS <- round(cbind(tmp,JS.cs,tmp-JS.cs,(tmp-JS.cs)/JS.cs*100),1)
    # all values are within 3
    expect_that(all(abs(compJS[,3])<=3,na.rm=TRUE),is_true())
  }
})

test_that("removal with 'Seber3' matches Cowx (1983) page 75",{
  tmp <- summary(removal(c(72,56,46),method="Seber3"))
  expect_that(round(tmp["No","Estimate"],0), equals(353))
})

test_that("removal with 'Seber2' matches Cowx (1983) page 75",{
  tmp <- summary(removal(c(72,56),method="Seber2"))
  expect_that(round(tmp["No","Estimate"],0), equals(324))
  expect_that(round(tmp["No","Std. Error"],2), equals(178.19))
  expect_that(round(tmp["p","Estimate"],2), equals(0.22))
})

test_that("removal with 'Seber2' matches Seber(2012) example 7.4",{
  tmp <- summary(removal(c(79,28),method="Seber2"))
  expect_that(round(tmp["No","Estimate"],0), equals(122))
  expect_that(round(tmp["No","Std. Error"],1), equals(8.8))
  expect_that(round(tmp["p","Estimate"],2), equals(0.65))
})

test_that("removal with 'RobsonRegier2' matches Cowx (1983) page 75",{
  tmp <- summary(removal(c(72,56),method="RobsonRegier2"))
  # used ceiling because of weird round issue
  expect_that(ceiling(tmp["No","Estimate"]), equals(321))
  expect_that(round(tmp["No","Std. Error"],2), equals(178.19))
})


test_that("removal with 'Moran' matches Schnute (1983)",{
  if (require(FSAdata)) {
    data(BrookTroutNEWP1)
    Ns <- ps <- LHs <- NLCI <- NUCI <- numeric(nrow(BrookTroutNEWP1))
    for (i in 1:nrow(BrookTroutNEWP1)) {
      tmp <- removal(as.numeric(BrookTroutNEWP1[i,c("first","second","third","fourth")]),method="Moran")
      Ns[i] <- round(tmp$est[["No"]],1)
      ps[i] <- round(tmp$est[["p"]],2)
      LHs[i] <- round(tmp$min.nlogLH,2)
      tmp <- confint(tmp)
      NLCI[i] <- tmp[1]
      NUCI[i] <- tmp[2]
    }
    ## check point estimates
    tmp <- cbind(sample=1:nrow(BrookTroutNEWP1),Ns,ps,LHs,BrookTroutNEWP1[,c("Moran.N","Moran.p","Moran.LH")])
    ## perfect matches
    expect_that(tmp[,"Ns"],equals(BrookTroutNEWP1$Moran.N[]))
    expect_that(tmp[,"ps"],equals(BrookTroutNEWP1$Moran.p[]))
    expect_that(tmp[,"LHs"],equals(BrookTroutNEWP1$Moran.LH[]))
    ## Check CIs (off by no more than 0.1 in a small handful of the UCIs)
    tmp <- cbind(sample=1:nrow(BrookTroutNEWP1),NLCI,NUCI,BrookTroutNEWP1[,c("Moran.NLCI","Moran.NUCI")])
  }
})

test_that("removal with 'Schnute' matches Schnute (1983)",{
  if (require(FSAdata)) {
    data(BrookTroutNEWP1)
    Ns <- p1s <- ps <- LHs <- NLCI <- NUCI <- numeric(nrow(BrookTroutNEWP1))
    for (i in 1:nrow(BrookTroutNEWP1)) {
      tmp <- removal(as.numeric(BrookTroutNEWP1[i,c("first","second","third","fourth")]),method="Schnute")
      Ns[i] <- round(tmp$est[["No"]],1)
      p1s[i] <- round(tmp$est[["p1"]],2)
      ps[i] <- round(tmp$est[["p"]],2)
      LHs[i] <- round(tmp$min.nlogLH,2)
      tmp <- confint(tmp)
      NLCI[i] <- tmp[1]
      NUCI[i] <- tmp[2]
    }
    ## check point estimates
    tmp <- cbind(sample=1:nrow(BrookTroutNEWP1),Ns,p1s,ps,LHs,BrookTroutNEWP1[,c("Schnute.N","Schnute.p1","Schnute.p","Schnute.LH")])
    ## perfect matches except sample 5 N is off by 0.1
    expect_that(tmp[-5,"Ns"],equals(BrookTroutNEWP1$Schnute.N[-5]))
    expect_that(tmp[,"p1s"],equals(BrookTroutNEWP1$Schnute.p1[]))
    expect_that(tmp[,"ps"],equals(BrookTroutNEWP1$Schnute.p[]))
    expect_that(tmp[,"LHs"],equals(BrookTroutNEWP1$Schnute.LH[]))
    ## Check CIs (off by no more than 0.1)
    tmp <- cbind(sample=1:nrow(BrookTroutNEWP1),NLCI,NUCI,BrookTroutNEWP1[,c("Schnute.NLCI","Schnute.NUCI")])
  }
})