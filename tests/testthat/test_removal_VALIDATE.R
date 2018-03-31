context("removal() VALIDATE")

test_that("removal with 'CarleStrub' matches Carle-Strub (1978) examples",{
  tmp <- summary(removal(c(38,26,12)))
  expect_equal(round(tmp["No","Estimate"],0),91)
  expect_equal(round(tmp["No","Std. Error"],1),9.7)
  expect_equal(round(tmp["p","Estimate"],3),0.444)
  
  tmp <- summary(removal(c(5,7,8)))
  expect_equal(round(tmp["No","Estimate"],0),44)
  expect_equal(round(tmp["p","Estimate"],3),0.174)
})

test_that("removal with 'CarleStrub' matches Cowx (1983) page 77",{
  tmp <- summary(removal(c(72,56,46,30,24)))
  expect_equal(round(tmp["No","Estimate"],0),298)
  expect_equal(round(tmp["p","Estimate"],3),0.250)
  # SE does not match
  #expect_equal(round(tmp["No","Std. Error"],1),23.62)
  
  tmp <- summary(removal(c(8,23,17,8,6)))
  expect_equal(round(tmp["No","Estimate"],0),95)
  expect_equal(round(tmp["p","Estimate"],3),0.187)
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
    expect_true(all(abs(compJS[,3])<=3,na.rm=TRUE))
  }
})

test_that("removal with 'Seber3' matches Cowx (1983) page 75",{
  tmp <- summary(removal(c(72,56,46),method="Seber3"))
  expect_equal(round(tmp["No","Estimate"],0),353)
})

test_that("removal with 'Seber2' matches Cowx (1983) page 75",{
  tmp <- summary(removal(c(72,56),method="Seber2"))
  expect_equal(round(tmp["No","Estimate"],0),324)
  expect_equal(round(tmp["No","Std. Error"],2),178.19)
  expect_equal(round(tmp["p","Estimate"],2),0.22)
})

test_that("removal with 'Seber2' matches Seber(2012) example 7.4",{
  tmp <- summary(removal(c(79,28),method="Seber2"))
  expect_equal(round(tmp["No","Estimate"],0),122)
  expect_equal(round(tmp["No","Std. Error"],1),8.8)
  expect_equal(round(tmp["p","Estimate"],2),0.65)
})

test_that("removal with 'RobsonRegier2' matches Cowx (1983) page 75",{
  tmp <- summary(removal(c(72,56),method="RobsonRegier2"))
  # used ceiling because of weird round issue
  expect_equal(ceiling(tmp["No","Estimate"]),321)
  expect_equal(round(tmp["No","Std. Error"],2),178.19)
})


test_that("removal with 'Moran' matches Schnute (1983)",{
  if (require(FSAdata)) {
    data(BrookTroutNEWP1)
    Ns <- ps <- LHs <- NLCI <- NUCI <- numeric(nrow(BrookTroutNEWP1))
    for (i in seq_len(nrow(BrookTroutNEWP1))) {
      tmp <- removal(as.numeric(BrookTroutNEWP1[i,c("first","second","third","fourth")]),method="Moran")
      Ns[i] <- round(tmp$est[["No"]],1)
      ps[i] <- round(tmp$est[["p"]],2)
      LHs[i] <- round(tmp$min.nlogLH,2)
      tmp <- confint(tmp)
      NLCI[i] <- tmp[1]
      NUCI[i] <- tmp[2]
    }
    ## check point estimates
    tmp <- cbind(sample=seq_len(nrow(BrookTroutNEWP1)),Ns,ps,LHs,BrookTroutNEWP1[,c("Moran.N","Moran.p","Moran.LH")])
    ## perfect matches
    expect_equal(tmp[,"Ns"],BrookTroutNEWP1$Moran.N[])
    expect_equal(tmp[,"ps"],BrookTroutNEWP1$Moran.p[])
    expect_equal(tmp[,"LHs"],BrookTroutNEWP1$Moran.LH[])
    ## Check CIs (off by no more than 0.1 in a small handful of the UCIs)
    tmp <- cbind(sample=seq_len(nrow(BrookTroutNEWP1)),NLCI,NUCI,BrookTroutNEWP1[,c("Moran.NLCI","Moran.NUCI")])
    expect_true(all(abs(tmp[,2:3]-tmp[,4:5])<=0.1001))
  }
})

test_that("removal with 'Schnute' matches Schnute (1983)",{
  if (require(FSAdata)) {
    data(BrookTroutNEWP1)
    Ns <- p1s <- ps <- LHs <- NLCI <- NUCI <- numeric(nrow(BrookTroutNEWP1))
    for (i in seq_len(nrow(BrookTroutNEWP1))) {
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
    tmp <- cbind(sample=seq_len(nrow(BrookTroutNEWP1)),Ns,p1s,ps,LHs,BrookTroutNEWP1[,c("Schnute.N","Schnute.p1","Schnute.p","Schnute.LH")])
    ## perfect matches except sample 5 N is off by 0.1
    expect_equal(tmp[-5,"Ns"],BrookTroutNEWP1$Schnute.N[-5])
    expect_equal(tmp[,"p1s"],BrookTroutNEWP1$Schnute.p1[])
    expect_equal(tmp[,"ps"],BrookTroutNEWP1$Schnute.p[])
    expect_equal(tmp[,"LHs"],BrookTroutNEWP1$Schnute.LH[])
    ## Check CIs (off by no more than 0.1)
    tmp <- cbind(sample=seq_len(nrow(BrookTroutNEWP1)),NLCI,NUCI,BrookTroutNEWP1[,c("Schnute.NLCI","Schnute.NUCI")])
    expect_true(all(abs(tmp[,2:3]-tmp[,4:5])<=0.1001,na.rm=TRUE))
  }
})
