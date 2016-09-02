context("removal Function Tests")

# ############################################################
# Messaging
# ############################################################
test_that("removal() errors and warnings",{
  ## wrong type
  expect_error(removal(c(346,184,49),method="Derek"))
  ## wrong type for CS.se
  expect_error(removal(c(346,184,49),method="CarleStrub",CS.se="Derek"))
  ## alpha and beta are not positive
  expect_error(removal(c(346,184,49),method="CarleStrub",alpha=1,beta=0))
  expect_error(removal(c(346,184,49),method="CarleStrub",alpha=-1,beta=1))
  expect_error(removal(c(346,184,49),method="CarleStrub",alpha=-1,beta=0))
  ## Catch not in a vector
  expect_error(removal(matrix(c(346,184,49,12),nrow=2)))
  ## only one catch
  expect_error(removal(346))
  ## Try using Moran or Schnute method with not three catches
  expect_error(removal(c(346,184),method="Moran"))
  expect_error(removal(c(346,184),method="Schnute"))
  ## Try using 3-pass method with not three catches
  expect_error(removal(c(346,184),method="Seber3"))
  expect_error(removal(c(346,184,49,12),method="Seber3"))
  ## Try using 2-pass method with not >2 catches
  expect_error(removal(c(346,184,49),method="Seber2"))
  expect_error(removal(c(346,184,49),method="RobsonRegier2"))
  ## Errors in 2- and 3-pass methods if last catch is greater than first catch
  expect_warning(removal(c(184,346),method="Seber2"))
  expect_warning(removal(c(184,346),method="RobsonRegier2"))
  expect_warning(removal(c(49,184,346),method="Seber3"))
  ## wrong parm in summary and confint
  tmp <- removal(c(346,184,49))
  expect_error(summary(tmp,parm="Derek"))
  expect_error(confint(tmp,parm="Derek"))
  expect_error(removal(c(346,184,49),conf.level=0),"must be between 0 and 1")
  expect_error(removal(c(346,184,49),conf.level=1),"must be between 0 and 1")
  
  ## Bad data leads to failure of Zippin (from Carle-Strub (1978) example 2)
  expect_warning(removal(c(5,7,8),method="Zippin"))
  ## Chose "p1" summary for other than Schnute method
  tmp <- removal(c(45,11,18,8),method="Zippin")
  expect_error(summary(tmp,parm="p1"))
  expect_warning(summary(tmp,parm=c("p","p1")))
  ## Chose only "p" CI for Moran or Schnute method
  tmp <- removal(c(45,11,18,8),method="Schnute")
  expect_error(confint(tmp,parm="p"))
  ## Chose bad value for Tmult
  expect_error(removal(c(45,11,18,8),method="Moran",Tmult=0.9))  
  expect_warning(removal(c(45,11,18,8),method="Moran",Tmult=1.2))  
  ## NAs in catch vector
  expect_warning(removal(c(45,11,NA,8))) 
})  

test_that("removal() verbose= messages",{
  expect_message(summary(removal(c(38,26,12)),verbose=TRUE),"Carle & Strub")
  expect_message(summary(removal(c(38,26,12),method="Moran"),verbose=TRUE),"Moran")
  expect_message(summary(removal(c(38,26,12),method="Zippin"),verbose=TRUE),"Zippin")
  expect_message(summary(removal(c(38,26,12),method="Schnute"),verbose=TRUE),"Schnute")
  expect_message(summary(removal(c(38,26,12),method="Seber3"),verbose=TRUE),"Seber")
  expect_message(summary(removal(c(38,26),method="Seber2"),verbose=TRUE),"Seber")
  expect_message(summary(removal(c(38,26),method="RobsonRegier2"),verbose=TRUE),"Robson & Regier")
})   

# ############################################################
# Analytical Results
# ############################################################
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
    expect_equal(tmp[,"Ns"],BrookTroutNEWP1$Moran.N[])
    expect_equal(tmp[,"ps"],BrookTroutNEWP1$Moran.p[])
    expect_equal(tmp[,"LHs"],BrookTroutNEWP1$Moran.LH[])
    ## Check CIs (off by no more than 0.1 in a small handful of the UCIs)
    tmp <- cbind(sample=1:nrow(BrookTroutNEWP1),NLCI,NUCI,BrookTroutNEWP1[,c("Moran.NLCI","Moran.NUCI")])
    expect_true(all(abs(tmp[,2:3]-tmp[,4:5])<=0.1001))
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
    expect_equal(tmp[-5,"Ns"],BrookTroutNEWP1$Schnute.N[-5])
    expect_equal(tmp[,"p1s"],BrookTroutNEWP1$Schnute.p1[])
    expect_equal(tmp[,"ps"],BrookTroutNEWP1$Schnute.p[])
    expect_equal(tmp[,"LHs"],BrookTroutNEWP1$Schnute.LH[])
    ## Check CIs (off by no more than 0.1)
    tmp <- cbind(sample=1:nrow(BrookTroutNEWP1),NLCI,NUCI,BrookTroutNEWP1[,c("Schnute.NLCI","Schnute.NUCI")])
    expect_true(all(abs(tmp[,2:3]-tmp[,4:5])<=0.1001,na.rm=TRUE))
  }
})

