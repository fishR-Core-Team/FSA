context("depletion() VALIDATE")
# Set up some results to test below
# fishmethods's Darter data
if (require(fishmethods)) {
  data(darter,package="fishmethods")
  # fishmethod Leslie
  deplet(catch=darter$catch,effort=darter$effort,method="l")
  cf1fm <- l.out$results[,1:2]
  ci1fm <- l.out$results[,3:4]
  # FSA Leslie
  ex1 <- with(darter,depletion(darter$catch,darter$effort))
  cf1 <- summary(ex1)
  ci1 <- confint(ex1)
  
  # fishmethod DeLury
  deplet(catch=darter$catch,effort=darter$effort,method="d")
  cf2fm <- d.out$results[,1:2]
  ci2fm <- d.out$results[,3:4]
  # FSA Leslie
  ex2 <- with(darter,depletion(darter$catch,darter$effort,
                               method="DeLury",Ricker.mod=TRUE))
  cf2 <- summary(ex2)
  ci2 <- confint(ex2) 
}

# DeLury's Lobster Data
if (require(FSAdata)) {
  data(LobsterPEI,package="FSAdata")
  df <- subset(LobsterPEI,day>16)
  # fishmethod Leslie
  deplet(catch=df$catch,effort=df$effort,method="l")
  cf3fm <- l.out$results[,1:2]
  ci3fm <- l.out$results[,3:4]
  # FSA Leslie
  ex3 <- with(df,depletion(catch,effort))
  cf3 <- summary(ex3)
  ci3 <- confint(ex3)
  
  # fishmethod Leslie
  deplet(catch=df$catch,effort=df$effort,method="d")
  cf4fm <- d.out$results[,1:2]
  ci4fm <- d.out$results[,3:4]
  # FSA DeLury
  ex4 <- with(df,depletion(catch,effort,method="DeLury",Ricker.mod=TRUE))
  cf4 <- summary(ex4)
  ci4 <- confint(ex4)
}

# Fischler's Blue Crab data
if (require(FSAdata)) {
  data(BlueCrab,package="FSAdata")
  # fishmethod Leslie
  deplet(catch=BlueCrab$catch,effort=BlueCrab$effort,method="l")
  cf5fm <- l.out$results[,1:2]
  ci5fm <- l.out$results[,3:4]
  # FSA Leslie
  ex5 <- with(BlueCrab,depletion(catch,effort))
  cf5 <- summary(ex5)
  ci5 <- confint(ex5)
  
  # fishmethod DeLury
  deplet(catch=BlueCrab$catch,effort=BlueCrab$effort,method="d")
  cf6fm <- d.out$results[,1:2]
  ci6fm <- d.out$results[,3:4]
  # FSA DeLury
  ex6 <- with(BlueCrab,depletion(catch,effort,method="DeLury",Ricker.mod=TRUE))
  cf6 <- summary(ex6)
  ci6 <- confint(ex6)
}

# Omand's SMB data
if (require(fishmethods)) {
  # fishmethod Leslie
  deplet(catch=SMBassLS$catch,effort=SMBassLS$effort,method="l")
  cf7fm <- l.out$results[,1:2]
  ci7fm <- l.out$results[,3:4]
  # FSA Leslie
  ex7 <- with(SMBassLS,depletion(catch,effort))
  cf7 <- summary(ex7)
  ci7 <- confint(ex7)
  # FSA Leslie with Ricker mod
  ex7r <- with(SMBassLS,depletion(catch,effort,Ricker.mod=TRUE))
  cf7r <- summary(ex7r)
  ci7r <- confint(ex7r)
  
  # fishmethod DeLury
  deplet(catch=SMBassLS$catch,effort=SMBassLS$effort,method="d")
  cf8fm <- d.out$results[,1:2]
  ci8fm <- d.out$results[,3:4]
  # FSA DeLury
  ex8 <- with(SMBassLS,depletion(catch,effort,method="DeLury",Ricker.mod=TRUE))
  cf8 <- summary(ex8)
  ci8 <- confint(ex8)
}

test_that("depletion() with 'Leslie' matches fishmethod's 'deplet' for darter data",{
  if (require(fishmethods)) {
    expect_equal(round(cf1[["No","Estimate"]],0),round(cf1fm[["N","Estimate"]],0))
    expect_equal(round(cf1[["No","Std. Err."]],1),round(cf1fm[["N","SE"]],1))
    expect_equivalent(round(cf1["q",],7),round(cf1fm["q",],7))
    expect_equivalent(round(ci1["No",],1),round(ci1fm["N",],1))
    expect_equivalent(round(ci1["q",],6),round(ci1fm["q",],6))
  }
})

test_that("depletion() with 'Leslie' matches fishmethod's 'deplet' for lobster data",{
  if (require(fishmethods)) {
    expect_equal(round(cf3[["No","Estimate"]],0),round(cf3fm[["N","Estimate"]],0))
    expect_equal(round(cf3[["No","Std. Err."]],1),round(cf3fm[["N","SE"]],1))
    expect_equivalent(round(cf3["q",],7),round(cf3fm["q",],7))
    expect_equivalent(round(ci3["No",],1),round(ci3fm["N",],1))
    expect_equivalent(round(ci3["q",],5),round(ci3fm["q",],5))
  }
})

test_that("depletion() with 'Leslie' matches example 7.1 (p. 299) in Seber (2002) for lobster data",{
  if (require(FSAdata)) {
    expect_equal(round(cf3["No","Estimate"],1),120.5)
    expect_equal(round(cf3["q","Estimate"],4),0.0074)
    ## way off (52%)
    #expect_equal(round(ci3["No",],0),c(77,327))
    ## off by only 0.0001 for the LCI
    #expect_equal(round(ci3["q",],4),c(0.0058,0.0090))
  }
})

test_that("depletion() with 'Leslie' matches fishmethod's 'deplet' for blue crab data",{
  if (require(fishmethods)) {
    expect_equal(round(cf5[["No","Estimate"]],0),round(cf5fm[["N","Estimate"]],0))
    expect_equal(round(cf5[["No","Std. Err."]],1),round(cf5fm[["N","SE"]],1))
    expect_equivalent(round(cf5["q",],7),round(cf5fm["q",],7))
    expect_equivalent(round(ci5["No",],1),round(ci5fm["N",],1))
    expect_equivalent(round(ci5["q",],6),round(ci5fm["q",],6))
  }
})

test_that("depletion with 'Leslie' matches example 7.2 (p. 300) in Seber (2002) for blue crab",{
  if (require(FSAdata)) {
    ## Off by <0.3%
    #expect_equal(round(cf5["No","Estimate"],-2),330300)
    expect_equal(round(cf5["q","Estimate"],5),0.00056)
    ## off by ~2%
    #expect_equal(round(ci5["No",],-2),c(299600,373600))
    ## off by 0.00001 for the LCI
    #expect_equal(round(ci5["q",],5),c(0.00045,0.00067))
  }
})

test_that("depletion() with 'Leslie' matches fishmethod's 'deplet' for SMB data",{
  if (require(fishmethods)) {
    expect_equal(round(cf7[["No","Estimate"]],0),round(cf7fm[["N","Estimate"]],0))
    expect_equal(round(cf7[["No","Std. Err."]],1),round(cf7fm[["N","SE"]],1))
    expect_equivalent(round(cf7["q",],7),round(cf7fm["q",],7))
    expect_equivalent(round(ci7["No",],1),round(ci7fm["N",],1))
    expect_equivalent(round(ci7["q",],5),round(ci7fm["q",],5))
  }
})

test_that("depletion() with 'Leslie' with Ricker.mod matches example 6.1 (p. 151) in Ricker (1975).",{
  if (require(FSAdata)) {
    expect_equal(round(cf7r["No","Estimate"],0),1078)
    # multiplied by 7 because Ricker fit on Ct rather than CPEt but ft was a constant 7
    expect_equal(round(cf7r["q","Estimate"]*7,5),0.10676)
    ## Way different ... Ricker used a method by DeLury rather than Seber (2002)
    #expect_equal(round(ci7r["No",],0),c(814,2507))
  }
})


test_that("depletion() with 'DeLury' and Ricker.mod matches fishmethod's 'deplet'",{
  if (require(fishmethods)) {
    expect_equal(round(cf2[["No","Estimate"]],0),round(cf2fm[["N","Estimate"]],0))
    expect_equal(round(cf2[["No","Std. Err."]],1),round(cf2fm[["N","SE"]],1))
    expect_equivalent(round(cf2["q",],7),round(cf2fm["q",],7))
    expect_equivalent(round(ci2["No",],1),round(ci2fm["N",],1))
    expect_equivalent(round(ci2["q",],6),round(ci2fm["q",],6))
  }
})

test_that("depletion() with 'DeLury' and Ricker.mod matches fishmethod's 'deplet' for the lobster data",{
  if (require(fishmethods)) {
    expect_equal(round(cf4[["No","Estimate"]],0),round(cf4fm[["N","Estimate"]],0))
    expect_equal(round(cf4[["No","Std. Err."]],1),round(cf4fm[["N","SE"]],1))
    expect_equivalent(round(cf4["q",],7),round(cf4fm["q",],7))
    expect_equivalent(round(ci4["No",],1),round(ci4fm["N",],1))
    expect_equivalent(round(ci4["q",],7),round(ci4fm["q",],7))
  }
})

#test_that("depletion() with 'DeLury' matches DeLury (1947) for lobster data",{
#  if (require(FSAdata)) {
    ## REGRESSION RESULTS WERE DIFFERENT
    ## off by 4%
    #expect_equal(round(cf4["No","Estimate"],1),116.33)
    ## off by 7.7% (but that is fourth decimal) 
    # expect_equal(round(cf4[[1,"q"]],7),0.0079835)
#  }
#})

test_that("depletion() with 'DeLury' and Ricker.mod matches fishmethod's 'deplet' for the blue crab data",{
  if (require(fishmethods)) {
    expect_equal(round(cf6[["No","Estimate"]],0),round(cf6fm[["N","Estimate"]],0))
    expect_equal(round(cf6[["No","Std. Err."]],1),round(cf6fm[["N","SE"]],1))
    expect_equivalent(round(cf6["q",],7),round(cf6fm["q",],7))
    expect_equivalent(round(ci6["No",],1),round(ci6fm["N",],1))
    expect_equivalent(round(ci6["q",],5),round(ci6fm["q",],5))
  }
})

test_that("depletion() with 'DeLury' and Ricker.mod matches fishmethod's 'deplet' for the SMB data",{
  if (require(fishmethods)) {
    expect_equal(round(cf8[["No","Estimate"]],0),round(cf8fm[["N","Estimate"]],0))
    expect_equal(round(cf8[["No","Std. Err."]],1),round(cf8fm[["N","SE"]],1))
    expect_equivalent(round(cf8["q",],7),round(cf8fm["q",],7))
    expect_equivalent(round(ci8["No",],1),round(ci8fm["N",],1))
    expect_equivalent(round(ci8["q",],6),round(ci8fm["q",],6))
  }
})


test_that("depletion() with 'DeLury' with Ricker.mod matches example 6.2 (p. 154) in Ricker (1975).",{
  if (require(FSAdata)) {
    expect_equal(round(cf8["No","Estimate"],0),1150)
    expect_equal(round(cf8["q","Estimate"],5),0.01319)
  }
})
