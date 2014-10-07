context("depletion() Tests")

# ############################################################
# ============================================================
# Messaging
# ============================================================
# ############################################################

test_that("depletion() errors and warnings",{
  ## wrong type
  expect_that(depletion(c(346,184,49),rep(7,3),method="Derek"),throws_error())
  ## no efforts
  expect_that(depletion(c(346,184,49)),throws_error())
  ## bad types of data
  expect_that(depletion(c(346,184,"Derek"),rep(7,3)),throws_error())
  expect_that(depletion(c(346,184,49),rep("Derek",3)),throws_error())
  ## different vector sizes
  expect_that(depletion(c(346,184,49,57),rep(7,3)),throws_error())
  expect_that(depletion(c(346,184,49),rep(7,4)),throws_error())
  ## too few catches
  expect_that(depletion(c(346,184),rep(7,2)),throws_error())
  ## negative catchs or non-non-negative efforts
  expect_that(depletion(c(346,184,-49),rep(7,3)),throws_error())
  expect_that(depletion(c(346,184,49),c(7,3,-1)),throws_error())
  expect_that(depletion(c(346,184,49),c(7,3,0)),throws_error())
  ## zero catches with DeLury method
  expect_that(depletion(c(346,184,0),rep(7,3),method="DeLury"),throws_error())
  ## Bad regressions (i.e., have non-significant or positive slopes)
  expect_that(depletion(c(49,184,346),rep(7,3)),gives_warning())
  expect_that(depletion(c(346,144,341),rep(7,3)),gives_warning())
  expect_that(depletion(c(49,184,346),rep(7,3),method="DeLury"),gives_warning())
  expect_that(depletion(c(346,144,341),rep(7,3),method="DeLury"),gives_warning())
  
  suppressWarnings(ex1 <- depletion(c(346,184,49),rep(1,3)))
  ## wrong type in methods
  expect_that(summary(ex5,type="Derek"),throws_error())
  expect_that(coef(ex5,type="Derek"),throws_error())
  expect_that(confint(ex5,parm="Derek"),throws_error())
})


# ############################################################
# ============================================================
# Analytical Results
# ============================================================
# ############################################################

# ------------------------------------------------------------
# Set up some results to test below
# ------------------------------------------------------------
# fishmethods's Darter data
if (require(fishmethods)) {
  data(darter)
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
  ex2 <- with(darter,depletion(darter$catch,darter$effort,method="DeLury",Ricker.mod=TRUE))
  cf2 <- summary(ex2)
  ci2 <- confint(ex2) 
}

# DeLury's Lobster Data
if (require(FSAdata)) {
  data(LobsterPEI)
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
  data(BlueCrab)
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
if (require(FSAdata)) {
  data(SMBassLS)
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


# ------------------------------------------------------------
# Tests of LESLIE results
# ------------------------------------------------------------
test_that("depletion with 'Leslie' matches fishmethod's 'deplet' for darter data",{
  if (require(fishmethods)) {
    expect_that(round(cf1[["No","Estimate"]],0), equals(round(cf1fm[["N","Estimate"]],0)))
    expect_that(round(cf1[["No","Std. Err."]],1), equals(round(cf1fm[["N","SE"]],1)))
    expect_that(round(cf1["q",],7), is_equivalent_to(round(cf1fm["q",],7)))
    expect_that(round(ci1["No",],1), is_equivalent_to(round(ci1fm["N",],1)))
    expect_that(round(ci1["q",],6), is_equivalent_to(round(ci1fm["q",],6)))
  }
})

test_that("depletion with 'Leslie' matches fishmethod's 'deplet' for lobster data",{
  if (require(fishmethods)) {
    expect_that(round(cf3[["No","Estimate"]],0), equals(round(cf3fm[["N","Estimate"]],0)))
    expect_that(round(cf3[["No","Std. Err."]],1), equals(round(cf3fm[["N","SE"]],1)))
    expect_that(round(cf3["q",],7), is_equivalent_to(round(cf3fm["q",],7)))
    expect_that(round(ci3["No",],1), is_equivalent_to(round(ci3fm["N",],1)))
    expect_that(round(ci3["q",],5), is_equivalent_to(round(ci3fm["q",],5)))
  }
})

test_that("depletion with 'Leslie' matches example 7.1 (p. 299) in Seber (2002) for lobster data",{
  if (require(FSAdata)) {
    expect_that(round(cf3["No","Estimate"],1), equals(120.5))
    expect_that(round(cf3["q","Estimate"],4), equals(0.0074))
    ## way off (52%)
    #expect_that(round(ci3["No",],0), equals(c(77,327)))
    ## off by only 0.0001 for the LCI
    #expect_that(round(ci3["q",],4), equals(c(0.0058,0.0090)))
  }
})

test_that("depletion with 'Leslie' matches fishmethod's 'deplet' for blue crab data",{
  if (require(fishmethods)) {
    expect_that(round(cf5[["No","Estimate"]],0), equals(round(cf5fm[["N","Estimate"]],0)))
    expect_that(round(cf5[["No","Std. Err."]],1), equals(round(cf5fm[["N","SE"]],1)))
    expect_that(round(cf5["q",],7), is_equivalent_to(round(cf5fm["q",],7)))
    expect_that(round(ci5["No",],1), is_equivalent_to(round(ci5fm["N",],1)))
    expect_that(round(ci5["q",],6), is_equivalent_to(round(ci5fm["q",],6)))
  }
})

test_that("depletion with 'Leslie' matches example 7.2 (p. 300) in Seber (2002) for blue crab",{
  if (require(FSAdata)) {
    ## Off by <0.3%
    #expect_that(round(cf5["No","Estimate"],-2), equals(330300))
    expect_that(round(cf5["q","Estimate"],5), equals(0.00056))
    ## off by ~2%
    #expect_that(round(ci5["No",],-2), equals(c(299600,373600)))
    ## off by 0.00001 for the LCI
    #expect_that(round(ci5["q",],5), equals(c(0.00045,0.00067)))
  }
})

test_that("depletion with 'Leslie' matches fishmethod's 'deplet' for SMB data",{
  if (require(fishmethods)) {
    expect_that(round(cf7[["No","Estimate"]],0), equals(round(cf7fm[["N","Estimate"]],0)))
    expect_that(round(cf7[["No","Std. Err."]],1), equals(round(cf7fm[["N","SE"]],1)))
    expect_that(round(cf7["q",],7), is_equivalent_to(round(cf7fm["q",],7)))
    expect_that(round(ci7["No",],1), is_equivalent_to(round(ci7fm["N",],1)))
    expect_that(round(ci7["q",],5), is_equivalent_to(round(ci7fm["q",],5)))
  }
})

test_that("depletion with 'Leslie' with Ricker.mod matches example 6.1 (p. 151) in Ricker (1975).",{
  if (require(FSAdata)) {
    expect_that(round(cf7r["No","Estimate"],0), equals(1078))
    # multiplied by 7 because Ricker fit on Ct rather than CPEt but ft was a constant 7
    expect_that(round(cf7r["q","Estimate"]*7,5), equals(0.10676))
    ## Way different ... Ricker used a method by DeLury rather than Seber (2002)
    #expect_that(round(ci7r["No",],0), equals(c(814,2507)))
  }
})


# ------------------------------------------------------------
# Tests of DELURY results
# ------------------------------------------------------------
test_that("depletion with 'DeLury' and Ricker.mod matches fishmethod's 'deplet'",{
  if (require(fishmethods)) {
    expect_that(round(cf2[["No","Estimate"]],0), equals(round(cf2fm[["N","Estimate"]],0)))
    expect_that(round(cf2[["No","Std. Err."]],1), equals(round(cf2fm[["N","SE"]],1)))
    expect_that(round(cf2["q",],7), is_equivalent_to(round(cf2fm["q",],7)))
    expect_that(round(ci2["No",],1), is_equivalent_to(round(ci2fm["N",],1)))
    expect_that(round(ci2["q",],6), is_equivalent_to(round(ci2fm["q",],6)))
  }
})

test_that("depletion with 'DeLury' and Ricker.mod matches fishmethod's 'deplet' for the lobster data",{
  if (require(fishmethods)) {
    expect_that(round(cf4[["No","Estimate"]],0), equals(round(cf4fm[["N","Estimate"]],0)))
    expect_that(round(cf4[["No","Std. Err."]],1), equals(round(cf4fm[["N","SE"]],1)))
    expect_that(round(cf4["q",],7), is_equivalent_to(round(cf4fm["q",],7)))
    expect_that(round(ci4["No",],1), is_equivalent_to(round(ci4fm["N",],1)))
    expect_that(round(ci4["q",],7), is_equivalent_to(round(ci4fm["q",],7)))
  }
})

test_that("depletion with 'DeLury' matches DeLury (1947) for lobster data",{
  if (require(FSAdata)) {
    ## REGRESSION RESULTS WERE DIFFERENT
    ## off by 4%
    #expect_that(round(cf4["No","Estimate"],1), equals(116.33))
    ## off by 7.7% (but that is fourth decimal) 
    # expect_that(round(cf4[[1,"q"]],7), equals(0.0079835))
  }
})

test_that("depletion with 'DeLury' and Ricker.mod matches fishmethod's 'deplet' for the blue crab data",{
  if (require(fishmethods)) {
    expect_that(round(cf6[["No","Estimate"]],0), equals(round(cf6fm[["N","Estimate"]],0)))
    expect_that(round(cf6[["No","Std. Err."]],1), equals(round(cf6fm[["N","SE"]],1)))
    expect_that(round(cf6["q",],7), is_equivalent_to(round(cf6fm["q",],7)))
    expect_that(round(ci6["No",],1), is_equivalent_to(round(ci6fm["N",],1)))
    expect_that(round(ci6["q",],5), is_equivalent_to(round(ci6fm["q",],5)))
  }
})

test_that("depletion with 'DeLury' and Ricker.mod matches fishmethod's 'deplet' for the SMB data",{
  if (require(fishmethods)) {
    expect_that(round(cf8[["No","Estimate"]],0), equals(round(cf8fm[["N","Estimate"]],0)))
    expect_that(round(cf8[["No","Std. Err."]],1), equals(round(cf8fm[["N","SE"]],1)))
    expect_that(round(cf8["q",],7), is_equivalent_to(round(cf8fm["q",],7)))
    expect_that(round(ci8["No",],1), is_equivalent_to(round(ci8fm["N",],1)))
    expect_that(round(ci8["q",],6), is_equivalent_to(round(ci8fm["q",],6)))
  }
})


test_that("depletion with 'DeLury' with Ricker.mod matches example 6.2 (p. 154) in Ricker (1975).",{
  if (require(FSAdata)) {
    expect_that(round(cf8["No","Estimate"],0), equals(1150))
    expect_that(round(cf8["q","Estimate"],5), equals(0.01319))
  }
})
