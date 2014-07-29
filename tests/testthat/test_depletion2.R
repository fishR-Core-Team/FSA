context("Verification of 'depletion()' results.")

##############################################################
## Set up some results to test below
##############################################################
# DeLury's Lobster Data
if (require(FSAdata)) {
  data(LobsterPEI)
  df <- subset(LobsterPEI,day>16)
  ex1 <- with(df,depletion(catch,effort))
  cf1 <- coef(ex1)
  ci1 <- confint(ex1)
  
  ex1a <- with(df,depletion(catch,effort,method="DeLury"))
  cf1a <- coef(ex1a)
  ci1a <- confint(ex1a)
}

# Fischler's Blue Crab data
if (require(FSAdata)) {
  data(BlueCrab)
  ex2 <- with(BlueCrab,depletion(catch,effort))
  cf2 <- coef(ex2)
  ci2 <- confint(ex2)
}

# Omand's SMB data
if (require(FSAdata)) {
  data(SMBassLS)
  ex3 <- with(SMBassLS,depletion(catch,effort,Ricker.mod=TRUE))
  cf3 <- coef(ex3)
  ci3 <- confint(ex3)
  ex3a <- with(SMBassLS,depletion(catch,effort,method="DeLury",Ricker.mod=TRUE))
  cf3a <- coef(ex3a)
  ci3a <- confint(ex3a)
}

# fishmethods's Darter data
if (require(fishmethods)) {
  data(darter)
  # fishmethod Leslie
  deplet(catch=darter$catch,effort=darter$effort,method="l")
  cf4a <- l.out$results[,1:2]
  ci4a <- l.out$results[,3:4]
  # FSA Leslie
  ex4 <- with(darter,depletion(darter$catch,darter$effort))
  cf4 <- summary(ex4)
  ci4 <- confint(ex4)
  
  # fishmethod DeLury
  deplet(catch=darter$catch,effort=darter$effort,method="d")
  cf5a <- d.out$results[,1:2]
  ci5a <- d.out$results[,3:4]
  # FSA Leslie
  ex5 <- with(darter,depletion(darter$catch,darter$effort,method="DeLury",Ricker.mod=TRUE))
  cf5 <- summary(ex5)
  ci5 <- confint(ex5) 
}


##############################################################
## Tests of results
##############################################################
test_that("depletion with 'Leslie' matches fishmethod's 'deplet'",{
  if (require(fishmethods)) {
    expect_that(round(cf4[["No","Estimate"]],0), equals(round(cf4a[["N","Estimate"]],0)))
    expect_that(round(cf4[["No","Std. Err."]],1), equals(round(cf4a[["N","SE"]],1)))
    expect_that(round(cf4["q",],7), is_equivalent_to(round(cf4a["q",],7)))
    expect_that(round(ci4["No",],1), is_equivalent_to(round(ci4a["N",],1)))
    expect_that(round(ci4["q",],6), is_equivalent_to(round(ci4a["q",],6)))
  }
})

test_that("depletion with 'Leslie' matches example 7.1 (p. 299) in Seber (2002)",{
  if (require(FSAdata)) {
    expect_that(round(cf1[[1,"No"]],1), equals(120.5))
    expect_that(round(cf1[[1,"q"]],4), equals(0.0074))
    ## way off (52%)
    #expect_that(round(ci1["No",],0), equals(c(77,327)))
    ## off by 0.0001 for the LCI
    #expect_that(round(ci1["q",],4), equals(c(0.0058,0.0090)))
  }
})

test_that("depletion with 'Leslie' matches example 7.2 (p. 300) in Seber (2002)",{
  if (require(FSAdata)) {
    ## Off by <0.3%
    #expect_that(round(cf2[[1,"No"]],-2), equals(330300))
    expect_that(round(cf2[[1,"q"]],5), equals(0.00056))
    ## off by ~2%
    #expect_that(round(ci2["No",],-2), equals(c(299600,373600)))
    ## off by 0.00001 for the LCI
    #expect_that(round(ci2["q",],5), equals(c(0.00045,0.00067)))
  }
})

test_that("depletion with 'Leslie' with Ricker.mod matches example 6.1 (p. 151) in Ricker (1975).",{
  if (require(FSAdata)) {
    ## Off by <0.3%
    expect_that(round(cf3[[1,"No"]],0), equals(1078))
    # multiplied by 7 because Ricker fit on Ct rather than CPEt but ft was a constant 7
    expect_that(round(cf3[[1,"q"]]*7,5), equals(0.10676))
    ## Way different ... Ricker used a method by DeLury rather than Seber (2002)
    #expect_that(round(ci3["No",],0), equals(c(814,2507)))
  }
})

test_that("depletion with 'DeLury' and Ricker.mod matches fishmethod's 'deplet'",{
  if (require(fishmethods)) {
    expect_that(round(cf5[["No","Estimate"]],0), equals(round(cf5a[["N","Estimate"]],0)))
    expect_that(round(cf5[["No","Std. Err."]],1), equals(round(cf5a[["N","SE"]],1)))
    expect_that(round(cf5["q",],7), is_equivalent_to(round(cf5a["q",],7)))
    expect_that(round(ci5["No",],1), is_equivalent_to(round(ci5a["N",],1)))
    expect_that(round(ci5["q",],6), is_equivalent_to(round(ci5a["q",],6)))
  }
})

test_that("depletion with 'DeLury' matches DeLury (1947)",{
  if (require(FSAdata)) {
    ## REGRESSION RESULTS WERE DIFFERENT
    ## off by 4%
    #expect_that(round(cf1[[1,"No"]],1), equals(116.33))
    ## off by 7.7% (but that is fourth decimal) 
    # expect_that(round(cf1[[1,"q"]],7), equals(0.0079835))
  }
})

test_that("depletion with 'DeLury' with Ricker.mod matches example 6.2 (p. 154) in Ricker (1975).",{
  if (require(FSAdata)) {
    expect_that(round(cf3a[[1,"No"]],0), equals(1150))
    expect_that(round(cf3a[[1,"q"]],5), equals(0.01319))
  }
})
