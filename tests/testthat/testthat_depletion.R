## Results for validation tests below ----
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

## Test Messages ----
test_that("depletion() messages",{
  ## wrong type
  expect_error(depletion(c(346,184,49),rep(7,3),method="Derek"),
               "should be one of")
  ## no efforts
  expect_error(depletion(c(346,184,49)),
               '"effort" is missing')
  ## bad types of data
  expect_error(depletion(c(346,184,"Derek"),rep(7,3)),
               "must be a numeric vector")
  expect_error(depletion(c(346,184,49),rep("Derek",3)),
               "must be a numeric vector")
  ## different vector sizes
  expect_error(depletion(c(346,184,49,57),rep(7,3)),
               "must be same length")
  expect_error(depletion(c(346,184,49),rep(7,4)),
               "must be same length")
  ## too few catches
  expect_error(depletion(c(346,184),rep(7,2)),
               "Must have at least 3 values")
  ## negative catchs or non-non-negative efforts
  expect_error(depletion(c(346,184,-49),rep(7,3)),
               "must be non-negative")
  expect_error(depletion(c(346,184,49),c(7,3,-1)),
               "must be positive")
  expect_error(depletion(c(346,184,49),c(7,3,0)),
               "must be positive")
  ## zero catches with DeLury method
  expect_error(depletion(c(346,184,0),rep(7,3),method="DeLury"),
               "Can't have zero")
  ## Bad regressions (i.e., have non-significant or positive slopes)
  expect_warning(depletion(c(49,184,346),rep(7,3)),
                 "Estimates are suspect")
  expect_warning(depletion(c(346,144,341),rep(7,3)),
                 "Estimates are suspect")
  expect_warning(depletion(c(49,184,346),rep(7,3),method="DeLury"),
                 "Estimates are suspect")
  expect_warning(depletion(c(346,144,341),rep(7,3),method="DeLury"),
                 "Estimates are suspect")
  
  suppressWarnings(ex1 <- depletion(c(346,184,49),rep(1,3)))
  ## wrong type in methods
  expect_error(summary(ex1,parm="Derek"),
               "should be one of")
  expect_error(coef(ex1,parm="Derek"),
               "should be one of")
  expect_error(confint(ex1,parm="Derek"),
               "should be one of")
  expect_error(confint(ex1,conf.level=0),
               "must be between 0 and 1")
  expect_error(confint(ex1,conf.level=1),
               "must be between 0 and 1")
})


## Test Output Types ----
test_that("depletion() output types",{
  l1 <- depletion(SMBassLS$catch,SMBassLS$effort,method="Leslie")
  expect_is(l1,"depletion")
  # coef()s
  l1A <- coef(l1)
  expect_true(is.vector(l1A))
  expect_is(l1A,"numeric")
  expect_equal(length(l1A),2)
  expect_equal(names(l1A),c("No","q"))
  l1A <- coef(l1,parm="No")
  expect_true(is.vector(l1A))
  expect_is(l1A,"numeric")
  expect_equal(length(l1A),1)
  expect_equal(names(l1A),c("No"))
  l1A <- coef(l1,parm="lm")
  expect_true(is.vector(l1A))
  expect_is(l1A,"numeric")
  expect_equal(length(l1A),2)
  expect_equal(names(l1A),c("(Intercept)","K"))
  # confint()s
  l1A <- confint(l1)
  expect_true(is.matrix(l1A))
  expect_equal(mode(l1A),"numeric")
  expect_equal(nrow(l1A),2)
  expect_equal(ncol(l1A),2)
  expect_equal(rownames(l1A),c("No","q"))
  expect_equal(colnames(l1A),c("95% LCI","95% UCI"))
  l1A <- confint(l1,parm="No")
  expect_true(is.matrix(l1A))
  expect_equal(mode(l1A),"numeric")
  expect_equal(ncol(l1A),2)
  expect_equal(nrow(l1A),1)
  expect_equal(rownames(l1A),c("No"))
  expect_equal(colnames(l1A),c("95% LCI","95% UCI"))
  l1A <- confint(l1,parm="lm")
  expect_true(is.matrix(l1A))
  expect_equal(mode(l1A),"numeric")
  expect_equal(nrow(l1A),2)
  expect_equal(ncol(l1A),2)
  expect_equal(rownames(l1A),c("(Intercept)","K"))
  expect_equal(colnames(l1A),c("95% LCI","95% UCI"))
  # summary()s
  l1A <- summary(l1)
  expect_true(is.matrix(l1A))
  expect_equal(mode(l1A),"numeric")
  expect_equal(nrow(l1A),2)
  expect_equal(ncol(l1A),2)
  expect_equal(rownames(l1A),c("No","q"))
  expect_equal(colnames(l1A),c("Estimate","Std. Err."))
  l1A <- summary(l1,parm="No")
  expect_true(is.matrix(l1A))
  expect_equal(mode(l1A),"numeric")
  expect_equal(nrow(l1A),1)
  expect_equal(ncol(l1A),2)
  expect_equal(rownames(l1A),c("No"))
  expect_equal(colnames(l1A),c("Estimate","Std. Err."))
  l1A <- summary(l1,parm="lm")
  expect_equal(class(l1A),"summary.lm")
  expect_equal(mode(l1A),"list")
  # anova()s
  l1A <- anova(l1)
  expect_equal(class(l1A),c("anova","data.frame"))
  expect_equal(mode(l1A),"list")
  # r-squared
  expect_true(is.numeric(rSquared(l1)))
  
  d1 <- depletion(SMBassLS$catch,SMBassLS$effort,method="DeLury")
  expect_is(d1,"depletion")
  # coef()s
  d1A <- coef(d1)
  expect_true(is.vector(d1A))
  expect_is(d1A,"numeric")
  expect_equal(length(d1A),2)
  expect_equal(names(d1A),c("No","q"))
  d1A <- coef(d1,parm="No")
  expect_true(is.vector(d1A))
  expect_is(d1A,"numeric")
  expect_equal(length(d1A),1)
  expect_equal(names(d1A),c("No"))
  d1A <- coef(d1,parm="lm")
  expect_true(is.vector(d1A))
  expect_is(d1A,"numeric")
  expect_equal(length(d1A),2)
  # confint()s
  d1A <- confint(d1)
  expect_true(is.matrix(d1A))
  expect_equal(mode(d1A),"numeric")
  expect_equal(nrow(d1A),2)
  expect_equal(ncol(d1A),2)
  expect_equal(rownames(d1A),c("No","q"))
  expect_equal(colnames(d1A),c("95% LCI","95% UCI"))
  d1A <- confint(d1,parm="No")
  expect_true(is.matrix(d1A))
  expect_equal(mode(d1A),"numeric")
  expect_equal(length(d1A),2)
  expect_equal(rownames(d1A),c("No"))
  expect_equal(colnames(d1A),c("95% LCI","95% UCI"))
  d1A <- confint(d1,parm="lm")
  expect_true(is.matrix(d1A))
  expect_equal(mode(d1A),"numeric")
  expect_equal(nrow(d1A),2)
  expect_equal(ncol(d1A),2)
  expect_equal(colnames(d1A),c("95% LCI","95% UCI"))
  # summary()s
  d1A <- summary(d1)
  expect_true(is.matrix(d1A))
  expect_equal(mode(d1A),"numeric")
  expect_equal(nrow(d1A),2)
  expect_equal(ncol(d1A),2)
  expect_equal(rownames(d1A),c("No","q"))
  expect_equal(colnames(d1A),c("Estimate","Std. Err."))
  d1A <- summary(d1,parm="No")
  expect_true(is.matrix(d1A))
  expect_equal(mode(d1A),"numeric")
  expect_equal(nrow(d1A),1)
  expect_equal(ncol(d1A),2)
  expect_equal(rownames(d1A),c("No"))
  expect_equal(colnames(d1A),c("Estimate","Std. Err."))
  d1A <- summary(d1,parm="lm")
  expect_equal(class(d1A),"summary.lm")
  expect_equal(mode(d1A),"list")
  # anova()s
  d1A <- anova(d1)
  expect_equal(class(d1A),c("anova","data.frame"))
  expect_equal(mode(d1A),"list")
  # r-squared
  expect_true(is.numeric(rSquared(d1)))
})


## Validate Results ----
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

