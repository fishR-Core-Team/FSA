context("Catch Curve and Chapman-Robson")

data(BrookTroutTH)
BrookTroutTH$fact <- as.factor(sample(c("A","B"),nrow(BrookTroutTH),replace=TRUE))
d <- BrookTroutTH

test_that("catchCurve errors and warnings",{
  # bad variables
  expect_error(catchCurve(d$age,d$fact))
  expect_error(catchCurve(d$fact,d$age))
  expect_error(catchCurve(age~fact,data=d))
  expect_error(catchCurve(fact~age,data=d))
  # bad formulas
  expect_error(catchCurve(catch~age+fact,data=d))
  expect_error(catchCurve(catch+age~fact,data=d))
  # bad numbers of individuals
  expect_error(catchCurve(catch~age[-1],data=d))
  expect_error(catchCurve(catch[-1]~age,data=d))
  expect_error(catchCurve(d$age,d$catch[-1]))
  expect_error(catchCurve(d$age[-1],d$catch))
  # too few data
  expect_error(catchCurve(d$age[1],d$catch[1]))
  expect_error(catchCurve(d$age,d$catch,ages2use=3))
  # bad ages2use
  expect_warning(catchCurve(catch~age,data=d,ages2use=c(2:9)))
  expect_error(catchCurve(catch~age,data=d,ages2use=c(-1,2:6)))
})

test_that("chapmanRobson errors and warnings",{
  # bad variables
  expect_error(chapmanRobson(d$age,d$fact))
  expect_error(chapmanRobson(d$fact,d$age))
  expect_error(chapmanRobson(age~fact,data=d))
  expect_error(chapmanRobson(fact~age,data=d))
  # bad formulas
  expect_error(chapmanRobson(catch~age+fact,data=d))
  expect_error(chapmanRobson(catch+age~fact,data=d))
  # bad numbers of individuals
  expect_error(chapmanRobson(catch~age[-1],data=d))
  expect_error(chapmanRobson(catch[-1]~age,data=d))
  expect_error(chapmanRobson(d$age,d$catch[-1]))
  expect_error(chapmanRobson(d$age[-1],d$catch))
  # too few data
  expect_error(chapmanRobson(d$age[1],d$catch[1]))
  expect_error(chapmanRobson(d$age,d$catch,ages2use=3))
  # bad ages2use
  expect_warning(chapmanRobson(catch~age,data=d,ages2use=c(2:9)))
  expect_error(chapmanRobson(catch~age,data=d,ages2use=c(-1,2:6)))
})

test_that("catchCurve and chapmanRobson match Miranda & Bettoli (2007) boxes 6.3 and 6.4",{
  df <- data.frame(age=1:10,n=c(90,164,162,110,55,41,20,14,7,5))
  cc1 <- catchCurve(n~age,data=df,ages2use=3:10)
  scc1 <- summary(cc1)
  cc2 <- catchCurve(n~age,data=df,ages2use=3:10,weighted=TRUE)
  scc2 <- summary(cc2)  
  cr1 <- chapmanRobson(n~age,data=df,ages2use=3:10)
  scr1 <- summary(cr1)
  
  ## matches for catchCurve
  expect_equal(round(scc1["Z","Estimate"],5),0.51122)
  expect_equal(round(scc1["Z","Std. Error"],4),0.0156)
  expect_equal(round(scc2["Z","Estimate"],5),0.51139)
  
  ## SE does not match for chapmanRobson because B&M rounded S in SE calculation
  expect_equal(round(scr1["S","Estimate"]/100,3),0.580)
  #expect_equal(round(scr1["S","Std. Error"]/10,3),0.018)
})

test_that("catchCurve and chapmanRobson match results from fishmethods package",{
  if (require(fishmethods)) {
    ## get data
    data(rockbass)
    ## fishmethods results
    fm <- agesurv(age=rockbass$age,full=6)$results
    ## FSA results
    df <- data.frame(xtabs(~age,data=rockbass))
    df$age <- fact2num(df$age)
    cc1 <- catchCurve(Freq~age,data=df)
    scc1 <- summary(cc1) 
    cr1 <- chapmanRobson(Freq~age,data=df,zmethod="original")
    scr1 <- summary(cr1)
    cr2 <- chapmanRobson(Freq~age,data=df,zmethod="Smithetal")
    scr2 <- summary(cr2)    
    # catchCurve results match
    expect_equal(round(scc1["Z","Estimate"],2),fm$Estimate[2])
    expect_equal(round(scc1["Z","Std. Error"],3),fm$SE[2])
    # chapmanRobson results match
    expect_equal(round(scr1["Z","Estimate"],2),fm$Estimate[6])
    expect_equal(round(scr1["Z","Std. Error"],3),fm$SE[6])
    # chapmanRobson (with Smith et al. (2012) bias corrections)
    #   results match for the point estimaes but not the SE
    #   fishmethos appears to use eqn 5 from smith et al. for
    #   the uncorrected SE of Z, whereas FSA uses eqn 2
    expect_equal(round(scr2["Z","Estimate"],2),fm$Estimate[8])
    #expect_equal(round(scr2["Z","Std. Error"],3),fm$SE[8])
  }  
})

test_that("catchCurve and ChaptmanRobson handle NA values properly.",{
  ## matches for catchCurve
  df <- data.frame(age=1:10,n=c(90,164,162,110,55,41,20,14,7,5))
  cc1 <- catchCurve(n~age,data=df,ages2use=3:10)
  scc1 <- summary(cc1)
  cc2 <- catchCurve(n~age,data=df,ages2use=3:10,weighted=TRUE)
  scc2 <- summary(cc2)  
  
  dfA <- data.frame(age=0:12,n=c(NA,90,164,162,110,55,41,20,14,7,5,NA,NA))
  cc1A <- catchCurve(n~age,data=dfA,ages2use=3:12)
  scc1A <- summary(cc1A)
  cc2A <- catchCurve(n~age,data=dfA,ages2use=3:12,weighted=TRUE)
  scc2A <- summary(cc2A) 
  
  expect_equal(scc1["Z","Estimate"],scc1A["Z","Estimate"])
  expect_equal(scc1["Z","Std. Error"],scc1A["Z","Std. Error"])
  expect_equal(scc2["Z","Estimate"],scc2A["Z","Estimate"])
  expect_equal(scc2["Z","Std. Error"],scc2A["Z","Std. Error"])
  
  ## matches for chapmanRobson
  cr1 <- chapmanRobson(n~age,data=df,ages2use=3:10)
  scr1 <- summary(cr1)
  
  cr1A <- chapmanRobson(n~age,data=dfA,ages2use=3:12)
  scr1A <- summary(cr1A)
  
  expect_equal(scr1["Z","Estimate"],scr1A["Z","Estimate"])
  expect_equal(scr1["Z","Std. Error"],scr1A["Z","Std. Error"])
  expect_equal(scr1["S","Estimate"],scr1A["S","Estimate"])
  expect_equal(scr1["S","Std. Error"],scr1A["S","Std. Error"])
})
