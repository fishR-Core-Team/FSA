context("Catch Curve and Chapman-Robson VALIDATE")

test_that("catchCurve() and chapmanRobson() match Miranda & Bettoli (2007) boxes 6.3 and 6.4",{
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

test_that("catchCurve() and chapmanRobson() match results from fishmethods package",{
  if (require(fishmethods)) {
    ## get data
    data(rockbass,package="fishmethods")
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
    expect_equal(round(scc1["Z","Estimate"],2),
                 fm$Estimate[fm$Method=="Linear Regression" & fm$Parameter=="Z"])
    expect_equal(round(scc1["Z","Std. Error"],3),
                 fm$SE[fm$Method=="Linear Regression" & fm$Parameter=="Z"])
    # chapmanRobson results match
    expect_equal(round(scr1["Z","Estimate"],2),
                 fm$Estimate[fm$Method=="Chapman-Robson" & fm$Parameter=="Z"])
    expect_equal(round(scr1["Z","Std. Error"],3),
                 fm$SE[fm$Method=="Chapman-Robson" & fm$Parameter=="Z"])
    # chapmanRobson (with Smith et al. (2012) bias corrections)
    #   results match for the point estimaes but not the SE
    #   fishmethods appears to use eqn 5 from smith et al. for
    #   the uncorrected SE of Z, whereas FSA uses eqn 2
    expect_equal(round(scr2["Z","Estimate"],2),
                 fm$Estimate[fm$Method=="Chapman-Robson CB" & fm$Parameter=="Z"])
    #expect_equal(round(scr2["Z","Std. Error"],3),fm$SE[fm$Method=="Chapman-Robson CB" & fm$Parameter=="Z"])
  }  
})
