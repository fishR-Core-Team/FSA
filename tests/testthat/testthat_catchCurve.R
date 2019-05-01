## Data for testing ----
BrookTroutTH$fact <- as.factor(sample(c("A","B"),nrow(BrookTroutTH),replace=TRUE))
d <- BrookTroutTH

cc <- catchCurve(catch~age,data=d,ages2use=2:6)
cc2 <- catchCurve(catch~age,data=d,ages2use=2:6,weighted=TRUE)
cr <- chapmanRobson(catch~age,data=d,ages2use=2:6)
cr1 <- chapmanRobson(catch~age,data=d,ages2use=2:6,zmethod="Hoenigetal")
cr2 <- chapmanRobson(catch~age,data=d,ages2use=2:6,zmethod="original")


## Test Messages ----
test_that("catchCurve() messages",{
  # bad variables
  expect_error(catchCurve(d$age,d$fact),
               "must be numeric")
  expect_error(catchCurve(d$fact,d$age),
               "must be numeric")
  expect_error(catchCurve(age~fact,data=d),
               "must be numeric")
  expect_error(catchCurve(fact~age,data=d),
               "must be numeric")
  # bad formulas
  expect_error(catchCurve(catch~age+fact,data=d),
               "only one RHS variable")
  expect_error(catchCurve(catch+age~fact,data=d),
               "more than one variable on the LHS")
  # bad numbers of individuals
  expect_error(catchCurve(catch~age[-1],data=d),
               "variable lengths differ")
  expect_error(catchCurve(catch[-1]~age,data=d),
               "variable lengths differ")
  expect_error(catchCurve(d$age,d$catch[-1]),
               "are different lengths")
  expect_error(catchCurve(d$age[-1],d$catch),
               "are different lengths")
  # too few data
  expect_error(catchCurve(d$age[1],d$catch[1]),
               "Fewer than 2 data points")
  expect_error(catchCurve(d$age,d$catch,ages2use=3),
               "Fewer than 2 data points")
  # bad ages2use
  expect_warning(catchCurve(catch~age,data=d,ages2use=c(2:9)),
                 "not in observed ages")
  expect_error(catchCurve(catch~age,data=d,ages2use=c(-1,2:6)),
               "all positive or negative")
  # bad args in coef
  expect_error(coef(cc,parm="derek"),
               "should be one of")
  # bad args in confint
  expect_error(confint(cc,parm="derek"),
               "should be one of")
  expect_error(confint(cc,conf.level=0),
               "must be between 0 and 1")
  expect_error(confint(cc,conf.level=1),
               "must be between 0 and 1")
  # bad args in summary
  expect_error(summary(cc,parm="derek"),
               "should be one of")
  
  # How does catchCurve() handle negative weights
  d <- data.frame(catch=c(10,5,3,1,1,1),age=1:6)
  expect_warning(catchCurve(catch~age,data=d,weighted=TRUE),
                 "Non-positive weights were set to 0")
  expect_warning(catchCurve(catch~age,data=d,
                            weighted=TRUE,negWeightReplace=3),
                 "Non-positive weights were set to 3")
  expect_error(catchCurve(catch~age,data=d,
                          weighted=TRUE,negWeightReplace=-1),
               "must be non-negative")
})

test_that("chapmanRobson errors and warnings",{
  # bad zmethod
  expect_error(chapmanRobson(catch~age,data=d,ages2use=2:6,zmethod="Derek"),
               "should be one of")
  # bad variables
  expect_error(chapmanRobson(d$age,d$fact),
               "must be numeric")
  expect_error(chapmanRobson(d$fact,d$age),
               "must be numeric")
  expect_error(chapmanRobson(age~fact,data=d),
               "must be numeric")
  expect_error(chapmanRobson(fact~age,data=d),
               "must be numeric")
  # bad formulas
  expect_error(chapmanRobson(catch~age+fact,data=d),
               "only one RHS variable")
  expect_error(chapmanRobson(catch+age~fact,data=d),
               "more than one variable on the LHS")
  # bad numbers of individuals
  expect_error(chapmanRobson(catch~age[-1],data=d),
               "variable lengths differ")
  expect_error(chapmanRobson(catch[-1]~age,data=d),
               "variable lengths differ")
  expect_error(chapmanRobson(d$age,d$catch[-1]),
               "are different lengths")
  expect_error(chapmanRobson(d$age[-1],d$catch),
               "are different lengths")
  # too few data
  expect_error(chapmanRobson(d$age[1],d$catch[1]),
               "Fewer than 2 data points")
  expect_error(chapmanRobson(d$age,d$catch,ages2use=3),
               "Fewer than 2 data points")
  # bad ages2use
  expect_warning(chapmanRobson(catch~age,data=d,ages2use=c(2:9)),
                 "not in observed ages")
  expect_error(chapmanRobson(catch~age,data=d,ages2use=c(-1,2:6)),
               "all positive or negative")
  # bad choice of axis types
  expect_error(plot(cr,axis.age="derek"),
               "should be one of")
  # bad args in summary and coef
  expect_error(summary(cr,parm="derek"),
               "should be one of")
  expect_error(coef(cr,parm="derek"),
               "should be one of")
  # bad args in confint
  expect_error(confint(cr,parm="derek"),
               "should be one of")
  expect_error(confint(cr,conf.level=0),
               "must be between 0 and 1")
  expect_error(confint(cr,conf.level=1),
               "must be between 0 and 1")
})


## Test Output Types ----
test_that("catchCurve() outpute types",{
  expect_is(cc,"catchCurve")
  # coef, unweighted
  ccA <- coef(cc)
  expect_true(is.vector(ccA))
  expect_is(ccA,"numeric")
  expect_equal(length(ccA),2)
  expect_equal(names(ccA),c("Z","A"))
  ccA <- coef(cc,parm="Z")
  expect_true(is.vector(ccA))
  expect_is(ccA,"numeric")
  expect_equal(length(ccA),1)
  expect_equal(names(ccA),c("Z"))
  ccA <- coef(cc,parm="lm")
  expect_true(is.vector(ccA))
  expect_is(ccA,"numeric")
  expect_equal(length(ccA),2)
  expect_equal(names(ccA),c("(Intercept)","age.e"))
  # coef, weighted
  cc2A <- coef(cc2)
  expect_true(is.vector(cc2A))
  expect_is(cc2A,"numeric")
  expect_equal(length(cc2A),2)
  expect_equal(names(cc2A),c("Z","A"))
  cc2A <- coef(cc2,parm="A")
  expect_true(is.vector(cc2A))
  expect_is(cc2A,"numeric")
  expect_equal(length(cc2A),1)
  expect_equal(names(cc2A),c("A"))
  cc2A <- coef(cc2,parm="lm")
  expect_true(is.vector(cc2A))
  expect_is(cc2A,"numeric")
  expect_equal(length(cc2A),2)
  expect_equal(names(cc2A),c("(Intercept)","age.e"))
  # confint, unweighted
  ccA <- confint(cc)
  expect_is(ccA,"matrix")
  expect_equal(mode(ccA),"numeric")
  expect_equal(nrow(ccA),2)
  expect_equal(ncol(ccA),2)
  expect_equal(rownames(ccA),c("Z","A"))
  expect_equal(colnames(ccA),c("95% LCI","95% UCI"))
  ccA <- confint(cc,parm="Z")
  expect_is(ccA,"matrix")
  expect_equal(mode(ccA),"numeric")
  expect_equal(nrow(ccA),1)
  expect_equal(ncol(ccA),2)
  expect_equal(rownames(ccA),c("Z"))
  expect_equal(colnames(ccA),c("95% LCI","95% UCI"))
  ccA <- confint(cc,parm="lm")
  expect_is(ccA,"matrix")
  expect_equal(mode(ccA),"numeric")
  expect_equal(nrow(ccA),2)
  expect_equal(ncol(ccA),2)
  expect_equal(rownames(ccA),c("(Intercept)","age.e"))
  expect_equal(colnames(ccA),c("95% LCI","95% UCI"))
  # confint, weighted
  cc2A <- confint(cc2)
  expect_is(cc2A,"matrix")
  expect_equal(mode(cc2A),"numeric")
  expect_equal(nrow(cc2A),2)
  expect_equal(ncol(cc2A),2)
  expect_equal(rownames(cc2A),c("Z","A"))
  expect_equal(colnames(cc2A),c("95% LCI","95% UCI"))
  cc2A <- confint(cc2,parm="Z")
  expect_is(cc2A,"matrix")
  expect_equal(mode(cc2A),"numeric")
  expect_equal(nrow(cc2A),1)
  expect_equal(ncol(cc2A),2)
  expect_equal(rownames(cc2A),c("Z"))
  expect_equal(colnames(cc2A),c("95% LCI","95% UCI"))
  cc2A <- confint(cc2,parm="lm")
  expect_is(cc2A,"matrix")
  expect_equal(mode(cc2A),"numeric")
  expect_equal(nrow(cc2A),2)
  expect_equal(ncol(cc2A),2)
  expect_equal(rownames(cc2A),c("(Intercept)","age.e"))
  expect_equal(colnames(cc2A),c("95% LCI","95% UCI"))
  # summary
  cc2A <- summary(cc2)
  expect_is(cc2A,"matrix")
  expect_equal(mode(cc2A),"numeric")
  expect_equal(nrow(cc2A),2)
  expect_equal(ncol(cc2A),4)
  expect_equal(rownames(cc2A),c("Z","A"))
  expect_equal(colnames(cc2A),c("Estimate","Std. Error","t value","Pr(>|t|)"))
  cc2A <- summary(cc2,parm="Z")
  expect_is(cc2A,"matrix")
  expect_equal(mode(cc2A),"numeric")
  expect_equal(nrow(cc2A),1)
  expect_equal(ncol(cc2A),4)
  expect_equal(rownames(cc2A),c("Z"))
  expect_equal(colnames(cc2A),c("Estimate","Std. Error","t value","Pr(>|t|)"))
  cc2A <- summary(cc2,parm="lm")
  expect_is(cc2A,"summary.lm")
  
  # r-squared
  expect_true(is.numeric(rSquared(cc)))
  expect_true(is.numeric(rSquared(cc2)))
  
  # anova
  expect_equal(anova(cc2),anova(cc2$lm))
})

test_that("chapmanRobson() output types",{
  expect_is(cr,"chapmanRobson")
  # coef
  crA <- coef(cr)
  expect_true(is.vector(crA))
  expect_is(crA,"numeric")
  expect_equal(length(crA),2)
  expect_equal(names(crA),c("S","Z"))
  crA <- coef(cr,parm="S")
  expect_true(is.vector(crA))
  expect_is(crA,"numeric")
  expect_equal(length(crA),1)
  expect_equal(names(crA),c("S"))
  # summary
  crA <- summary(cr)
  expect_is(crA,"matrix")
  expect_equal(mode(crA),"numeric")
  expect_equal(nrow(crA),2)
  expect_equal(ncol(crA),2)
  expect_equal(rownames(crA),c("S","Z"))
  expect_equal(colnames(crA),c("Estimate","Std. Error"))
  crA <- summary(cr,parm="S")
  expect_is(crA,"matrix")
  expect_equal(mode(crA),"numeric")
  expect_equal(nrow(crA),1)
  expect_equal(ncol(crA),2)
  expect_equal(rownames(crA),c("S"))
  expect_equal(colnames(crA),c("Estimate","Std. Error"))
  # confint
  crA <- confint(cr)
  expect_is(crA,"matrix")
  expect_equal(mode(crA),"numeric")
  expect_equal(nrow(crA),2)
  expect_equal(ncol(crA),2)
  expect_equal(rownames(crA),c("S","Z"))
  expect_equal(colnames(crA),c("95% LCI","95% UCI"))
  crA <- confint(cr,parm="S")
  expect_is(crA,"matrix")
  expect_equal(mode(crA),"numeric")
  expect_equal(nrow(crA),1)
  expect_equal(ncol(crA),2)
  expect_equal(rownames(crA),c("S"))
  expect_equal(colnames(crA),c("95% LCI","95% UCI"))
  
  expect_is(cr1,"chapmanRobson")
  # coef
  crA <- coef(cr1)
  expect_true(is.vector(crA))
  expect_is(crA,"numeric")
  expect_equal(length(crA),2)
  expect_equal(names(crA),c("S","Z"))
  # summary
  crA <- summary(cr1)
  expect_is(crA,"matrix")
  expect_equal(mode(crA),"numeric")
  expect_equal(nrow(crA),2)
  expect_equal(ncol(crA),2)
  expect_equal(rownames(crA),c("S","Z"))
  expect_equal(colnames(crA),c("Estimate","Std. Error"))
  # confint
  crA <- confint(cr1)
  expect_is(crA,"matrix")
  expect_equal(mode(crA),"numeric")
  expect_equal(nrow(crA),2)
  expect_equal(ncol(crA),2)
  expect_equal(rownames(crA),c("S","Z"))
  expect_equal(colnames(crA),c("95% LCI","95% UCI"))
  expect_is(cr1,"chapmanRobson")
  
  # coef
  crA <- coef(cr2)
  expect_true(is.vector(crA))
  expect_is(crA,"numeric")
  expect_equal(length(crA),2)
  expect_equal(names(crA),c("S","Z"))
  # summary
  crA <- summary(cr2)
  expect_is(crA,"matrix")
  expect_equal(mode(crA),"numeric")
  expect_equal(nrow(crA),2)
  expect_equal(ncol(crA),2)
  expect_equal(rownames(crA),c("S","Z"))
  expect_equal(colnames(crA),c("Estimate","Std. Error"))
  # confint
  crA <- confint(cr2)
  expect_is(crA,"matrix")
  expect_equal(mode(crA),"numeric")
  expect_equal(nrow(crA),2)
  expect_equal(ncol(crA),2)
  expect_equal(rownames(crA),c("S","Z"))
  expect_equal(colnames(crA),c("95% LCI","95% UCI"))
})

test_that("catchCurve() and ChaptmanRobson() handle NA values properly.",{
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

test_that("catchCurve() and ChaptmanRobson() does negative ages2use properly.",{
  ## matches for catchCurve
  df <- data.frame(age=1:10,n=c(90,164,162,110,55,41,20,14,7,5))
  cc1 <- catchCurve(n~age,data=df,ages2use=3:10)
  scc1 <- summary(cc1)
  cc2 <- catchCurve(n~age,data=df,ages2use=-(1:2))
  scc2 <- summary(cc2)  
  expect_equal(cc1,cc2)
  expect_equal(scc1,scc2)
  
  cr1 <- chapmanRobson(n~age,data=df,ages2use=3:10)
  scr1 <- summary(cr1)
  cr2 <- chapmanRobson(n~age,data=df,ages2use=-(1:2))
  scr2 <- summary(cr2)
  expect_equal(cr1,cr2)
  expect_equal(scr1,scr2)
})


## Validate Results ----
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

