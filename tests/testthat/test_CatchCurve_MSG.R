context("Catch Curve and Chapman-Robson MESSAGES")
source("EXS_CatchCurve.R")

test_that("catchCurve() messages",{
  # bad variables
  expect_error(catchCurve(d$age,d$fact),"must be numeric")
  expect_error(catchCurve(d$fact,d$age),"must be numeric")
  expect_error(catchCurve(age~fact,data=d),"must be numeric")
  expect_error(catchCurve(fact~age,data=d),"must be numeric")
  # bad formulas
  expect_error(catchCurve(catch~age+fact,data=d),"only one RHS variable")
  expect_error(catchCurve(catch+age~fact,data=d),"more than one variable on the LHS")
  # bad numbers of individuals
  expect_error(catchCurve(catch~age[-1],data=d),"variable lengths differ")
  expect_error(catchCurve(catch[-1]~age,data=d),"variable lengths differ")
  expect_error(catchCurve(d$age,d$catch[-1]),"have different lengths")
  expect_error(catchCurve(d$age[-1],d$catch),"have different lengths")
  # too few data
  expect_error(catchCurve(d$age[1],d$catch[1]),"Fewer than 2 data points")
  expect_error(catchCurve(d$age,d$catch,ages2use=3),"Fewer than 2 data points")
  # bad ages2use
  expect_warning(catchCurve(catch~age,data=d,ages2use=c(2:9)),"not in observed ages")
  expect_error(catchCurve(catch~age,data=d,ages2use=c(-1,2:6)),"all positive or negative")
  # bad args in coef
  expect_error(coef(cc,parm="derek"),"should be one of")
  # bad args in confint
  expect_error(confint(cc,parm="derek"),"should be one of")
  expect_error(confint(cc,conf.level=0),"must be between 0 and 1")
  expect_error(confint(cc,conf.level=1),"must be between 0 and 1")
  # bad args in summary
  expect_error(summary(cc,parm="derek"),"should be one of")
  
  # How does catchCurve() handle negative weights
  d <- data.frame(catch=c(10,5,3,1,1,1),age=1:6)
  expect_warning(catchCurve(catch~age,data=d,weighted=TRUE),
                 "Some weights were non-positive")
})

test_that("chapmanRobson errors and warnings",{
  # bad zmethod
  expect_error(chapmanRobson(catch~age,data=d,ages2use=2:6,zmethod="Derek"),"should be one of")
  # bad variables
  expect_error(chapmanRobson(d$age,d$fact),"must be numeric")
  expect_error(chapmanRobson(d$fact,d$age),"must be numeric")
  expect_error(chapmanRobson(age~fact,data=d),"must be numeric")
  expect_error(chapmanRobson(fact~age,data=d),"must be numeric")
  # bad formulas
  expect_error(chapmanRobson(catch~age+fact,data=d),"only one RHS variable")
  expect_error(chapmanRobson(catch+age~fact,data=d),"more than one variable on the LHS")
  # bad numbers of individuals
  expect_error(chapmanRobson(catch~age[-1],data=d),"variable lengths differ")
  expect_error(chapmanRobson(catch[-1]~age,data=d),"variable lengths differ")
  expect_error(chapmanRobson(d$age,d$catch[-1]),"have different lengths")
  expect_error(chapmanRobson(d$age[-1],d$catch),"have different lengths")
  # too few data
  expect_error(chapmanRobson(d$age[1],d$catch[1]),"Fewer than 2 data points")
  expect_error(chapmanRobson(d$age,d$catch,ages2use=3),"Fewer than 2 data points")
  # bad ages2use
  expect_warning(chapmanRobson(catch~age,data=d,ages2use=c(2:9)),"not in observed ages")
  expect_error(chapmanRobson(catch~age,data=d,ages2use=c(-1,2:6)),"all positive or negative")
  # bad choice of axis types
  expect_error(plot(cr,axis.age="derek"),"should be one of")
  # bad args in summary and coef
  expect_error(summary(cr,parm="derek"),"should be one of")
  expect_error(coef(cr,parm="derek"),"should be one of")
  # bad args in confint
  expect_error(confint(cr,parm="derek"),"should be one of")
  expect_error(confint(cr,conf.level=0),"must be between 0 and 1")
  expect_error(confint(cr,conf.level=1),"must be between 0 and 1")
})
