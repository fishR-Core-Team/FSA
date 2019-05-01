## Example data for testing ----
df <- data.frame(x=c(1,2,3,4,5,6,7,8,9,10),
                 y=c(4,6,5,7,9,8,7,12,16,22),
                 z=as.factor(rep(c("A","B"),each=5)),
                 w=as.factor(rep(c("A","B"),times=5)))
df$x2 <- df$x^2

## Some fits
lm.0 <- lm(y~1,data=df)
lm.1 <- lm(y~x,data=df)
lm.2 <- lm(y~x+x2,data=df)
lm.2b <- lm(y~x*z,data=df)
lm.1a <- lm(y~w,data=df)
lm.2c <- lm(y~w*z,data=df)

# note that the rep() in the first model is needed to eliminate a warning
# that comes from nls().
nls.0 <- nls(y~rep(c,length(df$y)),data=df,start=list(c=10))
nls.1 <- nls(y~a*x+c,data=df,start=list(a=1,c=1))
nls.2 <- nls(y~b*x2+a*x+c,data=df,start=list(a=-1,b=0.3,c=10))

if (suppressMessages(require(nlme))) {
  gls.0 <- gls(y~1,data=df,method="ML")
  gls.1 <- gls(y~x,data=df,method="ML")
  gls.2 <- gls(y~x+x2,data=df,method="ML")  
}

suppressMessages(library(lmtest))


## Test Messages ----
test_that("extraSS() and lrt() messages",{
  # models not of same class
  expect_error(extraSS(lm.0,com=nls.0),
               "same class")
  expect_error(lrt(lm.0,com=nls.0),
               "same class")
  # does not work with gls models
  expect_error(extraSS(gls.0,com=gls.1),
               "only works with")
  # com model is not more complex
  expect_warning(extraSS(lm.0,com=lm.0),
                 "more complex")
  expect_warning(extraSS(lm.2,com=lm.0),
                 "more complex")
  expect_warning(extraSS(lm.1,lm.2,com=lm.0),
                 "more complex")
  expect_warning(extraSS(lm.0,lm.2,com=lm.1),
                 "more complex")
  expect_warning(lrt(lm.0,com=lm.0),
                 "more complex")
  expect_warning(lrt(lm.2,com=lm.0),
                 "more complex")
  expect_warning(lrt(lm.1,lm.2,com=lm.0),
                 "more complex")
  expect_warning(lrt(lm.0,lm.2,com=lm.1),
                 "more complex")
  # names are bad
  expect_error(extraSS(lm.0,com=lm.1,sim.names=c("simple 1","simple 2"),
                       com.name="Complex"),
               "simple models provided")
  expect_error(extraSS(lm.0,lm.1,com=lm.2,
                       sim.names=c("simple 1","simple 2","simple 3"),
                       com.name="Complex"),
               "simple models provided")
  expect_error(extraSS(lm.0,lm.1,com=lm.2,sim.names="simple 1",
                       com.name="Complex"),
               "simple models provided")
  expect_warning(extraSS(lm.0,com=lm.2,sim.names="simple 1",
                         com.name=c("Complex","Complex 2")),
                 "more than one")
  expect_error(lrt(lm.0,com=lm.1,sim.names=c("simple 1","simple 2"),
                   com.name="Complex"),
               "simple models provided")
  expect_error(lrt(lm.0,lm.1,com=lm.2,
                   sim.names=c("simple 1","simple 2","simple 3"),
                   com.name="Complex"),
               "simple models provided")
  expect_error(lrt(lm.0,lm.1,com=lm.2,sim.names="simple 1",
                   com.name="Complex"),
               "simple models provided")
  expect_warning(lrt(lm.0,com=lm.2,sim.names="simple 1",
                     com.name=c("Complex","Complex 2")),
                 "more than one")
})


## Test Output Types ----
test_that("extraSS() and lrt() output",{
  # extraSS() returns a matrix of class extraTest
  tmp1 <- extraSS(lm.0,com=lm.1)
  expect_is(tmp1,"extraTest")
  expect_true(is.matrix(tmp1))
  expect_equal(nrow(tmp1),1)
  tmp2 <- extraSS(lm.0,lm.1,com=lm.2)
  expect_is(tmp2,"extraTest")
  expect_true(is.matrix(tmp2))
  expect_equal(nrow(tmp2),2)
  # print() returns x
  junk <- capture.output( tmp <- FSA:::print.extraTest(tmp1))
  expect_equal(tmp1,tmp)
  junk <- capture.output( tmp <- FSA:::print.extraTest(tmp2))
  expect_equal(tmp2,tmp)
  # lrt() returns a matrix of class extraTest
  tmp1 <- lrt(lm.0,com=lm.1)
  expect_is(tmp1,"extraTest")
  expect_true(is.matrix(tmp1))
  expect_equal(nrow(tmp1),1)
  tmp2 <- lrt(lm.0,lm.1,com=lm.2)
  expect_is(tmp2,"extraTest")
  expect_true(is.matrix(tmp2))
  expect_equal(nrow(tmp2),2)
  # print() returns x
  junk <- capture.output( tmp <- FSA:::print.extraTest(tmp1))
  expect_equal(tmp1,tmp)
  junk <- capture.output( tmp <- FSA:::print.extraTest(tmp2))
  expect_equal(tmp2,tmp)
})


## Validate Results ----
test_that("extraSS() computations",{
  ## Two model lm comparisons
  tmp1 <- extraSS(lm.0,com=lm.1)
  tmp2 <- anova(lm.0,lm.1)
  expect_equivalent(tmp1[1,"F"],tmp2[2,"F"])
  expect_equivalent(tmp1[1,"Df"],tmp2[2,"Df"])
  expect_equivalent(tmp1[1,"SS"],tmp2[2,"Sum of Sq"])
  ## Three model lm comparisons (only can compare to last)
  tmp1 <- extraSS(lm.0,lm.1,com=lm.2)
  tmp2 <- anova(lm.0,lm.1,lm.2)
  expect_equivalent(tmp1[2,"F"],tmp2[3,"F"])
  expect_equivalent(tmp1[2,"Df"],tmp2[3,"Df"])
  expect_equivalent(tmp1[2,"SS"],tmp2[3,"Sum of Sq"])
  ## Two model nls comparisons
  tmp1 <- extraSS(nls.0,com=nls.1)
  tmp2 <- anova(nls.0,nls.1)
  expect_equivalent(tmp1[1,"F"],tmp2[2,"F value"])
  expect_equivalent(tmp1[1,"Df"],tmp2[2,"Df"])
  expect_equivalent(tmp1[1,"SS"],tmp2[2,"Sum Sq"])
  ## Three model nls comparisons (only can compare to last)
  tmp1 <- extraSS(nls.0,nls.1,com=nls.2)
  tmp2 <- anova(nls.0,nls.1,nls.2)
  expect_equivalent(tmp1[2,"F"],tmp2[3,"F value"])
  expect_equivalent(tmp1[2,"Df"],tmp2[3,"Df"])
  expect_equivalent(tmp1[2,"SS"],tmp2[3,"Sum Sq"])
})

test_that("lrt() computations",{
  require(lmtest)
  ## Two model lm comparisons
  tmp1 <- lrt(lm.0,com=lm.1)
  tmp2 <- lrtest(lm.0,lm.1)
  expect_equivalent(tmp1[1,"Chisq"],tmp2[2,"Chisq"])
  expect_equivalent(tmp1[1,"Df"],tmp2[2,"Df"])
  expect_equivalent(tmp1[1,"logLikO"],tmp2[1,"LogLik"])
  expect_equivalent(tmp1[1,"logLikA"],tmp2[2,"LogLik"])
  ## Three model lm comparisons (only can compare to last)
  tmp1 <- lrt(lm.0,lm.1,com=lm.2)
  tmp2 <- lrtest(lm.0,lm.1,lm.2)
  expect_equivalent(tmp1[2,"Chisq"],tmp2[3,"Chisq"])
  expect_equivalent(tmp1[2,"Df"],tmp2[3,"Df"])
  expect_equivalent(tmp1[2,"logLikO"],tmp2[2,"LogLik"])
  expect_equivalent(tmp1[2,"logLikA"],tmp2[3,"LogLik"])
  ## Two model nls comparisons
  tmp1 <- lrt(nls.0,com=nls.1)
  tmp2 <- lrtest(nls.0,nls.1)
  expect_equivalent(tmp1[1,"Chisq"],tmp2[2,"Chisq"])
  expect_equivalent(tmp1[1,"Df"],tmp2[2,"Df"])
  expect_equivalent(tmp1[1,"logLikO"],tmp2[1,"LogLik"])
  expect_equivalent(tmp1[1,"logLikA"],tmp2[2,"LogLik"])
  ## Three model nls comparisons (only can compare to last)
  tmp1 <- lrt(nls.0,nls.1,com=nls.2)
  tmp2 <- lrtest(nls.0,nls.1,nls.2)
  expect_equivalent(tmp1[2,"Chisq"],tmp2[3,"Chisq"])
  expect_equivalent(tmp1[2,"Df"],tmp2[3,"Df"])
  expect_equivalent(tmp1[2,"logLikO"],tmp2[2,"LogLik"])
  expect_equivalent(tmp1[2,"logLikA"],tmp2[3,"LogLik"])
})

