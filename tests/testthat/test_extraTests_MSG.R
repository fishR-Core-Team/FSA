context("extraTests() MESSAGES")
source("EXS_extraTests.R")

test_that("extraSS() and lrt() messages",{
  # models not of same class
  expect_error(extraSS(lm.0,com=nls.0),"same class")
  expect_error(lrt(lm.0,com=nls.0),"same class")
  # does not work with gls models
  expect_error(extraSS(gls.0,com=gls.1),"only works with")
  # com model is not more complex
  expect_warning(extraSS(lm.0,com=lm.0),"more complex")
  expect_warning(extraSS(lm.2,com=lm.0),"more complex")
  expect_warning(extraSS(lm.1,lm.2,com=lm.0),"more complex")
  expect_warning(extraSS(lm.0,lm.2,com=lm.1),"more complex")
  expect_warning(lrt(lm.0,com=lm.0),"more complex")
  expect_warning(lrt(lm.2,com=lm.0),"more complex")
  expect_warning(lrt(lm.1,lm.2,com=lm.0),"more complex")
  expect_warning(lrt(lm.0,lm.2,com=lm.1),"more complex")
  # names are bad
  expect_error(extraSS(lm.0,com=lm.1,sim.names=c("simple 1","simple 2"),com.name="Complex"),"simple models provided")
  expect_error(extraSS(lm.0,lm.1,com=lm.2,sim.names=c("simple 1","simple 2","simple 3"),com.name="Complex"),"simple models provided")
  expect_error(extraSS(lm.0,lm.1,com=lm.2,sim.names="simple 1",com.name="Complex"),"simple models provided")
  expect_warning(extraSS(lm.0,com=lm.2,sim.names="simple 1",com.name=c("Complex","Complex 2")),"more than one")
  expect_error(lrt(lm.0,com=lm.1,sim.names=c("simple 1","simple 2"),com.name="Complex"),"simple models provided")
  expect_error(lrt(lm.0,lm.1,com=lm.2,sim.names=c("simple 1","simple 2","simple 3"),com.name="Complex"),"simple models provided")
  expect_error(lrt(lm.0,lm.1,com=lm.2,sim.names="simple 1",com.name="Complex"),"simple models provided")
  expect_warning(lrt(lm.0,com=lm.2,sim.names="simple 1",com.name=c("Complex","Complex 2")),"more than one")
})
