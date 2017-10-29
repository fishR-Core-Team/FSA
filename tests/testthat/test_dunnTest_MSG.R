context("dunnTest() MESSAGES")
source("EXS_dunnTest.R")

test_that("dunnTest() error and warning messages",{
  expect_error(dunnTest(ponds$pond,ponds$pH),"to a factor")
  expect_warning(dunnTest(ponds$pH,ponds$pond),"to a factor")
  expect_warning(dunnTest(ponds$pH,ponds$cpond),"to a factor")
  expect_error(dunnTest(pond~pH,data=ponds),"must be a factor")
  expect_warning(dunnTest(pH~pond,data=ponds),"to a factor")
  expect_warning(dunnTest(pH~cpond,data=ponds),"to a factor")
  expect_error(dunnTest(pH~pond+cpond,data=ponds),"only one RHS variable")
  expect_error(dunnTest(pH+cpond~pond,data=ponds))
  expect_warning(dunnTest(pH~fpond,data=ponds2),"Some rows deleted from")
})
