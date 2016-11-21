context("extraTests() OUTPUT")
source("EXS_extraTests.R")

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
  expect_equal(tmp1,print.extraTest(tmp1))
  expect_equal(tmp2,print.extraTest(tmp2))
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
  expect_equal(tmp1,print.extraTest(tmp1))
  expect_equal(tmp2,print.extraTest(tmp2))
})
