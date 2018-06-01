context("ksTest() MESSAGES")

test_that("ksTest() messages",{
  ## Get data, make two more factor variables
  tmp <- rep(LETTERS[1:3],each=17)[-1]
  iris$fact1 <- factor(c(tmp,tmp,tmp))
  tmp <- c(rep(letters[1:6],each=8),"f","f")
  iris$fact2 <- factor(c(tmp,tmp,tmp))
  ## Problems with the formula
  expect_error(ksTest(~Species,data=iris),"must contain 1 response")
  expect_error(ksTest(Species~1,data=iris),"must contain 1 response")
  expect_error(ksTest(Species~Sepal.Width,data=iris),"LHS variable must be numeric")
  expect_error(ksTest(Species~fact1,data=iris),"LHS variable must be numeric")
  expect_error(ksTest(Sepal.Length~Sepal.Width,data=iris),"RHS Variable must be a factor")
  expect_error(ksTest(Sepal.Length~Species+Sepal.Width,data=iris),"must contain 1 response")
  expect_error(ksTest(Sepal.Length~Species+fact1+fact2,data=iris),"must contain 1 response")
  ## Problems with number of levels in factor variable
  expect_error(ksTest(Sepal.Length~Species,data=iris),"RHS variable has two levels")
})
