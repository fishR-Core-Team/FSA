context("Growth Functions MESSAGES")
source("EXS_growthFuns.R")

test_that("growthFunShow() & Schnute() messages",{
  ## wrong models
  expect_error(growthFunShow("Derek",param="Original"),"should be one of")
  expect_error(growthFunShow("vonBertalanffy",param="Derek"),"should be one of")
  expect_error(growthFunShow("Gompertz",param="Derek"),"should be one of")
  expect_error(growthFunShow("Logistic",param="Derek"),"should be one of")
  expect_error(growthFunShow("vonBertalanffy",param=1),"must be a character string")
  expect_error(growthFunShow("Gompertz",param=1),"must be a character string")
  expect_error(growthFunShow("Logistic",param=1),"must be a character string")
  expect_error(growthFunShow("Richards",param="Derek"),"must be numeric when")
  expect_error(growthFunShow("Schnute",param="Derek"),"must be numeric when")
  expect_error(growthFunShow("Richards",param=0),"must be from")
  expect_error(growthFunShow("Richards",param=7),"must be from")
  expect_error(growthFunShow("Schnute",param=0),"must be from")
  expect_error(growthFunShow("Schnute",param=5),"must be from")

  ## bad choices for parameters in Schnute()
  # L1>L3
  expect_error(Schnute(3,t1=1,t3=15,L1=300,L3=30,a=0.3,b=0.5),"greater than")
  ## bad choices or givens for t1 and t3
  # t1==t3
  expect_error(Schnute(3,t1=1,t3=1,L1=30,L3=300,a=0.3,b=0.5),"cannot equal")
  # t1>t3
  expect_warning(Schnute(3,t1=15,t3=1,L1=30,L3=300,a=0.3,b=0.5),"greater than")
  # did not provide t1 and t3 when just a single value of t
  expect_error(Schnute(3,L1=30,L3=300,a=0.3,b=0.5),"Must provide")
  # t1 and t3 computed from t but came out to same value
  expect_error(Schnute(c(3,3,3),L1=30,L3=300,a=0.3,b=0.5),"cannot equal")
})
