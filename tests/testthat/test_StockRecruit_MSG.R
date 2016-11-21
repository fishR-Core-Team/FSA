context("Stock-Recruitment MESSAGES")

test_that("srFunShow() messages",{
  expect_error(srFunShow("Derek"),"should be one of")
  expect_error(srFunShow(type="BevertonHolt",param=0),"must be from")
  expect_error(srFunShow(type="BevertonHolt",param=5),"must be from")
  expect_error(srFunShow(type="Ricker",param=0),"must be from")
  expect_error(srFunShow(type="Ricker",param=4),"must be from")
})

test_that("srFuns() messages",{
  ## wrong type
  expect_error(srFuns(type="Derek"),"should be one")
  ## wrong parameterization choices
  expect_error(srFuns(type="BevertonHolt",param=0),"must be in")
  expect_error(srFuns(type="BevertonHolt",param=5),"must be in")
  expect_error(srFuns(type="BevertonHolt",param=c(1,3)),"Only one")
  expect_error(srFuns(type="Ricker",param=0),"must be in")
  expect_error(srFuns(type="Ricker",param=4),"must be in")
  expect_error(srFuns(type="Ricker",param=c(1,3)),"Only one")
  expect_warning(srFuns(type="Shepherd",param=2),"param=1")
  expect_warning(srFuns(type="Saila",param=2),"param=1")
  expect_warning(srFuns(type="independence",param=2),"param=1")
})

test_that("srStarts() messages",{
  data(CodNorwegian)
  CodNorwegian$fyear <- factor(CodNorwegian$year)
  ## Asked for a dynamicPlot, which now does not exist
  expect_warning(srStarts(recruits~stock,data=CodNorwegian,dynamicPlot=TRUE),
                 "functionality has been moved to")
  ## wrong type
  expect_error(srStarts(type="Derek"))
  ## wrong parameterization choices
  expect_error(srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",param=0),
               "'param' must be in")
  expect_error(srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",param=5),
               "'param' must be in")
  expect_error(srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",param=c(1,3)),
               "Only one 'param'")
  expect_error(srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=0),
               "'param' must be in")
  expect_error(srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=4),
               "'param' must be in")
  expect_error(srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=c(1,3)),
               "Only one 'param'")
  ## wrong variables
  expect_error(srStarts(~stock,data=CodNorwegian),"with both LHS and RHS")
  expect_error(srStarts(stock~1,data=CodNorwegian),"with both LHS and RHS")
  expect_error(srStarts(~stock+recruits,data=CodNorwegian),"only one LHS variable")
  expect_error(srStarts(stock+recruits~1,data=CodNorwegian),"with both LHS and RHS")
  expect_error(srStarts(stock~fyear,data=CodNorwegian),"RHS variable must be numeric")
  expect_error(srStarts(fyear~recruits,data=CodNorwegian),"LHS variable must be numeric")
  expect_error(srStarts(stock~recruits+fyear,data=CodNorwegian),
               "only one LHS and only one RHS")
})
