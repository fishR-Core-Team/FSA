context("Growth Functions OUTPUT")
source("EXS_growthFuns.R")

test_that("growthFunShow() results",{
  for (i in vbs) {
    expect_is(growthFunShow("vonBertalanffy",param=i),"expression")
    expect_is(growthFunShow("vonBertalanffy",param=i,plot=TRUE),"expression")
  }
  for (i in gomps) {
    expect_is(growthFunShow("Gompertz",param=i),"expression")
    expect_is(growthFunShow("Gompertz",param=i,plot=TRUE),"expression")
  }
  for (i in logistics) {
    expect_is(growthFunShow("Logistic",param=i),"expression")
    expect_is(growthFunShow("Logistic",param=i,plot=TRUE),"expression")
  }
  for (i in 1:6) {
    expect_is(growthFunShow("Richards",param=i),"expression")
    expect_is(growthFunShow("Richards",param=i,plot=TRUE),"expression")
  }
  for (i in 1:4) {
    expect_is(growthFunShow("Schnute",param=i),"expression")
    expect_is(growthFunShow("Schnute",param=i,plot=TRUE),"expression")
  }
})

test_that("vbFuns() output",{
  ## Do all choices return a function
  for (i in vbs) {
    expect_is(vbFuns(i),"function")
    expect_is(vbFuns(i,simple=TRUE),"function")
  }
  ## Do all choices return a message with the name of the function in it
  for (i in vbs) expect_message(vbFuns(i,msg=TRUE),i)
})

test_that("vbFuns() arguments are in same order as vbStarts() list",{
  ## Make sure the two names match ... delete the "t", "t1", and "t3"
  ##   arguments from vbFuns() result as these are variable names that
  ##   would not be returned by vbStarts()
  ## Remove some VBGFs for which starting values don't exist
  tmp <- vbs[-which(vbs %in% c("Laslett","Polacheck","Fabens","Fabens2","Wang","Wang2","Wang3","Ogle"))]
  for (i in tmp) {
    fnms <- names(formals(vbFuns(i)))
    fnms <- fnms[-which(fnms %in% c("t","t1","t3"))]
    snms <- names(vbStarts(tl~age,data=SpotVA1,type=i))
    expect_equal(fnms,snms)
  }
})

test_that("GompertzFuns() output",{
  ## Do all choices return a function
  for (i in gomps) {
    expect_is(GompertzFuns(i),"function")
    expect_is(GompertzFuns(i,simple=TRUE),"function")
  }
  ## Do all choices return a message with the name of the function in it
  for (i in gomps) expect_message(GompertzFuns(i,msg=TRUE),i)
})

test_that("logisticFuns() output",{
  ## Do all choices return a function
  for (i in logistics) {
    expect_is(logisticFuns(i),"function")
    expect_is(logisticFuns(i,simple=TRUE),"function")
  }
  ## Do all choices return a message with the name of the function in it
  for (i in logistics) expect_message(logisticFuns(i,msg=TRUE),i)
})

test_that("RichardsFuns() output",{
  ## Do all choices return a function
  for (i in 1:6) {
    expect_is(RichardsFuns(i),"function")
    expect_is(RichardsFuns(i,simple=TRUE),"function")
  }
  ## Do all choices return a message with the name of the function in it
  for (i in 1:6) expect_message(RichardsFuns(i,msg=TRUE),paste0("Richards",i))
})

test_that("Schnute() output",{
  expect_is(Schnute(3,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1),"numeric")
  expect_is(Schnute(3,case=2,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1),"numeric")
  expect_is(Schnute(3,case=3,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1),"numeric")
  expect_is(Schnute(3,case=4,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1),"numeric")
})
