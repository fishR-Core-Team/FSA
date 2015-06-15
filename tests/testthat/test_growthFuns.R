context("Tests of Growth Functions")

test_that("xxxFuns() & Schnute() messages",{
  ## wrong models
  expect_error(vbFuns("Derek"),"should be one of")
  expect_error(GompertzFuns("Derek"),"should be one of")
  expect_error(logisticFuns("Derek"),"should be one of")
  expect_error(Schnute(case=0),"case")
  expect_error(Schnute(case=5),"case")
  expect_error(RichardsFuns(param=0),"param")
  expect_error(RichardsFuns(param=7),"param")
  ## bad choices for parameters in schnute()
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

test_that("xxxModels() messages",{
  ## wrong types
  expect_error(vbModels(type="Derek"),"should be one of")
  expect_error(GompertzModels(type="Derek"),"should be one of")
  expect_error(logisticModels(type="Derek"),"should be one of")
})  

test_that("vbFuns() output",{
  ## List all choices for vbFuns()
  tmp <- c("typical","BevertonHolt","original","vonBertalanffy","GQ","GallucciQuinn","Mooij","Weisberg","Schnute","Francis","Laslett","Polacheck","Fabens","Fabens2","Somers","Somers2","Wang","Wang2","Wang3")
  ## Do all choices return a function
  for (i in tmp) {
    expect_is(vbFuns(i),"function")
    expect_is(vbFuns(i,simple=TRUE),"function")
  }
  ## Do all choices return a message with the name of the function in it
  for (i in tmp) expect_message(vbFuns(i,msg=TRUE),i)
})

test_that("GompertzFuns() output",{
  ## List all choices for GompertzFuns()
  tmp <- c("Ricker1","Ricker2","Ricker3","QD1","QD2","QD3","KM","AFS","original","Troynikov1","Troynikov2")
  ## Do all choices return a function
  for (i in tmp) {
    expect_is(GompertzFuns(i),"function")
    expect_is(GompertzFuns(i,simple=TRUE),"function")
  }
  ## Do all choices return a message with the name of the function in it
  for (i in tmp) expect_message(GompertzFuns(i,msg=TRUE),i)
})

test_that("logisticFuns() output",{
  ## List all choices for logisticFuns()
  tmp <- c("CJ1","CJ2","Karkach","Haddon")
  ## Do all choices return a function
  for (i in tmp) {
    expect_is(logisticFuns(i),"function")
    expect_is(logisticFuns(i,simple=TRUE),"function")
  }
  ## Do all choices return a message with the name of the function in it
  for (i in tmp) expect_message(logisticFuns(i,msg=TRUE),i)
})

test_that("Schnute() output",{
  expect_is(Schnute(3,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1),"numeric")
  expect_is(Schnute(3,case=2,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1),"numeric")
  expect_is(Schnute(3,case=3,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1),"numeric")
  expect_is(Schnute(3,case=4,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1),"numeric")
})

test_that("RichardsFuns() output",{
  ## Do all choices return a function
  for (i in 1:4) {
    expect_is(RichardsFuns(i),"function")
    expect_is(RichardsFuns(i,simple=TRUE),"function")
  }
  ## Do all choices return a message with the name of the function in it
  for (i in 1:4) expect_message(RichardsFuns(i,msg=TRUE),paste0("Richards",i))
})