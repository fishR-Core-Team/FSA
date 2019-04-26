## List all parametrizations for the different functions ----
vbs <- c("Typical","Traditional","BevertonHolt",
         "Original","vonBertalanffy","GallucciQuinn","GQ","Ogle","Pauly",
         "Mooij","Weisberg","Schnute","Francis","Laslett","Polacheck",
         "Fabens","Fabens2","Somers","Somers2","Wang","Wang2","Wang3")
gomps <- c("Original","Ricker1","Ricker2","Ricker3",
           "QuinnDeriso1","QuinnDeriso2","QuinnDeriso3",
           "Troynikov1","Troynikov2")
logistics <- c("Karkach","Haddon","CampanaJones1","CampanaJones2")


## Test Messages ----
test_that("growthFunShow() & Schnute() messages",{
  ## wrong models
  expect_error(growthFunShow("Derek",param="Original"),
               "should be one of")
  expect_error(growthFunShow("vonBertalanffy",param="Derek"),
               "should be one of")
  expect_error(growthFunShow("Gompertz",param="Derek"),
               "should be one of")
  expect_error(growthFunShow("Logistic",param="Derek"),
               "should be one of")
  expect_error(growthFunShow("vonBertalanffy",param=1),
               "must be a character string")
  expect_error(growthFunShow("Gompertz",param=1),
               "must be a character string")
  expect_error(growthFunShow("Logistic",param=1),
               "must be a character string")
  expect_error(growthFunShow("Richards",param="Derek"),
               "must be numeric when")
  expect_error(growthFunShow("Schnute",param="Derek"),
               "must be numeric when")
  expect_error(growthFunShow("Richards",param=0),
               "must be from")
  expect_error(growthFunShow("Richards",param=7),
               "must be from")
  expect_error(growthFunShow("Schnute",param=0),
               "must be from")
  expect_error(growthFunShow("Schnute",param=5),
               "must be from")
  
  ## bad choices for parameters in Schnute()
  # L1>L3
  expect_error(Schnute(3,t1=1,t3=15,L1=300,L3=30,a=0.3,b=0.5),
               "greater than")
  ## bad choices or givens for t1 and t3
  # t1==t3
  expect_error(Schnute(3,t1=1,t3=1,L1=30,L3=300,a=0.3,b=0.5),
               "cannot equal")
  # t1>t3
  expect_warning(Schnute(3,t1=15,t3=1,L1=30,L3=300,a=0.3,b=0.5),
                 "greater than")
  # did not provide t1 and t3 when just a single value of t
  expect_error(Schnute(3,L1=30,L3=300,a=0.3,b=0.5),
               "Must provide")
  # t1 and t3 computed from t but came out to same value
  expect_error(Schnute(c(3,3,3),L1=30,L3=300,a=0.3,b=0.5),
               "cannot equal")
})


## Test Output Types ----
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


## Validate Results ----

