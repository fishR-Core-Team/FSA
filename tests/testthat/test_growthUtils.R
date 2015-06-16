context("Tests of Growth Utility Functions")

## Get some data
data(SMBassWB)
SMBassWB <- subset(SMBassWB,agecap<3,select=c(names(SMBassWB)[1:9],"radcap"))

test_that("gConvert() messages",{
  ## Bad type
  expect_error(gConvert(SMBassWB,in.pre="anu",type="anu"),"should be one of")
  ## Neither or both of in.var= or in.pre= 
  expect_error(gConvert(SMBassWB),"must use one of")
  expect_warning(gConvert(SMBassWB,in.pre="anu",in.var=c("anu1","anu2")),"Both 'in.var='")
  ## Variable does not exist
  expect_error(gConvert(SMBassWB,in.var=c("anu1","derek")),"At least one variable")
  expect_error(gConvert(SMBassWB,in.pre="derek"),"No variables start with")
})

test_that("gReshape() messages",{
  ## No in.pre= 
  expect_error(gReshape(SMBassWB),"You must have a prefix string")
  ## Variable does not exist
  expect_error(gReshape(SMBassWB,in.pre="derek"),"No variables start with")
  expect_error(gReshape(SMBassWB,in.pre="anu",last.plus="derek"),"'last.plus=' variable not found")
})

test_that("gReshape() output",{
  ## Change "radcap" to "ttlanu" to make sure that it does not get treated with in.pre=
  tmp <- SMBassWB
  names(tmp)[which(names(tmp)=="radcap")] <- "ttlanu"
  tmp <- gReshape(tmp,in.pre="anu")
  expect_true(any(names(tmp)=="ttlanu"))
})

test_that("addRadCap() messages",{
  ## Neither or both of in.var= or in.pre= 
  expect_error(addRadCap(SMBassWB),"must use one of")
  expect_warning(addRadCap(SMBassWB,in.pre="anu",in.var=c("anu1","anu2")),"Both 'in.var='")
  ## Variable does not exist
  expect_error(addRadCap(SMBassWB,in.var=c("anu1","derek")),"Not all 'in.var='")
  expect_error(addRadCap(SMBassWB,in.pre="derek"),"No variables start with")
})
