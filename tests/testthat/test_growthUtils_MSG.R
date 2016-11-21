context("Growth Utility Functions MESSAGES")
source("EXS_growthUtils.R")

test_that("gConvert() messages",{
  ## Bad type
  expect_error(gConvert(SMBassWB,in.pre="anu",out.type="anu"),"should be one of")
  ## Neither or both of in.var= or in.pre= 
  expect_error(gConvert(SMBassWB),"must use one of")
  expect_warning(gConvert(SMBassWB,in.pre="anu",in.var=c("anu1","anu2")),"Both 'in.var='")
  ## Variable does not exist
  expect_error(gConvert(SMBassWB,in.var=c("anu1","derek")),"Not all 'in.var=' variables found")
  expect_error(gConvert(SMBassWB,in.pre="derek"),"No variables start with")
  ## Bad variable numbers
  expect_error(gConvert(SMBassWB,in.var=c(-1,10)),"Non-positive column number given")
  expect_error(gConvert(SMBassWB,in.var=c(10,100)),"Column numbers exceed number of columns")
  
})

test_that("addRadCap() messages",{
  ## Neither or both of in.var= or in.pre= 
  expect_error(addRadCap(SMBassWB),"must use one of")
  expect_warning(addRadCap(SMBassWB,in.pre="anu",in.var=c("anu1","anu2")),"Both 'in.var='")
  ## Variable does not exist
  expect_error(addRadCap(SMBassWB,in.var=c("anu1","derek")),"Not all 'in.var='")
  expect_error(addRadCap(SMBassWB,in.pre="derek"),"No variables start with")
  ## Bad variable numbers
  expect_error(addRadCap(SMBassWB,in.var=c(-1,10)),"Non-positive column number given")
  expect_error(addRadCap(SMBassWB,in.var=c(10,100)),"Column numbers exceed number of columns")
})
