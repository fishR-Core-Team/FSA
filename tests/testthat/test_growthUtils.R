context("Tests of Growth Utility Functions")

## Get some real data
data(SMBassWB)
SMBassWB <- subset(SMBassWB,agecap<3,select=c(names(SMBassWB)[1:9],"radcap"))
## Make some fake (but easy) data
bctmp <- data.frame(id=1:3,agecap=1:3,lencap=c(11,12,13),radcap=1:3,
                    anu1=c(1,1,1),anu2=c(NA,2,2),anu3=c(NA,NA,3))
## Make some fake (but easy) data (but with "plus growth")
bctmp2 <- data.frame(id=1:3,agecap=1:3,lencap=c(11,12,13),radcap=1:3+0.1,
                     anu1=c(1,1,1),anu2=c(1.1,2,2),anu3=c(NA,2.1,3),anu4=c(NA,NA,3.1))

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

test_that("gConvert() output",{
  ## Actually constructs increments from radii ... no plus-growth
  tmp <- gConvert(bctmp,in.pre="anu")
  expect_equal(names(tmp),c(names(bctmp)[1:4],paste0("inc",1:3)))
  expect_equivalent(as.numeric(tmp[1,]),as.numeric(cbind(bctmp[1,1:4],1,NA,NA)))
  expect_equivalent(as.numeric(tmp[2,]),as.numeric(cbind(bctmp[2,1:4],1,1,NA)))
  expect_equivalent(as.numeric(tmp[3,]),as.numeric(cbind(bctmp[3,1:4],1,1,1)))
  ## Actually re-constructs radii from increments ... no plus-growth
  tmp <- gConvert(tmp,in.pre="inc",out.type="rad",out.pre="anu")
  expect_equal(bctmp,tmp)
  ## Actually constructs increments from radii ... with plus-growth
  tmp <- gConvert(bctmp2,in.pre="anu")
  expect_equal(names(tmp),c(names(bctmp2)[1:4],paste0("inc",1:4)))
  expect_equivalent(as.numeric(tmp[1,]),as.numeric(cbind(bctmp2[1,1:4],1,0.1,NA,NA)))
  expect_equivalent(as.numeric(tmp[2,]),as.numeric(cbind(bctmp2[2,1:4],1,1,0.1,NA)))
  expect_equivalent(as.numeric(tmp[3,]),as.numeric(cbind(bctmp2[3,1:4],1,1,1,0.1)))
  ## Actually re-constructs radii from increments ... no plus-growth
  tmp <- gConvert(tmp,in.pre="inc",out.type="rad",out.pre="anu")
  expect_equal(bctmp2,tmp)
})

test_that("addRadCap() output",{
  ## Convert radii to increments ... no plus-growth
  tmp <- gConvert(bctmp,in.pre="anu")
  tmp <- addRadCap(tmp,in.pre="inc",var.name="newRadCap")
  expect_equal(tmp$radcap,tmp$newRadCap)
  ## Convert radii to increments ... plus-growth
  tmp <- gConvert(bctmp2,in.pre="anu")
  tmp <- addRadCap(tmp,in.pre="inc",var.name="newRadCap")
  expect_equal(tmp$radcap,tmp$newRadCap)
})  