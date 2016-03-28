context("Capture History Functions")

test_that("capHistSum() errors and warnings",{
  d <- data.frame(id=1:5,sex=c("m","m","f","m","f"),
                  first=c(1,1,1,0,0),
                  second=c(0,0,1,1,1))
  expect_error(capHistSum(d,cols2use=1,cols2ignore=4),"Cannot use both")
  expect_error(suppresswarnings(capHistSum(d,cols2ignore=1:5),"undefined columns selected"))
})  

test_that("capHistSum() results",{
  d <- data.frame(id=1:5,sex=c("m","m","f","m","f"),
                  first=c(1,1,1,0,0),
                  second=c(0,0,1,1,1),
                  third=c(1,0,0,0,1))
  ## Simple two-sample test data
  ch <- capHistSum(d,cols2use=3:4)
  expect_is(ch,"CapHist")
  #expect_type(ch,"list")
  expect_equivalent(ch$caphist,as.table(c(2,2,1)))
  expect_equivalent(names(ch$caphist),c("01","10","11"))
  expect_equal(ch$sum,data.frame(M=3,n=3,m=1))
  
  ## Simple three-sample test data
  ch <- capHistSum(d,cols2use=3:5)
  expect_is(ch,"CapHist")
  #expect_type(ch,"list")
  expect_equivalent(ch$caphist,as.table(c(1,1,1,1,1)))
  expect_equivalent(names(ch$caphist),c("010","011","100","101","110"))
  tmp <- data.frame(n=c(3,3,2),m=c(0,1,2),R=c(3,3,0),M=c(0,3,5),
                    u=c(3,3,0),v=c(1,3,2),f=c(2,3,0))
  expect_equal(ch$sum,tmp)
  expect_is(ch$methodB.top,"matrix")
  #expect_type(ch$methodB.top,"double")
  expect_identical(ch$methodB.top[upper.tri(ch$methodB.top)],c(1,1,1))
  expect_identical(ch$methodB.top[!upper.tri(ch$methodB.top)],as.numeric(rep(NA,6)))
  expect_equal(ch$sum,tmp)
  expect_is(ch$methodB.bot,"matrix")
  #expect_type(ch$methodB.bot,"double")
  expect_identical(rownames(ch$methodB.bot),c("m","u","n","R"))
  tmp <- matrix(c(0,3,3,3,1,2,3,3,2,0,2,0),nrow=4)
  expect_equivalent(ch$methodB.bot,tmp)
  expect_is(ch$m.array,"matrix")
  #expect_type(ch$m.array,"double")
  expect_identical(colnames(ch$m.array),c("ni","c2","c3","not recapt"))
  tmp <- matrix(c(3,3,2,1,NA,NA,1,1,NA,1,2,2),nrow=3)
  expect_equivalent(ch$m.array,tmp)
})  


test_that("capHistConvert() errors and warnings",{
  ## A small example of 'event' format
  ( ex1 <- data.frame(fish=c(17,18,21,17,21,18,19,20),
                      yr=c(1987,1987,1987,1988,1988,1989,1989,1990)) )
  # convert to 'individual' format
  ( ex1.E2I <- capHistConvert(ex1,id="fish",in.type="event") )
  # convert to 'frequency' format
  ( ex1.E2F <- capHistConvert(ex1,id="fish",in.type="event",out.type="frequency") )
  # convert to 'MARK' format
  ( ex1.E2M <- capHistConvert(ex1,id="fish",in.type="event",out.type="MARK") )
  # convert to 'RMark' format
  ( ex1.E2R <- capHistConvert(ex1,id="fish",in.type="event",out.type="RMark") )
  
  ## wrong types in in.type or out.type
  expect_error(capHistConvert(ex1,id="fish",in.type="derek"))
  expect_error(capHistConvert(ex1,id="fish",out.type="derek"))
  expect_error(capHistConvert(ex1,id="fish",in.type="event",out.type="event"))

  ## bad var.lbls.pre
  # start with a number
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",var.lbls.pre="7event"))
  # too many (must be just one)
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",var.lbls.pre=LETTERS[1:2]))
  ## bad var.lbls ... too few (too many is adjusted without warning)
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",var.lbls=LETTERS[1:2]))
  
  ## Bad (or semi-bad) arguments for specific conversions
  # no "id" variable when converting from 'event' to 'individual' format
  expect_error(capHistConvert(ex1,in.type="event"))
  # no "freq" variable when converting from 'frequency', 'RMark' or 'MARK' to 'individual' formats
  expect_warning(capHistConvert(ex1.E2F,in.type="frequency"))
  # should give a warning as well
  expect_error(suppressWarnings(capHistConvert(ex1.E2R,in.type="RMark")))
  expect_error(capHistConvert(ex1.E2R,in.type="RMark",id="fish",freq="fish"))
  # should give a warning as well
  expect_error(suppressWarnings(capHistConvert(ex1.E2M,in.type="MARK")))
})