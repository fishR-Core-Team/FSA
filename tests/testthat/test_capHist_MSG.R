context("Capture History Functions MESSAGES")
source("EXS_capHist.R")

test_that("capHistSum() messages",{
  expect_error(capHistSum(d,cols2use=1,cols2ignore=4),"Cannot use both")
  expect_error(suppressWarnings(capHistSum(d,cols2ignore=1:5),
                                "undefined columns selected"))
})  

test_that("capHistConvert() messages",{
  ## wrong types in in.type or out.type
  expect_error(capHistConvert(ex1,id="fish",in.type="derek"),"should be one of")
  expect_error(capHistConvert(ex1,id="fish",out.type="derek"),"should be one of")
  expect_error(capHistConvert(ex1,id="fish",in.type="event",out.type="event"),
               "cannot be the same")
  ## bad var.lbls.pre
  # start with a number
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",var.lbls.pre="7event"),
                 "cannot begin with a number")
  # too many (must be just one)
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",
                                var.lbls.pre=LETTERS[1:2]),"contains more than one prefix")
  ## bad var.lbls ... too few (too many is adjusted without warning)
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",
                                var.lbls=LETTERS[1:2]),"Too few labels")
  
  ## Bad (or semi-bad) arguments for specific conversions
  # no "id" variable when converting from 'event' to 'individual' format
  expect_error(capHistConvert(ex1,in.type="event"),"No variable with unique fish")
  # no "freq" var when from 'frequency', 'RMark' or 'MARK' to 'individual' formats
  expect_warning(capHistConvert(ex1.E2F,in.type="frequency"),"No 'freq' given")
  # should give a warning as well
  expect_error(suppressWarnings(capHistConvert(ex1.E2R,in.type="RMark")),
               "invalid 'times' argument")
  expect_error(capHistConvert(ex1.E2R,in.type="RMark",id="fish",freq="fish"))
  # should give a warning as well
  expect_error(suppressWarnings(capHistConvert(ex1.E2M,in.type="MARK")),
               "invalid 'times' argument")
})