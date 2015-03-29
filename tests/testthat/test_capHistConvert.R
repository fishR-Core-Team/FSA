context("capHistConvert")

# ############################################################
# ============================================================
# Messaging
# ============================================================
# ############################################################
test_that("capHistConvert() errors and warnings",{
  ## A small example of 'event' format
  ( ex1 <- data.frame(fish=c(17,18,21,17,21,18,19,20),yr=c(1987,1987,1987,1988,1988,1989,1989,1990)) )
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
  expect_error(suppressWarnings(capHistConvert(ex1.E2R,in.type="RMark")))   # should give a warning as well
  expect_error(capHistConvert(ex1.E2R,in.type="RMark",id="fish",freq="fish"))
  expect_error(suppressWarnings(capHistConvert(ex1.E2M,in.type="MARK")))    # should give a warning as well
  
})