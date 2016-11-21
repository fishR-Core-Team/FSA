context("lencat() MESSAGES")
source("EXS_lencat.R")

test_that("lencat() messages",{
  # wrong type of variable
  expect_error(lencat(~fish,data=df1),"must be numeric")
  # too many variables
  expect_error(lencat(~len+fish,data=df1),"only one variable")
  # bad width values
  expect_error(lencat(~len,data=df1,w=-1),"must be positive")
  expect_error(lencat(~len,data=df1,w=c(0.5,1)),"of length 1")
  # bad startcats
  expect_error(lencat(~len,data=df1,startcat=c(0.5,1)),"of length 1")
  # all NA values in the vector
  expect_warning(lencat(as.numeric(rep(NA,3))),"were missing")
  # use.names but no names given in breaks
  expect_warning(lencat(~len,data=df1,breaks=seq(0,10,1),use.names=TRUE),
                 "Used default labels")
  # data.frame with more than one variable
  expect_error(lencat(df1),"data.frame with one column")
})
