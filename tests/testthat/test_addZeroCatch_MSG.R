context("addZeroCatch() MESSAGES")
test_that("addZeroCatch() test messages",{
  source("EXS_addZeroCatch.R")
  expect_error(addZeroCatch(as.matrix(df1[,-3]),eventvar="net",specvar="species",zerovar="catch"),
                            "'df' must be a data.frame")
  expect_error(addZeroCatch(df1,eventvar="net",specvar="species"),"'zerovar' cannot be missing")
  expect_error(addZeroCatch(df1,eventvar="net",zerovar="catch"),"'specvar' cannot be missing")
  expect_error(addZeroCatch(df1,specvar="species",zerovar="catch"),"'eventvar' cannot be missing")
  expect_error(addZeroCatch(df1,eventvar="d",specvar="species",zerovar="catch"),"Not all 'eventvar'")
  expect_error(addZeroCatch(df1,eventvar="net",specvar="d",zerovar="catch"),"Not all 'specvar'")
  expect_error(addZeroCatch(df1,eventvar="net",specvar="species",zerovar="d"),"Not all 'zerovar'")
  expect_error(addZeroCatch(df5,eventvar="net",specvar=c("d","sex"),zerovar="catch"),
               "Not all 'specvar'")
  expect_error(addZeroCatch(df1,eventvar="net",specvar="species",zerovar=c("d","recaps")),
               "Not all 'zerovar'")
  expect_warning(addZeroCatch(df3,eventvar="net",specvar="species",zerovar="catch"),
                 "The original data.frame was returned")
})
  