context("expandLenFreq() MESSAGES")

test_that("expandLenFreq() messages",{
  ## dummy bad data
  x <- 1:26
  df <- data.frame(x,letters)
  ## check obvious errors
  expect_error(expandLenFreq(df,1,17),"must be a vector")
  expect_error(expandLenFreq(letters,1,17),"must be numeric")
  expect_error(expandLenFreq(x,-1,17),"must be positive")
  expect_error(expandLenFreq(x,1,17,startcat=-2),"must be positive")
  expect_error(expandLenFreq(x,1,total=17),"Total number to expand")
  ## do you get a message
  expect_output(expandLenFreq(x,1,17),"Length Frequency Expansion using")
})
