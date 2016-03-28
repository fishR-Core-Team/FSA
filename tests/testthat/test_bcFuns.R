context("Tests of Back-Calculation Functions")

test_that("bcFuns() messages",{
  ## Wrong use of arguments
  expect_error(bcFuns(),"must be chosen")
  ## wrong models
  expect_error(bcFuns(0),"BCM number must be")
  expect_error(bcFuns(23),"BCM number must be")
  expect_error(bcFuns("Derek"),"must be one of")
})

test_that("bcFuns() output",{
  ## List all choices for bcFuns() (note that TVG is not included because it is not yet implemented)
  tmp <- c("DALE","FRALE","BI","LBI","BPH","LBPH","TVG","SPH","LSPH",
           "AE","AESPH","AEBPH","MONA","MONA-BPH","MONA-SPH","WAKU",
           "FRY","MF","ABI","FRY-BPH","ABPH","FRY-SPH","ASPH","QBPH",
           "QSPH","PBPH","PSPH","EBPH","ESPH")
  tmp <- tmp[-7]
  ## Do all choices return a function
  for (i in c(1:4,6:22)) expect_is(bcFuns(i),"function")
  for (i in tmp) expect_is(bcFuns(i),"function")
  ## Do all choices return a message with the name of the function in it
  for (i in tmp) expect_message(bcFuns(i,verbose=TRUE),i)
})
