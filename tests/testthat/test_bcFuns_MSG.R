context("bcFuns() MESSAGES")

test_that("bcFuns() messages",{
  expect_error(bcFuns(),"must be chosen")
  expect_error(bcFuns(0),"BCM number must be")
  expect_error(bcFuns(23),"BCM number must be")
  expect_error(bcFuns("Derek"),"must be one of")
  expect_error(bcFuns("TVG"),"not yet implemented")
  expect_error(bcFuns(5),"not yet implemented")

  ## List all choices for bcFuns() ( TVG is not included because it is not yet implemented)
  tmp <- c("DALE","FRALE","BI","LBI","BPH","LBPH","TVG","SPH","LSPH",
           "AE","AESPH","AEBPH","MONA","MONA-BPH","MONA-SPH","WAKU",
           "FRY","MF","ABI","FRY-BPH","ABPH","FRY-SPH","ASPH","QBPH",
           "QSPH","PBPH","PSPH","EBPH","ESPH")
  tmp <- tmp[-7]
  ## Do all choices return a message with the name of the function in it
  for (i in tmp) expect_message(bcFuns(i,verbose=TRUE),i)
})
