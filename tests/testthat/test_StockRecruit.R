context("Stock-Recruitment")

# ############################################################
# ============================================================
# Messaging
# ============================================================
# ############################################################

test_that("srFuns errors and warnings",{
  ## wrong type
  expect_error(srFuns(type="Derek"))
  ## wrong parameterization choices
  expect_error(srFuns(type="BevertonHolt",param=0))
  expect_error(srFuns(type="BevertonHolt",param=5))
  expect_error(srFuns(type="BevertonHolt",param=c(1,3)))
  expect_error(srFuns(type="Ricker",param=0))
  expect_error(srFuns(type="Ricker",param=4))
  expect_error(srFuns(type="Ricker",param=c(1,3)))
})

test_that("srFuns errors and warnings",{
  ## wrong type
  expect_error(srStarts(type="Derek"))
  ## wrong parameterization choices
  expect_error(srStarts(type="BevertonHolt",param=0))
  expect_error(srStarts(type="BevertonHolt",param=5))
  expect_error(srStarts(type="BevertonHolt",param=c(1,3)))
  expect_error(srStarts(type="Ricker",param=0))
  expect_error(srStarts(type="Ricker",param=4))
  expect_error(srStarts(type="Ricker",param=c(1,3)))
})
