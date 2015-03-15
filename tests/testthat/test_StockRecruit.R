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
  expect_error(srFuns(type="BevertonHolt",type=0))
  expect_error(srFuns(type="BevertonHolt",type=5))
  expect_error(srFuns(type="BevertonHolt",type=c(1,3)))
  expect_error(srFuns(type="Ricker",type=0))
  expect_error(srFuns(type="Ricker",type=4))
  expect_error(srFuns(type="Ricker",type=c(1,3)))
})
