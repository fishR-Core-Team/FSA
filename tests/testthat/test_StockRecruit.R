context("Stock-Recruitment")

# ############################################################
# ============================================================
# Messaging
# ============================================================
# ############################################################

test_that("srFuns() errors and warnings",{
  ## wrong type
  expect_error(srFuns(type="Derek"),"should be one")
  ## wrong parameterization choices
  expect_error(srFuns(type="BevertonHolt",param=0),"must be in")
  expect_error(srFuns(type="BevertonHolt",param=5),"must be in")
  expect_error(srFuns(type="BevertonHolt",param=c(1,3)),"Only one")
  expect_error(srFuns(type="Ricker",param=0),"must be in")
  expect_error(srFuns(type="Ricker",param=4),"must be in")
  expect_error(srFuns(type="Ricker",param=c(1,3)),"Only one")
  expect_warning(srFuns(type="Shepherd",param=2),"param=1")
  expect_warning(srFuns(type="Saila",param=2),"param=1")
  expect_warning(srFuns(type="independence",param=2),"param=1")
})

test_that("srStarts() errors and warnings",{
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

test_that("srFuns() output",{
  ## Do all choices return a function
  expect_is(srFuns("BevertonHolt",param=1),"function")
  expect_is(srFuns("BevertonHolt",param=2),"function")
  expect_is(srFuns("BevertonHolt",param=3),"function")
  expect_is(srFuns("BevertonHolt",param=4),"function")
  expect_is(srFuns("Ricker",param=1),"function")
  expect_is(srFuns("Ricker",param=2),"function")
  expect_is(srFuns("Ricker",param=3),"function")
  expect_is(srFuns("Shepherd"),"function")
  expect_is(srFuns("Saila"),"function")
  expect_is(srFuns("independence"),"function")

  expect_is(srFuns("BevertonHolt",param=1,simple=TRUE),"function")
  expect_is(srFuns("BevertonHolt",param=2,simple=TRUE),"function")
  expect_is(srFuns("BevertonHolt",param=3,simple=TRUE),"function")
  expect_is(srFuns("BevertonHolt",param=4,simple=TRUE),"function")
  expect_is(srFuns("Ricker",param=1,simple=TRUE),"function")
  expect_is(srFuns("Ricker",param=2,simple=TRUE),"function")
  expect_is(srFuns("Ricker",param=3,simple=TRUE),"function")
  expect_is(srFuns("Shepherd",simple=TRUE),"function")
  expect_is(srFuns("Saila",simple=TRUE),"function")
  expect_is(srFuns("independence",simple=TRUE),"function")
  
  ## Do all choices return a message with the name of the function in it
  expect_message(srFuns("BevertonHolt",param=1,msg=TRUE),"first parameterization of the 'Beverton-Holt'")
  expect_message(srFuns("BevertonHolt",param=2,msg=TRUE),"second parameterization of the 'Beverton-Holt'")
  expect_message(srFuns("BevertonHolt",param=3,msg=TRUE),"third parameterization of the 'Beverton-Holt'")
  expect_message(srFuns("BevertonHolt",param=4,msg=TRUE),"fourth parameterization of the 'Beverton-Holt'")
  expect_message(srFuns("Ricker",param=1,msg=TRUE),"first parameterization of the 'Ricker'")
  expect_message(srFuns("Ricker",param=2,msg=TRUE),"second parameterization of the 'Ricker'")
  expect_message(srFuns("Ricker",param=3,msg=TRUE),"third parameterization of the 'Ricker'")
  expect_message(srFuns("Shepherd",msg=TRUE),"Shepherd")
  expect_message(srFuns("Saila",msg=TRUE),"Saila")
  expect_message(srFuns("independence",msg=TRUE),"density-independent")
})
