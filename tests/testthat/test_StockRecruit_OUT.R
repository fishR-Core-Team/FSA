context("Stock-Recruitment OUTPUT")

test_that("srFuns() output",{
  expect_equal(mode(srFunShow(type="BevertonHolt",param=1)),"expression")
  expect_equal(mode(srFunShow(type="BevertonHolt",param=2)),"expression")
  expect_equal(mode(srFunShow(type="BevertonHolt",param=3)),"expression")
  expect_equal(mode(srFunShow(type="BevertonHolt",param=4)),"expression")
  expect_equal(mode(srFunShow(type="Ricker",param=1)),"expression")
  expect_equal(mode(srFunShow(type="Ricker",param=2)),"expression")
  expect_equal(mode(srFunShow(type="Ricker",param=3)),"expression")
  expect_equal(mode(srFunShow(type="Shepherd")),"expression")
  expect_equal(mode(srFunShow(type="SailaLorda")),"expression")
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
  expect_message(srFuns("BevertonHolt",param=1,msg=TRUE),
                 "first parameterization of the 'Beverton-Holt'")
  expect_message(srFuns("BevertonHolt",param=2,msg=TRUE),
                 "second parameterization of the 'Beverton-Holt'")
  expect_message(srFuns("BevertonHolt",param=3,msg=TRUE),
                 "third parameterization of the 'Beverton-Holt'")
  expect_message(srFuns("BevertonHolt",param=4,msg=TRUE),
                 "fourth parameterization of the 'Beverton-Holt'")
  expect_message(srFuns("Ricker",param=1,msg=TRUE),
                 "first parameterization of the 'Ricker'")
  expect_message(srFuns("Ricker",param=2,msg=TRUE),
                 "second parameterization of the 'Ricker'")
  expect_message(srFuns("Ricker",param=3,msg=TRUE),
                 "third parameterization of the 'Ricker'")
  expect_message(srFuns("Shepherd",msg=TRUE),"Shepherd")
  expect_message(srFuns("Saila",msg=TRUE),"Saila")
  expect_message(srFuns("independence",msg=TRUE),"density-independent")
})

test_that("srStarts() output",{
  CodNorwegian$fyear <- factor(CodNorwegian$year)
  ## Returns a list with proper names
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",param=1)
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","b"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",param=2)
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","Rp"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",param=3)
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","b"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",param=4)
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","Rp"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=1)
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","b"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=2)
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","b"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=3)
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","Rp"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="Shepherd")
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","b","c"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="SailaLorda")
  expect_is(tmp,"list")
  expect_equal(names(tmp),c("a","b","c"))
  tmp <- srStarts(recruits~stock,data=CodNorwegian,type="independence")
  expect_is(tmp,"list")
  expect_equal(names(tmp),"a")
})
