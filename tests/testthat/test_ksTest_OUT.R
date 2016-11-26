context("ksTest() OUTPUT")

test_that("Does ksTest() match ks.test()",{
  x <- rnorm(50)
  y <- runif(30)
  df <- data.frame(dat=c(x,y),grp=rep(c("X","Y"),c(50,30)))
  
  ## one-sample (from ks.test) still works
  tmp <- ksTest(x+2,"pgamma",3,2)
  expect_is(tmp,"htest")
  expect_equal(mode(tmp),"list")
  expect_equal(names(tmp),c("statistic","p.value","alternative","method","data.name"))

  ## first two-sample example in ?ks.test
  tmp <- ksTest(x,y)
  expect_is(tmp,"htest")
  expect_equal(mode(tmp),"list")
  expect_equal(names(tmp),c("statistic","p.value","alternative","method","data.name"))
  # using formula notation
  tmp1 <- ksTest(dat~grp,data=df)
  expect_is(tmp1,"htest")
  expect_equal(mode(tmp1),"list")
  expect_equal(names(tmp1),c("statistic","p.value","alternative","method","data.name"))
  expect_identical(tmp,tmp1)
})
