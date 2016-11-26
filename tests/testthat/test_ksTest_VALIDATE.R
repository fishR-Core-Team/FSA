context("ksTest() VALIDATE")

test_that("Does ksTest() match ks.test()",{
  x <- rnorm(50)
  y <- runif(30)
  df <- data.frame(dat=c(x,y),grp=rep(c("X","Y"),c(50,30)))
  
  ## one-sample (from ks.test) still works
  tmp1 <- ksTest(x+2, "pgamma", 3, 2)
  tmp2 <- ks.test(x+2, "pgamma", 3, 2)
  expect_equal(tmp1$statistic,tmp2$statistic)
  expect_equal(tmp1$p.value,tmp2$p.value)

  ## first two-sample example in ?ks.test
  tmp1 <- ksTest(x,y)
  tmp2 <- ks.test(x,y)
  tmp3 <- ksTest(dat~grp,data=df)
  expect_equal(tmp1,tmp2)
  expect_equal(tmp3,tmp2)
})
