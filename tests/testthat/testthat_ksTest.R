## Test Messages ----
test_that("ksTest() messages",{
  ## Get data, make two more factor variables
  tmp <- rep(LETTERS[1:3],each=17)[-1]
  iris$fact1 <- factor(c(tmp,tmp,tmp))
  tmp <- c(rep(letters[1:6],each=8),"f","f")
  iris$fact2 <- factor(c(tmp,tmp,tmp))
  ## Problems with the formula
  expect_error(ksTest(~Species,data=iris),
               "must contain 1 response")
  expect_error(ksTest(Species~1,data=iris),
               "must contain 1 response")
  expect_error(ksTest(Species~Sepal.Width,data=iris),
               "LHS variable must be numeric")
  expect_error(ksTest(Species~fact1,data=iris),
               "LHS variable must be numeric")
  expect_error(ksTest(Sepal.Length~Sepal.Width,data=iris),
               "RHS Variable must be a factor")
  expect_error(ksTest(Sepal.Length~Species+Sepal.Width,data=iris),
               "must contain 1 response")
  expect_error(ksTest(Sepal.Length~Species+fact1+fact2,data=iris),
               "must contain 1 response")
  ## Problems with number of levels in factor variable
  expect_error(ksTest(Sepal.Length~Species,data=iris),
               "RHS variable has two levels")
})


## Test Output Types ----
test_that("Does ksTest() match ks.test()",{
  x <- rnorm(50)
  y <- runif(30)
  df <- data.frame(dat=c(x,y),grp=factor(rep(c("X","Y"),c(50,30))),
                   stringsAsFactors=FALSE)
  
  ## one-sample (from ks.test) still works
  tmp1 <- ksTest(x+2,"pgamma",3,2)
  tmp1o <- ks.test(x+2,"pgamma",3,2)
  expect_equal(class(tmp1),c("ks.test","htest"))
  expect_equal(mode(tmp1),"list")
  expect_equal(names(tmp1),names(tmp1o))
  
  ## first two-sample example in ?ks.test
  tmp1 <- ksTest(x,y)
  tmp1o <- ks.test(x,y)
  expect_equal(class(tmp1),c("ks.test","htest"))
  expect_equal(mode(tmp1),"list")
  expect_equal(names(tmp1),names(tmp1o))
  
  # using formula notation
  tmp1f <- ksTest(dat~grp,data=df)
  expect_equal(class(tmp1f),c("ks.test","htest"))
  expect_equal(mode(tmp1f),"list")
  expect_equal(names(tmp1f),names(tmp1o))
  expect_identical(tmp1,tmp1f)
})


## Validate Results ----
test_that("Does ksTest() match ks.test()",{
  x <- rnorm(50)
  y <- runif(30)
  df <- data.frame(dat=c(x,y),grp=factor(rep(c("X","Y"),c(50,30))),
                   stringsAsFactors=FALSE)
  
  ## one-sample (from ks.test) still works
  tmp1 <- ksTest(x+2, "pgamma", 3, 2)
  tmp1o <- ks.test(x+2, "pgamma", 3, 2)
  expect_equal(tmp1$statistic,tmp1o$statistic)
  expect_equal(tmp1$p.value,tmp1o$p.value)
  
  ## first two-sample example in ?ks.test
  tmp1 <- ksTest(x,y)
  tmp1o <- ks.test(x,y)
  tmp1f <- ksTest(dat~grp,data=df)
  expect_equal(tmp1,tmp1o)
  expect_equal(tmp1f,tmp1o)
})

