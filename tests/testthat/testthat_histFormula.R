## Test Messages ----
test_that("hist.formula() messages",{
  ## Get data, make two more factor variables
  tmp <- rep(LETTERS[1:3],each=17)[-1]
  iris$fact1 <- factor(c(tmp,tmp,tmp))
  tmp <- c(rep(letters[1:6],each=8),"f","f")
  iris$fact2 <- factor(c(tmp,tmp,tmp))
  ## Problems with the formula
  expect_error(FSA:::hist.formula(~Species,data=iris),
               "must be numeric")
  expect_error(FSA:::hist.formula(Species~1,data=iris),
               "must be numeric")
  expect_error(FSA:::hist.formula(Species~Sepal.Width,data=iris),
               "must be numeric")
  expect_error(FSA:::hist.formula(Species~fact1,data=iris),
               "must be numeric")
  expect_error(FSA:::hist.formula(Sepal.Length~Sepal.Width,data=iris),
               "only factor variables")
  expect_error(FSA:::hist.formula(Sepal.Length~Species+Sepal.Width,data=iris),
               "only factor variables")
  expect_error(FSA:::hist.formula(Sepal.Length~Species+fact1+fact2,data=iris),
               "only works with")
  ## Problems with ymax
  expect_error(FSA:::hist.formula(Sepal.Length~Species,data=iris,ymax=c(20,20)),
               "length equal to the number of")
  expect_error(FSA:::hist.formula(Sepal.Length~Species,data=iris,
                                  ymax=c(20,20,20,20)),
               "length equal to the number of")
  ## Problems with w
  expect_error(FSA:::hist.formula(Sepal.Length~Species,data=iris,w=-1),
               "must be positive")
  expect_error(FSA:::hist.formula(Sepal.Length~Species,data=iris,w=1:2),
               "must be a single value")
  expect_error(FSA:::hist.formula(Sepal.Length~Species,data=iris,w="derek"),
               "must be numeric")
})


## Test Output Types ----
test_that("hist.formula() w= vs breaks=",{
  tmp1 <- hist(~Sepal.Length,data=iris,breaks=seq(4,8,1))
  tmp2 <- hist(~Sepal.Length,data=iris,w=1)
  expect_identical(tmp1,tmp2)
  tmp1 <- hist(~Sepal.Length,data=iris,breaks=seq(4,8,2))
  tmp2 <- hist(~Sepal.Length,data=iris,w=2)
  expect_identical(tmp1,tmp2)
  tmp1 <- hist(~Sepal.Length,data=iris,breaks=seq(4,8,0.5))
  tmp2 <- hist(~Sepal.Length,data=iris,w=0.5)
  expect_identical(tmp1,tmp2)
  tmp1 <- hist(~Sepal.Length,data=iris,breaks=seq(4.25,8,0.25))
  tmp2 <- hist(~Sepal.Length,data=iris,w=0.25)
  expect_identical(tmp1,tmp2)
})


## Validate Results ----


