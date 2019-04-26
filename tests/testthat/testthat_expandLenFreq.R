## Test Messages ----
test_that("expandLenFreq() messages",{
  ## dummy bad data
  x <- 1:26
  df <- data.frame(x,letters)
  ## check obvious errors
  expect_error(expandLenFreq(df,1,17),"must be a vector")
  expect_error(expandLenFreq(letters,1,17),"must be numeric")
  expect_error(expandLenFreq(x,-1,17),"must be positive")
  expect_error(expandLenFreq(x,1,17,startcat=-2),"must be positive")
  expect_error(expandLenFreq(x,1,total=17),"Total number to expand")
  ## do you get a message
  expect_output(expandLenFreq(x,1,17),"Length Frequency Expansion using")
})


## Test Output Types and Validate Results ----
test_that("expandLenFreq() results",{
  lens1 <- c(1,2,3)
  # simple example using additional
  tmp <- expandLenFreq(lens1,1,9,show.summary=FALSE)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),9)
  expect_equal(min(tmp),1)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,3,3))
  # not so simple (but set seed to make reproducible) using additional
  set.seed(1)
  tmp <- expandLenFreq(lens1,1,10,show.summary=FALSE)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),10)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,4,3))
  expect_equal(min(tmp),1)
  # simple example using total
  tmp <- expandLenFreq(lens1,1,total=12,show.summary=FALSE)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),9)
  expect_equal(min(tmp),1)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,3,3))
  # not so simple (but set seed to make reproducible) using additional
  set.seed(1)
  tmp <- expandLenFreq(lens1,1,total=13,show.summary=FALSE)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),10)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,4,3))
  expect_equal(min(tmp),1)
  ## With decimals
  lens2 <- c(1.1,1.2,2.3,2.4,3.5,3.6)
  tmp <- expandLenFreq(lens2,1,9,show.summary=FALSE)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),9)
  expect_equal(min(tmp),1)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,3,3))
  # not so simple (but set seed to make reproducible) using additional
  set.seed(1)
  tmp <- expandLenFreq(lens2,1,10,show.summary=FALSE)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),10)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,4,3))
  expect_equal(min(tmp),1)
  ## With decimals, but unequal numbers
  lens3 <- c(1.1,1.2,1.3,2.4,2.5,3.6)
  tmp <- expandLenFreq(lens3,1,12,show.summary=FALSE)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),12)
  expect_equal(min(tmp),1)
  expect_equal(as.numeric(xtabs(~tmp)),c(6,4,2))
  # not so simple (but set seed to make reproducible) using additional
  set.seed(1)
  tmp <- expandLenFreq(lens3,1,13,show.summary=FALSE)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
  expect_equal(length(tmp),13)
  expect_equal(as.numeric(xtabs(~tmp)),c(7,4,2))
  expect_equal(min(tmp),1)
})
