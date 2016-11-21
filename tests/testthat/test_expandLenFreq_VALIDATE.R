context("expandLenFreq() VALIDATE")

test_that("expandLenFreq() results",{
  lens1 <- c(1,2,3)
  # simple example using additional
  tmp <- expandLenFreq(lens1,1,9,show.summary=FALSE)
  expect_equal(length(tmp),9)
  expect_equal(min(tmp),1)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,3,3))
  # not so simple (but set seed to make reproducible) using additional
  set.seed(1)
  tmp <- expandLenFreq(lens1,1,10,show.summary=FALSE)
  expect_equal(length(tmp),10)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,4,3))
  expect_equal(min(tmp),1)
  # simple example using total
  tmp <- expandLenFreq(lens1,1,total=12,show.summary=FALSE)
  expect_equal(length(tmp),9)
  expect_equal(min(tmp),1)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,3,3))
  # not so simple (but set seed to make reproducible) using additional
  set.seed(1)
  tmp <- expandLenFreq(lens1,1,total=13,show.summary=FALSE)
  expect_equal(length(tmp),10)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,4,3))
  expect_equal(min(tmp),1)
  ## With decimals
  lens2 <- c(1.1,1.2,2.3,2.4,3.5,3.6)
  tmp <- expandLenFreq(lens2,1,9,show.summary=FALSE)
  expect_equal(length(tmp),9)
  expect_equal(min(tmp),1)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,3,3))
  # not so simple (but set seed to make reproducible) using additional
  set.seed(1)
  tmp <- expandLenFreq(lens2,1,10,show.summary=FALSE)
  expect_equal(length(tmp),10)
  expect_equal(as.numeric(xtabs(~tmp)),c(3,4,3))
  expect_equal(min(tmp),1)
  ## With decimals, but unequal numbers
  lens3 <- c(1.1,1.2,1.3,2.4,2.5,3.6)
  tmp <- expandLenFreq(lens3,1,12,show.summary=FALSE)
  expect_equal(length(tmp),12)
  expect_equal(min(tmp),1)
  expect_equal(as.numeric(xtabs(~tmp)),c(6,4,2))
  # not so simple (but set seed to make reproducible) using additional
  set.seed(1)
  tmp <- expandLenFreq(lens3,1,13,show.summary=FALSE)
  expect_equal(length(tmp),13)
  expect_equal(as.numeric(xtabs(~tmp)),c(7,4,2))
  expect_equal(min(tmp),1)
})