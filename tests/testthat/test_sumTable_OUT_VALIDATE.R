context("sumTable() OUTPUT")
source("EXS_sumTable.R")

test_that("SumTable() Results",{
  # 1-D example of mean
  tmp <- sumTable(dat~g1,data=d)
  expect_is(tmp,"array")
  expect_equal(mode(tmp),"numeric")
  expect_equivalent(tmp,array(c(2,5,8),dimnames=list(c("A","B","C"))))
  # 1-D example of mean
  tmp <- sumTable(dat~g2,data=d)
  expect_is(tmp,"array")
  expect_equal(mode(tmp),"numeric")
  expect_equivalent(tmp,array(c(4,5,6),dimnames=list(c("a","b","c"))))
  # 2-D example of mean
  tmp <- sumTable(dat~g1*g2,data=d)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),3)
  expect_equivalent(tmp,matrix(1:9,nrow=3,byrow=TRUE))
  # 1-D example of length
  tmp <- sumTable(dat~g1,data=d,FUN=length)
  expect_is(tmp,"array")
  expect_equal(mode(tmp),"numeric")
  expect_equivalent(tmp,array(c(3,3,3),dimnames=list(c("A","B","C"))))
  # 2-D example of length
  tmp <- sumTable(dat~g1*g2,data=d,FUN=length)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),3)
  expect_equivalent(tmp,matrix(1,nrow=3,ncol=3,byrow=TRUE))
  # 1-D example of sd
  tmp <- sumTable(dat~g1,data=d,FUN=sd)
  expect_is(tmp,"array")
  expect_equal(mode(tmp),"numeric")
  expect_equivalent(tmp,array(c(1,1,1),dimnames=list(c("A","B","C"))))
  # 2-D example of sd
  tmp <- sumTable(dat~g1*g2,data=d,FUN=sd)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),3)
  expect_equivalent(tmp,matrix(NA,nrow=3,ncol=3,byrow=TRUE))
})