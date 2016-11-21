context("sumTable() OUTPUT")
source("EXS_sumTable.R")

test_that("SumTable() Results",{
  expect_equivalent(sumTable(dat~g1,data=d),array(c(2,5,8),dimnames=list(c("A","B","C"))))
  expect_equivalent(sumTable(dat~g2,data=d),array(c(4,5,6),dimnames=list(c("a","b","c"))))
  expect_equivalent(sumTable(dat~g1*g2,data=d),matrix(1:9,nrow=3,byrow=TRUE))
  expect_equivalent(sumTable(dat~g1,data=d,FUN=length),array(c(3,3,3),dimnames=list(c("A","B","C"))))
  expect_equivalent(sumTable(dat~g1*g2,data=d,FUN=length),matrix(1,nrow=3,ncol=3,byrow=TRUE))
  expect_equivalent(sumTable(dat~g1,data=d,FUN=sd),array(c(1,1,1),dimnames=list(c("A","B","C"))))
  expect_equivalent(sumTable(dat~g1*g2,data=d,FUN=sd),matrix(NA,nrow=3,ncol=3,byrow=TRUE))
})