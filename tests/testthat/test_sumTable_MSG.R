context("sumTable() MESSAGES")
source("EXS_sumTable.R")

test_that("sumTable() messages",{
  expect_error(sumTable(dat+junk~g1+g2,data=d),"more than one variable on the LHS")
  expect_error(sumTable(dat~g1+g2+g3,data=d),"one or two factor variables on RHS of formula")
  expect_error(sumTable(~g1,data=d),"one or two factor variables on RHS of formula")
  expect_warning(sumTable(dat~junk,data=d),"RHS variable was converted to a factor")
  expect_warning(sumTable(dat~g1+junk,data=d),"Second RHS variable was converted to a factor.")
  expect_warning(sumTable(dat~junk+g1,data=d),"First RHS variable was converted to a factor")
})
