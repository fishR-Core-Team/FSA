context("sumTable() tests")

## Create simple dummy data
d <- data.frame(g1=factor(c("A","A","A","B","B","B","C","C","C")),
                g2=factor(c("a","b","c","a","b","c","a","b","c")),
                g3=factor(c("x","x","x","x","x","y","y","y","y")),
                dat=1:9,junk=11:19)

test_that("sumTable() messages",{
  expect_error(sumTable(dat+junk~g1+g2,data=d),"more than one variable on the LHS")
  expect_error(sumTable(dat~g1+g2+g3,data=d),"one or two factor variables on RHS of formula")
  expect_error(sumTable(~g1,data=d),"one or two factor variables on RHS of formula")
  expect_warning(sumTable(dat~junk,data=d),"RHS variable was converted to a factor")
  expect_warning(sumTable(dat~g1+junk,data=d),"Second RHS variable was converted to a factor.")
  expect_warning(sumTable(dat~junk+g1,data=d),"First RHS variable was converted to a factor")
})

test_that("SumTable() Results",{
  expect_equivalent(sumTable(dat~g1,data=d),array(c(2,5,8),dimnames=list(c("A","B","C"))))
  expect_equivalent(sumTable(dat~g2,data=d),array(c(4,5,6),dimnames=list(c("a","b","c"))))
  expect_equivalent(sumTable(dat~g1*g2,data=d),matrix(1:9,nrow=3,byrow=TRUE))
  expect_equivalent(sumTable(dat~g1,data=d,FUN=length),array(c(3,3,3),dimnames=list(c("A","B","C"))))
  expect_equivalent(sumTable(dat~g1*g2,data=d,FUN=length),matrix(1,nrow=3,ncol=3,byrow=TRUE))
  expect_equivalent(sumTable(dat~g1,data=d,FUN=sd),array(c(1,1,1),dimnames=list(c("A","B","C"))))
  expect_equivalent(sumTable(dat~g1*g2,data=d,FUN=sd),matrix(NA,nrow=3,ncol=3,byrow=TRUE))
})