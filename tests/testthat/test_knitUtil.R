context("Tests for knitr utilities")

# ############################################################
# kCounts
# ############################################################
test_that("kCounts() tests",{
  expect_equal(kCounts(0),"zero")
  expect_equal(kCounts(1),"one")
  expect_equal(kCounts(10),"ten")
  expect_equal(kCounts(11),11)
  expect_equal(kCounts(-1),-1)
  expect_equal(kCounts(0,capitalize=TRUE),"Zero")
  expect_equal(kCounts(1,capitalize=TRUE),"One")
  expect_equal(kCounts(10,capitalize=TRUE),"Ten")
  expect_equal(kCounts(11,capitalize=TRUE),11)
  expect_equal(kCounts(-1,capitalize=TRUE),-1)
})

# ############################################################
# kPvalue
# ############################################################
test_that("kPvalue() tests",{
  tmp <- 0.123456789
  expect_equal(kPvalue(tmp),"$p=0.1235$")
  expect_equal(kPvalue(tmp,digits=2),"$p=0.12$")
  expect_equal(kPvalue(tmp,include.p=FALSE),"$0.1235$")
  expect_equal(kPvalue(tmp,latex=FALSE),"p=0.1235")
  expect_equal(kPvalue(tmp,include.p=FALSE,latex=FALSE),"0.1235")
  expect_equal(kPvalue(tmp,digits=2,include.p=FALSE,latex=FALSE),"0.12")
  ## very small p-values
  tmp2 <- tmp/10000000
  expect_equal(kPvalue(tmp2),"$p<0.00005$")
  expect_equal(kPvalue(tmp2,digits=2),"$p<0.005$")
  expect_equal(kPvalue(tmp2,include.p=FALSE),"$<0.00005$")
  expect_equal(kPvalue(tmp2,latex=FALSE),"p<0.00005")
  expect_equal(kPvalue(tmp2,include.p=FALSE,latex=FALSE),"<0.00005")
  expect_equal(kPvalue(tmp2,digits=2,include.p=FALSE,latex=FALSE),"<0.005")
  expect_equal(kPvalue(tmp2,digits=9),"$p=0.000000012$")
  ## very large p-values
  tmp2 <- tmp*10
  expect_equal(kPvalue(tmp2),"$p>1$")
  expect_equal(kPvalue(tmp2,digits=2),"$p>1$")
  expect_equal(kPvalue(tmp2,include.p=FALSE),"$>1$")
  expect_equal(kPvalue(tmp2,latex=FALSE),"p>1")
  expect_equal(kPvalue(tmp2,include.p=FALSE,latex=FALSE),">1")
  expect_equal(kPvalue(tmp2,digits=2,include.p=FALSE,latex=FALSE),">1")
  expect_equal(kPvalue(tmp2,digits=9),"$p>1$")
})
