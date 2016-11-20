context("addZeroCatch() VALIDATE")
test_that("addZeroCatch() validate results",{
  source("EXS_addZeroCatch.R")
  ## Check results
  df1mod <- addZeroCatch(df1,eventvar="net",specvar="species",zerovar="catch")
  tmp <- xtabs(~net,data=df1mod)
  expect_true(all(tmp==3))
  tmp <- xtabs(~net+species,data=df1mod)
  expect_true(all(tmp==1))
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),3)
  expect_equal(colnames(tmp),c("BKT","LKT","RBT"))
  
  df2mod <- addZeroCatch(df2,eventvar="net",specvar="species",zerovar="catch")
  tmp <- xtabs(~net,data=df2mod)
  expect_true(all(tmp==3))
  tmp <- xtabs(~net+species,data=df2mod)
  expect_true(all(tmp==1))
  
  df3mod3 <- suppressWarnings(addZeroCatch(df3,eventvar="net",
                                           specvar="species",zerovar="catch"))
  tmp <- xtabs(~net,data=df3mod)
  expect_true(all(tmp==3))
  tmp <- xtabs(~net+species,data=df3mod)
  expect_true(all(tmp==1))
  
  df4mod <- addZeroCatch(df4,eventvar="net",specvar="species",
                         zerovar=c("catch","recaps"))
  tmp <- xtabs(~net,data=df4mod)
  expect_true(all(tmp==3))
  tmp <- xtabs(~net+species,data=df4mod)
  expect_true(all(tmp==1))
  
  df5mod <- addZeroCatch(df5,"net",specvar=c("species","sex"),zerovar="catch")
  tmp <- xtabs(~net,data=df5mod)
  expect_true(all(tmp==6))
  tmp <- xtabs(~net+sex,data=df5mod)
  expect_true(all(tmp==3))
  tmp <- xtabs(~net+species,data=df5mod)
  expect_true(all(tmp==2))
  tmp <- xtabs(~sex+species+net,data=df5mod)
  expect_true(all(tmp==1))
})