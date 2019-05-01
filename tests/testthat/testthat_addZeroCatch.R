## Simulate some data sets ----
# Example 1
df1 <- data.frame(net=c(1,1,1,2,2,3),
                  eff=c(1,1,1,1,1,1),
                  species=c("BKT","LKT","RBT","BKT","LKT","RBT"),
                  catch=c(3,4,5,5,4,3))
# Same as example 1 but with no ancillary data specific to the net number
df2 <- df1[,-2]
# Example Data #3 (All nets have same species ... no zeros needed)
df3 <- data.frame(net=c(1,1,1,2,2,2,3,3,3),
                  eff=c(1,1,1,1,1,1,1,1,1),
                  species=c("BKT","LKT","RBT","BKT","LKT",
                            "RBT","BKT","LKT","RBT"),
                  catch=c(3,4,5,5,4,3,3,2,7))
# Example Data #4 (another variable that needs zeros)
df4 <- df1
df4$recaps <- c(0,0,0,1,2,1)
# Example Data #5 (two "specvar"s)
df5 <- df1
df5$sex <- c("m","m","f","m","f","f")


## Test Messages ----
test_that("addZeroCatch() test messages",{
  expect_error(addZeroCatch(as.matrix(df1[,-3]),eventvar="net",specvar="species",
                            zerovar="catch"),
               "'df' must be a data.frame")
  expect_error(addZeroCatch(df1,eventvar="net",specvar="species"),
               "'zerovar' cannot be missing")
  expect_error(addZeroCatch(df1,eventvar="net",zerovar="catch"),
               "'specvar' cannot be missing")
  expect_error(addZeroCatch(df1,specvar="species",zerovar="catch"),
               "'eventvar' cannot be missing")
  expect_error(addZeroCatch(df1,eventvar="d",specvar="species",zerovar="catch"),
               "Not all 'eventvar'")
  expect_error(addZeroCatch(df1,eventvar="net",specvar="d",zerovar="catch"),
               "Not all 'specvar'")
  expect_error(addZeroCatch(df1,eventvar="net",specvar="species",zerovar="d"),
               "Not all 'zerovar'")
  expect_error(addZeroCatch(df5,eventvar="net",specvar=c("d","sex"),
                            zerovar="catch"),
               "Not all 'specvar'")
  expect_error(addZeroCatch(df1,eventvar="net",specvar="species",
                            zerovar=c("d","recaps")),
               "Not all 'zerovar'")
  expect_warning(addZeroCatch(df3,eventvar="net",specvar="species",
                              zerovar="catch"),
                 "The original data.frame was returned")
})


## Test Output Types ----
test_that("addZeroCatch() test output types",{
  df1mod <- addZeroCatch(df1,eventvar="net",specvar="species",zerovar="catch")
  expect_is(df1mod,"data.frame")
  expect_equal(names(df1mod),names(df1mod))
  expect_true(nrow(df1mod)>nrow(df1))
  df2mod <- addZeroCatch(df2,eventvar="net",specvar="species",zerovar="catch")
  expect_is(df2mod,"data.frame")
  expect_equal(names(df2mod),names(df2mod))
  expect_true(nrow(df2mod)>nrow(df2))
  df3mod <- suppressWarnings(addZeroCatch(df3,eventvar="net",specvar="species",
                                          zerovar="catch"))
  expect_is(df3mod,"data.frame")
  expect_equal(names(df3mod),names(df3mod))
  expect_true(nrow(df3mod)==nrow(df3))
  df4mod <- addZeroCatch(df4,eventvar="net",specvar="species",
                         zerovar=c("catch","recaps"))
  expect_is(df4mod,"data.frame")
  expect_equal(names(df4mod),names(df4mod))
  expect_true(nrow(df4mod)>nrow(df4))
  df5mod <- addZeroCatch(df5,eventvar="net",specvar=c("species","sex"),
                         zerovar="catch")
  expect_is(df5mod,"data.frame")
  expect_equal(names(df5mod),names(df5mod))
  expect_true(nrow(df5mod)>nrow(df5))
})


## Validate Results ----
test_that("addZeroCatch() validate results",{
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
  
  df3mod <- suppressWarnings(addZeroCatch(df3,eventvar="net",
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
