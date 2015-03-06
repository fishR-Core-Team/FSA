context("lencat() tests")

## simulate data set
df1 <- data.frame(len=round(runif(50,0.1,9.9),1),
                  fish=sample(c("A","B","C"),50,replace=TRUE))

test_that("lencat() messages",{
  # wrong type of variable
  expect_error(lencat(~fish,data=df1))
  # too many variables
  expect_error(lencat(~len+fish,data=df1))
  # bad width values
  expect_error(lencat(~len,data=df1,w=-1))
  expect_error(lencat(~len,data=df1,w=c(0.5,1)))
  # bad startcats
  expect_error(lencat(~len,data=df1,startcat=c(0.5,1)))
})
