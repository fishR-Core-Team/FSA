context("wsXXX and wrXXX functions VALIDATE")
# read in external CSV file
ftmp <- system.file("extdata", "PSDWR_testdata.csv", package="FSA")
df <- read.csv(ftmp)

test_that("wrAdd() matches values computed in Excel.",{
  df$wr <- wrAdd(wt~tl+species,data=df)
  expect_equivalent(df$wr,df$WR)
})