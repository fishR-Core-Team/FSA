test_that("hist.formula() messages and results",{
  ## Make some data
  set.seed(634434789)
  df <- data.frame(quant1=round(rnorm(100,100,20)),
                   quant2=round(rnorm(100,500,50)),
                   fact1=sample(LETTERS[1:3],100,replace=TRUE))
  df$lcat10 <- lencat(df$quant1,w=10)
  df$lcat5 <- lencat(df$quant1,w=5)

  ## Problems with xtabs() results
  xtbl1 <- xtabs(~fact1,data=df)
  expect_error(histFromSum(xtbl1),"are not numeric")
  xtbl1 <- xtabs(~fact1+lcat10,data=df)
  expect_error(histFromSum(xtbl1),"1-dimensional")

  ## Problems with table() results
  xtbl1 <- table(df$fact1)
  expect_error(histFromSum(xtbl1),"are not numeric")
  xtbl1 <- table(df$fact1,df$lcat10)
  expect_error(histFromSum(xtbl1),"1-dimensional")

  ## Problems with using formula on a data.frame of results
  tmp <- data.frame(xtabs(~lcat10,data=df))
  tmp$lcat10q <- fact2num(tmp$lcat10)
  expect_error(histFromSum(~lcat10q,data=tmp),"1 response and 1 explanatory variable")
  expect_error(histFromSum(Freq~lcat10q+lcat10,data=tmp),"1 response and 1 explanatory variable")
  expect_error(histFromSum(~lcat10q+Freq,data=tmp),"1 left-side variable")
  expect_error(histFromSum(lcat10q~1,data=tmp),"1 response and 1 explanatory variable")
  expect_error(histFromSum(lcat10~Freq,data=tmp),"left-side variable in the formula must be numeric")
  expect_error(histFromSum(Freq~lcat10,data=tmp),"right-side variable in the formula must be numeric")

  ## Problems with data types for a data.frame of results
  expect_error(histFromSum(tmp$lcat10,tmp$Freq),"must be a numeric vector")
  expect_error(histFromSum(tmp$Freq,tmp$lcat10),"must be a numeric vector")
  expect_error(histFromSum(tmp[,2:3],tmp$Freq),"must be a numeric vector")
  expect_error(histFromSum(tmp$Freq,tmp[,2:3]),"must be a numeric vector")

  ## Problems with data types for a matrix of results
  tmp <- as.matrix(tmp[,c(2:3,2:3)])
  expect_error(histFromSum(tmp[,1:2],tmp[,3]),"must be a vector")
  expect_error(histFromSum(tmp[,1],tmp[,2:3]),"must be a vector")
})
