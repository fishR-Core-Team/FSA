context("expandCounts() MESSAGES")

test_that("expandCounts() messages",{
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                   upr.bin=c(1.5,2  ,2.5,1.5,2  ,2.5),
                   freq=c(1,2,3,4,5,6))
  ## cform errors
  expect_error(expandCounts(d1,~lwr.bin+upr.bin))
  expect_error(expandCounts(d1,~name))
  expect_error(expandCounts(d1,~name,~lwr.bin+upr.bin))
  expect_error(expandCounts(d1,lwr.bin~upr.bin))
  ## lform errors
  expect_error(expandCounts(d1,~freq,~lwr.bin))
  expect_error(expandCounts(d1,~freq,~lwr.bin+name))
  expect_error(expandCounts(d1,~freq,lwr.bin~upr.bin))
  
  ## A lwr is greater than an upper
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(2  ,1.5,2  ,1  ,1.5,2),
                   upr.bin=c(1.5,2  ,2.5,1.5,2  ,2.5),
                   freq=c(1,2,3,4,5,6))
  expect_error(expandCounts(d1,~freq,~lwr.bin+upr.bin))
  
  ##  some zero counts with both length values
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                   upr.bin=c(1.5,1.5,2.5,1.5,2  ,2.5),
                   freq=c(1,0,3,0,2,3))
  expect_error(expandCounts(d1,~freq,~lwr.bin+upr.bin))
  
  ##  some zero counts with one length values
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                   upr.bin=c(1.5, NA,2.5,NA ,2  ,2.5),
                   freq=c(1,0,3,0,2,3))
  expect_error(expandCounts(d1,~freq,~lwr.bin+upr.bin))
})
