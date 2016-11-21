context("mrOpen() MESSAGES")

# Setup some matrices for the tests
good.top <- matrix(c(
  NA,15, 1, 0, 0,
  NA,NA,13, 3, 0,
  NA,NA,NA,10, 5,
  NA,NA,NA,NA, 9,
  NA,NA,NA,NA,NA),nrow=5,byrow=TRUE)
good.bot <- matrix(c(
  15,14,13, 5,
  10, 9, 8,11,
  25,23,21,16,
  25,23,21,16),nrow=4,byrow=TRUE,
  dimnames=list(c("m","u","n","R")))

test_that("mrOpen() messages",{
  ## a top but not a bottom, but with capHistSum
  expect_error(mrOpen(good.top),"'mb.top' and a 'mb.bot'")
  ## a top that is not square
  expect_error(mrOpen(good.top[,-1],good.bot),"'mb.top' must be square")
  expect_error(mrOpen(good.top[-1,],good.bot),"'mb.top' must be square")
  ## a top without an NA on the diagonal or lower triangle
  bad.top <- good.top
  bad.top[2,2] <- 3
  expect_error(mrOpen(bad.top,good.bot),"Lower triangle and diagonal of 'mb.top'")
  bad.top <- good.top
  bad.top[3,2] <- 3
  expect_error(mrOpen(bad.top,good.bot),"Lower triangle and diagonal of 'mb.top'")
  ## a top with an NA in the upper triangle
  bad.top <- good.top
  bad.top[1,2] <- NA
  expect_error(mrOpen(bad.top,good.bot),"Upper triangle of 'mb.top' cannot contain any 'NA'")
  ## a top with a negative value in the upper triangle
  bad.top <- good.top
  bad.top[1,2] <- -3
  expect_error(mrOpen(bad.top,good.bot),"All non-NA values in 'mb.top' must be non-negative")
  
  ## bottom does not have enough rows
  expect_error(mrOpen(good.top,good.bot[-1,]),"must contain four rows with")
  ## bottom has bad names
  bad.bot <- good.bot
  names(bad.bot)[1] <- "Derek"
  expect_error(mrOpen(good.top,bad.bot),"rownames of 'mb.bot' must be")
  ## a bottom with a negative value
  bad.bot <- good.bot
  bad.bot[1,2] <- -3
  expect_error(mrOpen(good.top,bad.bot),"'mb.bot' must be non-negative")
  ## a bottom with a non-zero first number of marked fish
  bad.bot <- good.bot
  bad.bot["m",1] <- 3
  expect_error(mrOpen(good.top,bad.bot),"First value of 'm' row in 'mb.bot' must be 0")
  ## a bottom with a NA
  bad.bot["m",1] <- NA
  expect_error(mrOpen(good.top,bad.bot),"All values in 'mb.bot' must be non-NA")
  
  ## Confint
  data(CutthroatAL)
  expect_error(mrOpen(capHistSum(CutthroatAL,cols2use=-1),
                      conf.level=0),"must be between 0 and 1")
  expect_error(mrOpen(capHistSum(CutthroatAL,cols2use=-1),
                      conf.level=1),"must be between 0 and 1")
  cutt <- mrOpen(capHistSum(CutthroatAL,cols2use=-1))
  expect_warning(confint(cutt,conf.level=0.95),"It cannot be changed here")
  
  ## Summary()
  cutt <- suppressWarnings(mrOpen(capHistSum(CutthroatAL,cols2use=-1)))
  expect_error(summary(cutt,parm="Derek"),"should be one of")
  expect_message(summary(cutt,verbose=TRUE))
})
