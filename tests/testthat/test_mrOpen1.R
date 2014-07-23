context("mrOpen Messages")

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

test_that("mrOpen errors and warnings",{
  ## a top but not a bottom, but with capHistSum
  expect_that(mrOpen(good.top),throws_error())
  ## a top that is not square
  expect_that(mrOpen(good.top[,-1],good.bot),throws_error())
  expect_that(mrOpen(good.top[-1,],good.bot),throws_error())
  ## a top without an NA on the diagonal or lower triangle
  bad.top <- good.top
  bad.top[2,2] <- 3
  expect_that(mrOpen(bad.top,good.bot),throws_error())
  bad.top <- good.top
  bad.top[3,2] <- 3
  expect_that(mrOpen(bad.top,good.bot),throws_error())
  ## a top with an NA in the upper triangle
  bad.top <- good.top
  bad.top[1,2] <- NA
  expect_that(mrOpen(bad.top,good.bot),throws_error())
  ## a top with a negative value in the upper triangle
  bad.top <- good.top
  bad.top[1,2] <- -3
  expect_that(mrOpen(bad.top,good.bot),throws_error())
  
  ## bottom does not have enough rows
  expect_that(mrOpen(good.top,good.bot[-1,]),throws_error())
  ## bottom has bad names
  bad.bot <- good.bot
  names(bad.bot)[1] <- "Derek"
  expect_that(mrOpen(good.top,bad.bot),throws_error())
  ## a bottom with a negative value
  bad.bot <- good.bot
  bad.bot[1,2] <- -3
  expect_that(mrOpen(good.top,bad.bot),throws_error())
  ## a bottom with a non-zero first number of marked fish
  bad.bot <- good.bot
  bad.bot["m",1] <- 3
  expect_that(mrOpen(good.top,bad.bot),throws_error())
  ## a bottom with a NA
  bad.bot["m",1] <- NA
  expect_that(mrOpen(good.top,bad.bot),throws_error())
})
