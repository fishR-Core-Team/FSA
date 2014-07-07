context("mrClosed Messages")

test_that("mrClosed Single Census errors and warnings",{
  ## wrong type
  expect_that(mrClosed(346,184,49,type="Derek"),throws_error())
  ## missing numerical arguments
  expect_that(mrClosed(346),throws_error())
  expect_that(mrClosed(346,184),throws_error())
  ## multiple groups, missing arguments or different lengths
  expect_that(mrClosed(c(346,346)),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),49),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),c(49,49,49)),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),c(49,49),labels="Derek"),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),c(49,49),labels=c("A","B","C")),throws_error())
  ## R not used in single census
  expect_that(mrClosed(346,184,49,200),gives_warning())
  ## no M
  expect_that(mrClosed(n=200),throws_error())
  expect_that(mrClosed(m=200),throws_error())
  expect_that(mrClosed(R=200),throws_error())
  ## can't have more recaps (m) than number checked (n)
  expect_that(mrClosed(346,184,200),throws_error())
  expect_that(mrClosed(c(346,346),c(184,184),c(49,200)),throws_error())
  ## using capHistSum() but trying to provide other values
  data(BluegillJL)
  ch1 <- capHistSum(BluegillJL)
  expect_that(mrClosed(ch1,n=90),gives_warning())
})
  
test_that("mrClosed Multiple Census errors and warnings",{
  n1 <- c(20,18,14, 9,17)
  m1 <- c( 2, 3, 4, 4,14)
  R1 <- c(20,18,14, 9, 0)
  M1 <- c( 0,20,35,45,50)
  ## missing numerical arguments
  expect_that(mrClosed(n=n1,type="Schnabel"),throws_error())
  expect_that(mrClosed(n=n1,m=m1,type="Schnabel"),throws_error())
  expect_that(mrClosed(M=M1,type="Schnabel"),throws_error())
  expect_that(mrClosed(M=M1,n=n1,type="Schnabel"),throws_error())
  expect_that(mrClosed(R=R1,type="Schnabel"),throws_error())
  expect_that(mrClosed(R=R1,n=n1,type="Schnabel"),throws_error())
  expect_that(mrClosed(M=M1,R=R1,type="Schnabel"),throws_error())
  expect_that(mrClosed(M=M1,n=n1,m=m1,R=R1,type="Schnabel"),gives_warning())
  tmp <- mrClosed(M=M1,n=n1,m=m1,type="Schnabel")
  expect_that(summary(tmp,incl.SE=TRUE),gives_warning())
})  