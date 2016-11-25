context("mrClosed() MESSAGES")

test_that("mrClosed() Single Census messages",{
  ## wrong type
  expect_error(mrClosed(346,184,49,method="Derek"),"should be one of")
  ## missing numerical arguments
  expect_error(mrClosed(346),"One or both of 'n' or 'm' is missing")
  expect_error(mrClosed(346,184),"One or both of 'n' or 'm' is missing")
  ## multiple groups, missing arguments or different lengths
  expect_error(mrClosed(c(346,346)),"One or both of 'n' or 'm' is missing")
  expect_error(mrClosed(c(346,346),c(184,184),49),"vectors must be same length")
  expect_error(mrClosed(c(346,346),c(184,184),c(49,49,49)),"vectors must be same length")
  expect_error(mrClosed(c(346,346),c(184,184),c(49,49),labels="Derek"),
               "'labels' must be same length as")
  expect_error(mrClosed(c(346,346),c(184,184),c(49,49),labels=c("A","B","C")),
               "'labels' must be same length as")
  ## R not used in single census
  expect_warning(mrClosed(346,184,49,200),"'R' not used in single census methods")
  ## no M
  expect_error(mrClosed(n=200),"'M' is missing")
  expect_error(mrClosed(m=200),"'M' is missing")
  ## give R
  expect_error(mrClosed(R=200),"'R' not used in single census methods")
  expect_warning(mrClosed(M=346,n=184,m=49,R=200),"'R' not used in single census methods")
  ## can't have more recaps (m) than number checked (n)
  expect_error(mrClosed(346,184,200),"Can't have more recaptures")
  expect_error(mrClosed(c(346,346),c(184,184),c(49,200)),"has more recaptures")
  ## using capHistSum() but trying to provide other values
  data(BluegillJL)
  ch1 <- capHistSum(BluegillJL)
  expect_warning(mrClosed(ch1,n=90),"ignored when 'M' from")
  ## confint problems
  mr1 <- mrClosed(ch1)
  expect_error(confint(mr1,conf.level=0),"must be between 0 and 1")
  expect_error(confint(mr1,conf.level=1),"must be between 0 and 1")
  expect_error(confint(mr1,type="derek"),"should be one of")
  expect_error(confint(mr1,type="binomial",bin.type="derek"),"should be one of")
  expect_error(confint(mr1,type="poisson",poi.type="derek"),"should be one of")
  expect_warning(confint(mr1,parm="N"),"is meaningless")
})
  
test_that("mrClosed Multiple Census errors and warnings",{
  n1 <- c(20,18,14, 9,17)
  m1 <- c( 0, 3, 4, 4,14)
  R1 <- c(20,18,14, 9, 0)
  M1 <- c( 0,20,35,45,50)
  ## missing numerical arguments
  expect_error(mrClosed(n=n1,method="Schnabel"),"must be supplied")
  expect_error(mrClosed(n=n1,m=m1,method="Schnabel"),"must be supplied")
  expect_error(mrClosed(M=M1,method="Schnabel"),"missing without 'M'")
  expect_error(mrClosed(M=M1,n=n1,method="Schnabel"),"missing without 'M'")
  expect_error(mrClosed(R=R1,method="Schnabel"),"One or both of")
  expect_error(mrClosed(R=R1,n=n1,method="Schnabel"),"One or both of")
  expect_error(mrClosed(M=M1,R=R1,method="Schnabel"))
  expect_warning(mrClosed(M=M1,n=n1,m=m1,R=R1,method="Schnabel"),"Only need one of")
  ## NAs in some of the vectors
  m1x <- c(NA, 3, 4, 4,14)
  R1x <- c(20,18,14, 9,NA)
  M1x <- c(NA,20,35,45,50)
  expect_warning(mrClosed(n=n1,m=m1x,R=R1,method="Schnabel"),"'m' was ignored")
  expect_warning(mrClosed(n=n1,m=m1,R=R1x,method="Schnabel"),"'R' was ignored")
  expect_warning(mrClosed(n=n1,m=m1x,M=M1x,method="Schnabel"),"'M' was ignored")
  ## confint problems
  mr1 <- mrClosed(M1,n1,m1)
  expect_error(confint(mr1,conf.level=0),"must be between 0 and 1")
  expect_error(confint(mr1,conf.level=1),"must be between 0 and 1")
  expect_error(confint(mr1,poi.type="derek"),"should be one of")
  mr1 <- mrClosed(M1,n1,m1,method="Schumacher")
  expect_error(confint(mr1,conf.level=0),"must be between 0 and 1")
  expect_error(confint(mr1,conf.level=1),"must be between 0 and 1")
  expect_warning(confint(mr1,type="Poisson"),"changed to")
  expect_warning(confint(mr1,parm="N"),"is meaningless")
})
