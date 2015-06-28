context("expandCounts() tests")

test_that("expandCounts() errors",{
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

test_that("expandCounts() gives correct number of results",{
  ## some expansions, all need digits
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                   upr.bin=c(1.5,2  ,2.5,1.5,2  ,2.5),
                   freq=c(1,2,3,4,5,6))
  exp <- tapply(d1$freq,d1$name,FUN=sum)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  ## some expansions, none need digits
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                   upr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                   freq=c(1,2,3,4,5,6))
  exp <- tapply(d1$freq,d1$name,FUN=sum)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  ## no expansions, all need digits
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                   upr.bin=c(1.5,2  ,2.5,1.5,2  ,2.5),
                   freq=c(1,1,1,1,1,1))
  exp <- tapply(d1$freq,d1$name,FUN=sum)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  ## some expansions, some need digits
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                   upr.bin=c(1.5,1.5,2.5,1.5,2  ,2.5),
                   freq=c(1,2,3,1,2,3))
  exp <- tapply(d1$freq,d1$name,FUN=sum)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  ## no expansions, none need digits
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                   upr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                   freq=c(1,1,1,1,1,1))
  exp <- tapply(d1$freq,d1$name,FUN=sum)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  ## some expansions, some need digits, missing upper values
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                   upr.bin=c(1.5,NA ,2.5,NA ,2  ,2.5),
                   freq=c(1,2,3,1,2,3))
  exp <- tapply(d1$freq,d1$name,FUN=sum)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  ## some expansions, some need digits, missing lower values
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,NA ,2  ,NA ,1.5,NA),
                   upr.bin=c(1.5,1.5,2.5,1.5,2  ,2.5),
                   freq=c(1,2,3,1,2,3))
  exp <- tapply(d1$freq,d1$name,FUN=sum)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  ## some expansions, some need digits -- some zero counts
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,NA,2  ,NA,1.5,2.5),
                   upr.bin=c(1.5,NA,2.5,NA,2  ,2.5),
                   freq=c(1,0,3,0,2,3))
  exp <- tapply(d1$freq,d1$name,FUN=sum)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=Subset(tmp,!is.na(lwr.bin)))
  expect_equivalent(sum(obs),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  ## some expansions, some need digits -- some NA counts
  d1 <- data.frame(name=c("A","A","A","B","B","C"),
                   lwr.bin=c(1  ,NA,2  ,NA,1.5,2.5),
                   upr.bin=c(1.5,NA,2.5,NA,2  ,2.5),
                   freq=c(1,NA,3,NA,2,3))
  exp <- tapply(d1$freq,d1$name,FUN=sum,na.rm=TRUE)
  tmp <- expandCounts(d1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  obs <- xtabs(~name,data=Subset(tmp,!is.na(lwr.bin)))
  expect_equivalent(sum(obs),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
})