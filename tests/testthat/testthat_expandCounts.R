## Setup data for tests ----
## some expansions, all need digits
good1 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                    upr.bin=c(1.5,2  ,2.5,1.5,2  ,2.5),
                    freq=c(1,2,3,4,5,6))

## some expansions, none need digits
good2 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                    upr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                    freq=c(1,2,3,4,5,6))

## no expansions, all need digits
good3 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2),
                    upr.bin=c(1.5,2  ,2.5,1.5,2  ,2.5),
                    freq=c(1,1,1,1,1,1))

## some expansions, some need digits
good4 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                    upr.bin=c(1.5,1.5,2.5,1.5,2  ,2.5),
                    freq=c(1,2,3,1,2,3))

## no expansions, none need digits
good5 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                    upr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                    freq=c(1,1,1,1,1,1))

## some expansions, some need digits, missing upper values
good6 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,1.5,2  ,1  ,1.5,2.5),
                    upr.bin=c(1.5,NA ,2.5,NA ,2  ,2.5),
                    freq=c(1,2,3,1,2,3))

## some expansions, some need digits, missing lower values
good7 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,NA ,2  ,NA ,1.5,NA),
                    upr.bin=c(1.5,1.5,2.5,1.5,2  ,2.5),
                    freq=c(1,2,3,1,2,3))

## some expansions, some need digits -- some zero counts
good8 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,NA,2  ,NA,1.5,2.5),
                    upr.bin=c(1.5,NA,2.5,NA,2  ,2.5),
                    freq=c(1,0,3,0,2,3))

## some expansions, some need digits -- some NA counts
good9 <- data.frame(name=c("A","A","A","B","B","C"),
                    lwr.bin=c(1  ,NA,2  ,NA,1.5,2.5),
                    upr.bin=c(1.5,NA,2.5,NA,2  ,2.5),
                    freq=c(1,NA,3,NA,2,3))

# some need expansion, but different bin widths
good10 <- data.frame(name=c("A","A","A","B","B","C"),
                     lwr.bin=c(15,  15,  16,  16,  17.1,17.3),
                     upr.bin=c(15.5,15.9,16.5,16.9,17.1,17.3),
                     freq=c(6,4,2,3,1,1))

## Test Messages ----
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


## Test Output Types and Validate Results ----
test_that("expandCounts() type and value of results",{
  tmp <- expandCounts(good1,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good1)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good1$freq))
  expect_equal(max(apply(matrix(tmp$newlen,ncol=1),1,FSA:::iGetDecimals)),1)
  exp <- tapply(good1$freq,good1$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  # same as above, just with diffrent new.name
  tmp <- expandCounts(good1,~freq,~lwr.bin+upr.bin,new.name="DEREK",verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good1)[-4],"DEREK","lennote"))
  expect_equal(nrow(tmp),sum(good1$freq))
  expect_equal(max(apply(matrix(tmp$DEREK,ncol=1),1,FSA:::iGetDecimals)),1)
  exp <- tapply(good1$freq,good1$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  # same as first, but different lprec
  tmp <- expandCounts(good1,~freq,~lwr.bin+upr.bin,lprec=1,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good1)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good1$freq))
  expect_equal(max(apply(matrix(tmp$newlen,ncol=1),1,FSA:::iGetDecimals)),0)
  exp <- tapply(good1$freq,good1$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  # same as first, but different lprec
  tmp <- expandCounts(good1,~freq,~lwr.bin+upr.bin,lprec=0.01,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good1)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good1$freq))
  expect_equal(max(apply(matrix(tmp$newlen,ncol=1),1,FSA:::iGetDecimals)),2)
  exp <- tapply(good1$freq,good1$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  tmp <- expandCounts(good2,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good2)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good2$freq))
  exp <- tapply(good2$freq,good2$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  tmp <- expandCounts(good3,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good3)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good3$freq))
  exp <- tapply(good3$freq,good3$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  tmp <- expandCounts(good4,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good4)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good4$freq))
  exp <- tapply(good4$freq,good4$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  tmp <- expandCounts(good5,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good5)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good5$freq))
  exp <- tapply(good5$freq,good5$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  tmp <- expandCounts(good6,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good6)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good6$freq))
  exp <- tapply(good6$freq,good6$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  tmp <- expandCounts(good7,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good7)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good7$freq))
  exp <- tapply(good7$freq,good7$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  tmp <- expandCounts(good8,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good8)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good8$freq)+2) # adjusted for NAs
  exp <- tapply(good8$freq,good8$name,FUN=sum)
  obs <- xtabs(~name,data=filterD(tmp,!is.na(lwr.bin)))
  expect_equivalent(sum(obs),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  tmp <- expandCounts(good9,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good9)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good9$freq,na.rm=TRUE)+2) # adjusted for NAs
  exp <- tapply(good9$freq,good9$name,FUN=sum,na.rm=TRUE)
  obs <- xtabs(~name,data=filterD(tmp,!is.na(lwr.bin)))
  expect_equivalent(sum(obs),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  tmp <- expandCounts(good10,~freq,~lwr.bin+upr.bin,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good10)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good10$freq,na.rm=TRUE))
  exp <- tapply(good10$freq,good10$name,FUN=sum,na.rm=TRUE)
  obs <- xtabs(~name,data=filterD(tmp,!is.na(lwr.bin)))
  expect_equivalent(sum(obs),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
  
  # Same as first, but not lower and upper bins
  tmp <- expandCounts(good1,~freq,verbose=FALSE)
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good1)[-4]))
  expect_equal(nrow(tmp),sum(good1$freq))
  exp <- tapply(good1$freq,good1$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))

  # Same as first but with verbose
  expect_message( tmp <- expandCounts(good1,~freq,~lwr.bin+upr.bin,verbose=TRUE) )
  expect_is(tmp,"data.frame")
  expect_equal(names(tmp),c(names(good1)[-4],"newlen","lennote"))
  expect_equal(nrow(tmp),sum(good1$freq))
  expect_equal(max(apply(matrix(tmp$newlen,ncol=1),1,FSA:::iGetDecimals)),1)
  exp <- tapply(good1$freq,good1$name,FUN=sum)
  obs <- xtabs(~name,data=tmp)
  expect_equivalent(nrow(tmp),sum(exp))
  expect_equivalent(as.vector(obs),as.vector(exp))
})