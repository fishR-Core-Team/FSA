context("lencat() VALIDATE")
source("EXS_lencat.R")

test_that("lencat() results",{
  ## Simple examples (same width as that created)
  tmp <- lencat(~len1,data=df2,w=1)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freq)
  tmp <- lencat(~len0.1,data=df2,w=0.1)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freq)
  tmp <- lencat(~len0.01,data=df2,w=0.01)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freq)
  tmp <- lencat(~len10,data=df2,w=10)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freq)
  ## Does it handle 1-column data.frame
  tmp <- lencat(data.frame(df2$len1),w=1)
  expect_is(tmp,"numeric")
  expect_equal(as.numeric(xtabs(~tmp)),freq)
  ## Different widths (don't control startcat)
  freqtmp <- c(0,freq[c(2,4)])+freq[c(1,3,5)]
  tmp <- lencat(~len1,data=df2,w=2)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  tmp <- lencat(~len0.1,data=df2,w=0.2)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  tmp <- lencat(~len10,data=df2,w=20)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  ## Different widths (control startcat)
  freqtmp <- freq[c(1,3,5)]+c(freq[c(2,4)],0)
  tmp <- lencat(~len1,data=df2,w=2,startcat=1)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  tmp <- lencat(~len0.1,data=df2,w=0.2,startcat=0.1)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  tmp <- lencat(~len10,data=df2,w=20,startcat=10)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  ## Using breaks
  freqtmp <- freq[c(1,2,4)]+c(0,freq[c(3,5)])
  tmp <- lencat(~len1,data=df2,breaks=c(1,2,4))
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  tmp <- lencat(~len0.1,data=df2,breaks=c(1,2,4)/10)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  ## using named breaks
  # but don't use names
  tmp <- lencat(~len1,data=df2,breaks=c(one=1,two=2,four=4))
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  # but do use names
  tmp <- lencat(~len1,data=df2,breaks=c(one=1,two=2,four=4),use.names=TRUE)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp)
  # use names but don't return as a factor
  tmp <- lencat(~len1,data=df2,breaks=c(one=1,two=2,four=4),use.names=TRUE,as.fact=FALSE)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),freqtmp[c(3,1,2)])
  # Don't need an automatic larger break
  tmp <- lencat(~len1,data=df2,breaks=c(one=1,two=2,four=4,six=6),use.names=TRUE)
  expect_equal(as.numeric(xtabs(~LCat,data=tmp)),c(freqtmp,0))
})