context("FSA utilities MESSAGES")


test_that("chooseColors() messages",{
  ## check error messages
  expect_error(chooseColors("Derek"),"should be one of")
  expect_error(chooseColors(num=0),"positive")
})

test_that("col2rgbt() messages",{
  expect_error(col2rgbt("black",-1),"must be greater than 0")
  expect_error(col2rgbt("black",0),"must be greater than 0")
  expect_error(col2rgbt(c("black","blue","red"),c(2,3)),"must be 1 or same as length")
})

test_that("diags() messages",{
  mat1 <- matrix(1:16,nrow=4)
  mat2 <- matrix(1:20,nrow=4)
  expect_error(diags(0:5),"only works with matrices")
  expect_error(diags(matrix(0:5,nrow=6)),"more than 1 column")
  expect_error(diags(matrix(0:5,ncol=6)),"more than 1 row")
  expect_error(diags(mat1,which=-4),"diagonal does not exist")
  expect_error(diags(mat1,which=4),"diagonal does not exist")
  expect_error(diags(mat2,which=-5),"diagonal does not exist")
  expect_error(diags(mat1,which=4),"diagonal does not exist")
})

test_that("fact2num() messages",{
  expect_error(fact2num(0:5),"purpose")
  expect_error(fact2num(data.frame(x=0:5)),"purpose")
  expect_error(fact2num(factor(c("A","B","C"))),"aborted")
})

test_that("filterD() messages",{
  expect_error(filterD(0:5))
  expect_error(filterD(matrix(0:5,ncol=2)))
  expect_warning(filterD(iris,Species=="DEREK"),"resultant data.frame")
})  

test_that("Subset() messages",{
  expect_error(Subset(0:5),"with data.frames")
  expect_error(Subset(matrix(0:5,ncol=2)),"with data.frames")
  expect_warning(Subset(iris,Species=="DEREK"),"resultant data.frame")
})  

test_that("fishR() messages",{
  expect_error(fishR("Derek"),"should be one of")
})

test_that("geomean() / geosd() messages",{
  ## Bad data types
  expect_error(geomean(LETTERS),"must be a numeric vector")
  expect_error(geosd(LETTERS),"must be a numeric vector")
  expect_error(geomean(c(TRUE,FALSE)),"must be a numeric vector")
  expect_error(geosd(c(TRUE,FALSE)),"must be a numeric vector")
  expect_error(geomean(data.frame(x=1:3)),"must be a vector")
  expect_error(geosd(data.frame(x=1:3)),"must be a vector")
  ## Bad values
  expect_error(geomean(c(-1,1:3)),"all positive values")
  expect_error(geosd(c(-1,1:3)),"all positive values")
  expect_error(geomean(c(0,1:3)),"all positive values")
  expect_error(geosd(c(0,1:3)),"all positive values")
  expect_error(geomean(c(NA,1:3)),"missing value")
  expect_error(geosd(c(NA,1:3)),"missing value")
  ## Handling Negatives or Zeroes
  expect_warning(geomean(c(-1,1:3),zneg.rm=TRUE),"non-positive values were ignored")
  expect_warning(geosd(c(-1,1:3),zneg.rm=TRUE),"non-positive values were ignored")
  expect_warning(geomean(c(0,1:3),zneg.rm=TRUE),"non-positive values were ignored")
  expect_warning(geosd(c(0,1:3),zneg.rm=TRUE),"non-positive values were ignored")
})

test_that("headtail() messages",{
  expect_error(headtail(1:10),"matrix")
  expect_error(headtail(iris,n=c(1,2)),"single number")
})  

test_that("hoCoef() messages",{
  ## fit some linear regression results
  data(Mirex)
  lm1 <- lm(mirex~weight,data=Mirex)
  lm2 <- lm(mirex~weight+year,data=Mirex)
  # bad alt=
  expect_error(hoCoef(lm1,term=2,bo=0.1,alt="derek"),"should be one of")
  # bad term
  expect_error(hoCoef(lm1,term=-1,bo=0.1),"positive")
  expect_error(hoCoef(lm1,term=5,bo=0.1),"greater")
  expect_error(hoCoef(lm2,term=5,bo=0.1),"greater")
  
  ## fit some non-linear regression results
  data(Ecoli)
  fnx <- function(days,B1,B2,B3) {
    if (length(B1) > 1) {
      B2 <- B1[2]
      B3 <- B1[3]
      B1 <- B1[1]
    }
    B1/(1+exp(B2+B3*days))
  }
  nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,start=list(B1=6,B2=7.2,B3=-1.45))
  # bad model type
  expect_error(hoCoef(nl1,term=-1,bo=0.1),"lm")
})

test_that("lagratio() messages",{
  ## check error messages
  expect_error(lagratio(0:5),"zeroes")
  expect_error(lagratio(.leap.seconds),"POSIXt")
  expect_error(lagratio(1:5,direction="derek"),"one of")
  expect_error(lagratio(1:5,recursion=-1),"recursion")
})
test_that("logbtcf() messages",{
  ## toy data
  df <- data.frame(y=rlnorm(10),x=rlnorm(10))
  df$logey <- log(df$y)
  df$logex <- log(df$x)
  ## only works with lm
  glme <- glm(logey~logex,data=df)
  expect_error(logbtcf(glme),"must be from lm()")
})

test_that("oddeven() messages",{
  expect_error(is.odd("A"),"numeric")
  expect_error(is.even("A"),"numeric")
  expect_error(is.odd(matrix(1:5)),"vector")
  expect_error(is.even(matrix(1:5)),"vector")
})

test_that("perc() messages",{
  expect_error(perc("A"),"numeric")
  expect_warning(perc(1:4,c(1,2)),"first value")
})

test_that("pcumsum()/rcumsum() messages",{
  ## check error messages -- wrong type
  expect_error(pcumsum(letters),"numeric")
  expect_error(rcumsum(letters),"numeric")
  ## check error messages -- not 1-dimensional
  tmp <- data.frame(x=sample(1:5,100,replace=TRUE),
                    y=sample(1:5,100,replace=TRUE))
  tbl <- table(tmp$x,tmp$y)
  expect_error(pcumsum(tbl),"1-dimensional")
  expect_error(rcumsum(tbl),"1-dimensional")
  tbl <- as.data.frame(table(tmp$x))
  expect_error(pcumsum(tbl),"1-dimensional")
  expect_error(rcumsum(tbl),"1-dimensional")
  mat <- matrix(1:6,nrow=2)
  expect_error(pcumsum(mat),"1-dimensional")
  expect_error(rcumsum(mat),"1-dimensional")
})

test_that("se() messages",{
  expect_error(se(letters),"numeric")
  expect_error(se(data.frame(x=1:5)),"vector")
  expect_error(se(matrix(1:6,ncol=2)),"vector")
})

test_that("validn() messages",{
  expect_error(validn(data.frame(x=1:5,y=2:6)),"cannot be a data.frame")
  expect_error(validn(matrix(1:6,ncol=2)),"cannot be a matrix")
})
