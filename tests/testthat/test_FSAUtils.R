context("Tests for FSA utilities")

# ############################################################
# capFirst
# ############################################################
test_that("capFirst() capitalizations are correct",{
  ## simulate data set
  set.seed(345234534)
  dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
  dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
  dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
  dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
  dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
  dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
  df <- rbind(dbt,dbg,dlb)
  df$rnd <- runif(nrow(df))
  df$junk <- sample(c("Derek","Hugh","Ogle"),nrow(df),replace=TRUE)
  ## actual tests
  expect_equivalent(levels(factor(capFirst(df$species))),c("Bluefin Tuna","Bluegill","Lmb"))
  expect_equivalent(levels(factor(capFirst(df$species,which="first"))),c("Bluefin tuna","Bluegill","Lmb"))
})

test_that("capFirst() returned classes are correct",{
  ## simulate vector of names
  vec <- c("Derek Ogle","derek ogle","Derek ogle","derek Ogle","DEREK OGLE")
  fvec <- factor(vec)
  ## first example of non-factor vector
  vec1 <- capFirst(vec)
  expect_equivalent(class(vec),class(vec1))
  expect_equivalent(class(vec1),"character")
  ## second example of factored vector
  fvec1 <- capFirst(fvec)
  expect_equivalent(class(fvec),class(fvec1))
  expect_equivalent(class(fvec1),"factor")
})


# ############################################################
# chnagesPos
# ############################################################
test_that("changesPos() error messages",{
  ## check error messages
  expect_error(changesPos(numeric(0)),"length")
  expect_error(changesPos(1,include.first=FALSE),"include.first")
  expect_error(changesPos(matrix(1:4)),"vector")
  expect_error(changesPos(data.frame(x=1:4)),"vector")
})

test_that("changesPos() calculations",{
  expect_equal(changesPos(1:4),1:4)
  expect_equal(changesPos(c(1:2,1:2)),1:4)
  expect_equal(changesPos(c(1,2,2,1)),c(1,2,4))
  expect_equal(changesPos(c(1,2,2,1),include.first=FALSE),c(2,4))
  expect_equal(changesPos(c(1,1,1,1)),1)
  expect_equal(changesPos(c(1,1,1,1),include.first=FALSE),numeric(0))
})  


# ############################################################
# chooseColors
# ############################################################
test_that("chooseColors() error messages and return values",{
  ## check error messages
  expect_error(chooseColors("Derek"),"should be one of")
  expect_error(chooseColors(num=0),"positive")
  ## check return values
  n <- 10
  tmp <- chooseColors(num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  n <- 20
  tmp <- chooseColors("gray",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  ## check each
  n <- 10
  tmp <- chooseColors("gray",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp <- chooseColors("rich",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp <- chooseColors("cm",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp <- chooseColors("heat",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp <- chooseColors("jet",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp <- chooseColors("rainbow",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp <- chooseColors("topo",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp <- chooseColors("terrain",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp <- chooseColors("default",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"integer")
})


# ############################################################
# fact2num
# ############################################################
test_that("fact2num() error messages and results",{
  ## check error messages
  expect_error(fact2num(0:5),"purpose")
  expect_error(fact2num(data.frame(x=0:5)),"purpose")
  expect_error(fact2num(factor(c("A","B","C"))),"aborted")
  ## check results
  nums <- c(1,2,6,9,3)
  tmp <- fact2num(factor(nums))
  expect_equal(tmp,nums)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
})


# ############################################################
# filterD
# ############################################################
test_that("filterD() and Subset() error messages and results",{
  ## check error messages
  expect_error(filterD(0:5))
  expect_error(Subset(0:5),"with data.frames")
  expect_error(filterD(matrix(0:5,ncol=2)))
  expect_error(Subset(matrix(0:5,ncol=2)),"with data.frames")
  expect_warning(Subset(iris,Species=="DEREK"),"resultant data.frame")
  expect_warning(filterD(iris,Species=="DEREK"),"resultant data.frame")
  
  ## check results
  # limit to two groups
  grp <- c("setosa","versicolor")
  tmp <- Subset(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),100)
  tmp <- filterD(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),100)
  # limit to one group
  grp <- c("versicolor")
  tmp <- Subset(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  tmp <- filterD(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  # does Subset still work if columns are selected
  tmp <- Subset(iris,Species %in% grp,select=4:5)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  expect_equal(ncol(tmp),2)
  # does Subset still work if rows are not renumbered
  tmp <- Subset(iris,Species %in% grp,restRownames=FALSE)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  expect_equal(rownames(tmp),as.character(1:50))
})  


# ############################################################
# fishR
# ############################################################
test_that("fishR() error messages and return values",{
  ## check error messages
  expect_error(fishR("Derek"),"should be one of")
  ## check return values
  tmp <- fishR()
  expect_equal(tmp,"http://fishr.wordpress.com")
  tmp <- fishR("IFAR")
  expect_equal(tmp,"http://fishr.wordpress.com/ifar/")
  tmp <- fishR("general")
  expect_equal(tmp,"http://fishr.wordpress.com/vignettes/")
  tmp <- fishR("AIFFD")
  expect_equal(tmp,"http://fishr.wordpress.com/books/aiffd/")
  tmp <- fishR("posts")
  expect_equal(tmp,"http://fishr.wordpress.com/news/")
  tmp <- fishR("books")
  expect_equal(tmp,"http://fishr.wordpress.com/books/")
  tmp <- fishR("news")
  expect_equal(tmp,"http://fishr.wordpress.com/news/")
})


# ############################################################
# headtail
# ############################################################
test_that("headtail() error messages and return values",{
  ## check error messages
  expect_error(headtail(1:10),"matrix")
  expect_error(headtail(iris,n=c(1,2)),"single number")
  ## check of default values
  n <- 3
  tmp <- headtail(iris)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(iris))
  expect_equal(names(tmp),names(iris))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,rbind(head(iris,n=n),tail(iris,n=n)))
  ## check more rows
  n <- 6
  tmp <- headtail(iris,n=n)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(iris))
  expect_equal(names(tmp),names(iris))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,rbind(head(iris,n=n),tail(iris,n=n)))
  ## check of restricted columns
  n <- 3
  cols <- 2:3
  tmp <- headtail(iris,which=cols)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),length(cols))
  expect_equal(names(tmp),names(iris)[cols])
  expect_is(tmp,"data.frame")
  
  ## check for matrix
  miris <- as.matrix(iris[,1:4])
  tmp <- headtail(miris)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(miris))
  expect_equal(names(tmp),names(miris))
  expect_is(tmp,"matrix")
  expect_equivalent(tmp,rbind(head(miris,n=n),tail(miris,n=n)))
  # check of addrownums
  tmp <- headtail(miris,addrownums=FALSE)
  expect_true(is.null(rownames(tmp)))
  
  ## check how it handles tbl_df object
  if (require(dplyr)) {
    iris2 <- tbl_df(iris)
    tmp <- headtail(iris2,n=15)
    expect_is(tmp,"data.frame")
  }
})  


# ############################################################
# hoCoef
# ############################################################
test_that("hoCoef() error messages and return values",{
  ## fit some linear regression results
  data(Mirex)
  lm1 <- lm(mirex~weight,data=Mirex)
  lm2 <- lm(mirex~weight+year,data=Mirex)
  ## check error messages
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



# ############################################################
# lagratio
# ############################################################
test_that("lagratio() error messages",{
  ## check error messages
  expect_error(lagratio(0:5),"zeroes")
  expect_error(lagratio(.leap.seconds),"POSIXt")
  expect_error(lagratio(1:5,direction="derek"),"one of")
  expect_error(lagratio(1:5,recursion=-1),"recursion")
})

test_that("lagratio() calculations",{
  ## check calculations where latter is divided by former, no recursion
  res1 <- (2:10)/(1:9)
  res2 <- (3:10)/(1:8)
  res3 <- (4:10)/(1:7)
  expect_equal(lagratio(1:10,1),res1)
  expect_equal(lagratio(1:10,2),res2)
  expect_equal(lagratio(1:10,3),res3)
  ## check calculations where latter is divided by former, with one level of recursion
  res1r <- res1[2:length(res1)]/res1[1:(length(res1)-1)]
  res2r <- res2[3:length(res2)]/res2[1:(length(res2)-2)]
  res3r <- res3[4:length(res3)]/res3[1:(length(res3)-3)]
  expect_equal(lagratio(1:10,1,2),res1r)
  expect_equal(lagratio(1:10,2,2),res2r)
  expect_equal(lagratio(1:10,3,2),res3r)
  
  ## check calculations where former is divided by latter, no recursion
  res1 <- (1:9)/(2:10)
  res2 <- (1:8)/(3:10)
  res3 <- (1:7)/(4:10)
  expect_equal(lagratio(1:10,1,direction="forward"),res1)
  expect_equal(lagratio(1:10,2,direction="forward"),res2)
  expect_equal(lagratio(1:10,3,direction="forward"),res3)
  ## check calculations where latter is divided by former, with one level of recursion
  res1r <- res1[1:(length(res1)-1)]/res1[2:length(res1)]
  res2r <- res2[1:(length(res2)-2)]/res2[3:length(res2)]
  res3r <- res3[1:(length(res3)-3)]/res3[4:length(res3)]
  expect_equal(lagratio(1:10,1,2,direction="forward"),res1r)
  expect_equal(lagratio(1:10,2,2,direction="forward"),res2r)
  expect_equal(lagratio(1:10,3,2,direction="forward"),res3r)
})


# ############################################################
# logbtcf
# ############################################################
test_that("logbtcf() errors and output",{
  ## toy data
  df <- data.frame(y=rlnorm(10),x=rlnorm(10))
  df$logey <- log(df$y)
  df$log10y <- log10(df$y)
  df$logex <- log(df$x)
  df$log10x <- log10(df$x)
  
  # model and predictions on loge scale
  lme <- lm(logey~logex,data=df)
  cfe <- logbtcf(lme)
  cpe <- cfe*exp(ploge <- predict(lme,data.frame(logex=log(10))))
   
  # model and predictions on log10 scale
  lm10 <- lm(log10y~log10x,data=df)
  cf10 <- logbtcf(lm10,10)
  cp10 <- cf10*(10^(predict(lm10,data.frame(log10x=log10(10)))))
  
  ## Check output type
  expect_is(cfe,"numeric")
  expect_is(cf10,"numeric")
  
  ## Results should be equal
  expect_equal(cfe,cf10)
  expect_equal(cpe,cp10)
  
  ## only works with lm
  glme <- glm(logey~logex,data=df)
  expect_error(logbtcf(glme),"must be from lm()")
})


# ############################################################
# oddeven
# ############################################################
test_that("oddeven() error messages and return values",{
  ## check error messages
  expect_error(is.odd("A"),"numeric")
  expect_error(is.even("A"),"numeric")
  expect_error(is.odd(matrix(1:5)),"vector")
  expect_error(is.even(matrix(1:5)),"vector")
  ## check results
  expect_true(is.odd(1))
  expect_false(is.odd(2))
  expect_true(is.even(2))
  expect_false(is.even(1))
  expect_equal(is.odd(1:4),c(TRUE,FALSE,TRUE,FALSE))
  expect_equal(is.even(1:4),c(FALSE,TRUE,FALSE,TRUE))
  expect_is(is.odd(1:4),"logical")
})


# ############################################################
# perc
# ############################################################
test_that("perc() error messages and return values",{
  ## check error messages
  expect_error(perc("A"),"numeric")
  expect_warning(perc(1:4,c(1,2)),"first value")
  ## check results
  tmp <- c(1:8,NA,NA)
  ## percentages excluding NA values
  expect_equal(perc(tmp,5),50)
  expect_equal(perc(tmp,5,"gt"),37.5)
  expect_equal(perc(tmp,5,"leq"),62.5)
  expect_equal(perc(tmp,5,"lt"),50)
  ## percentages including NA values
  expect_equal(suppressWarnings(perc(tmp,5,na.rm=FALSE)),40)
  expect_equal(suppressWarnings(perc(tmp,5,"gt",na.rm=FALSE)),30)
  expect_equal(suppressWarnings(perc(tmp,5,"leq",na.rm=FALSE)),50)
  expect_equal(suppressWarnings(perc(tmp,5,"lt",na.rm=FALSE)),40)
  ## double check if NAs are in different places in the vector
  tmp <- c(1,NA,2:5,NA,6:8)
  ## percentages excluding NA values
  expect_equal(perc(tmp,5),50)
  expect_equal(perc(tmp,5,"gt"),37.5)
  expect_equal(perc(tmp,5,"leq"),62.5)
  expect_equal(perc(tmp,5,"lt"),50)
  ## percentages including NA values
  expect_equal(suppressWarnings(perc(tmp,5,na.rm=FALSE)),40)
  expect_equal(suppressWarnings(perc(tmp,5,"gt",na.rm=FALSE)),30)
  expect_equal(suppressWarnings(perc(tmp,5,"leq",na.rm=FALSE)),50)
  expect_equal(suppressWarnings(perc(tmp,5,"lt",na.rm=FALSE)),40)
})


# ############################################################
# rcumsum / pcumsum
# ############################################################
test_that("pcumsum()/rcumsum() error messages and return values",{
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
  ## check results
  tmp <- 1:3
  expect_equal(pcumsum(tmp),c(0,1,3))
  expect_equal(rcumsum(tmp),c(6,5,3))
})


# ############################################################
# se
# ############################################################
test_that("se() error messages and return values",{
  ## check error messages
  expect_error(se(letters),"numeric")
  expect_error(se(data.frame(x=1:5)),"vector")
  expect_error(se(matrix(1:6,ncol=2)),"vector")
  ## If an NA value occurs then return NA if na.rm=FALSE
  expect_true(is.na(se(c(1,2,NA),na.rm=FALSE)))
  ## check results
  tmp <- c(1:10)
  expect_equal(se(tmp),sd(tmp)/sqrt(length(tmp)))
})


# ############################################################
# validn
# ############################################################
test_that("validn() error messages and return values",{
  ## check error messages
  expect_error(validn(data.frame(x=1:5)),"vector")
  expect_error(validn(matrix(1:6,ncol=2)),"vector")
  ## check results
  expect_equal(validn(c(1,7,2,4,3,10,NA)),6)
  expect_equal(validn(c("Derek","Hugh","Ogle","Santa","Claus","Nick",NA,NA)),6)
  expect_equal(validn(factor(c("Derek","Hugh","Ogle","Santa","Claus","Nick",NA,NA))),6)
  expect_equal(validn(c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,NA,NA)),6)
})

