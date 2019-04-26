## Test Messages ----
test_that("chooseColors() messages",{
  ## check error messages
  expect_error(chooseColors("Derek"),"should be one of")
  expect_error(chooseColors(num=0),"positive")
})

test_that("col2rgbt() messages",{
  expect_error(col2rgbt("black",-1),"must be greater than 0")
  expect_error(col2rgbt("black",0),"must be greater than 0")
  expect_error(col2rgbt(c("black","blue","red"),c(2,3)),
               "must be 1 or same as length")
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
  ## Handling Negatives or Zeros
  expect_warning(geomean(c(-1,1:3),zneg.rm=TRUE),
                 "non-positive values were ignored")
  expect_warning(geosd(c(-1,1:3),zneg.rm=TRUE),
                 "non-positive values were ignored")
  expect_warning(geomean(c(0,1:3),zneg.rm=TRUE),
                 "non-positive values were ignored")
  expect_warning(geosd(c(0,1:3),zneg.rm=TRUE),
                 "non-positive values were ignored")
})

test_that("headtail() messages",{
  expect_error(FSA::headtail(1:10),"matrix")
  expect_error(FSA::headtail(iris,n=c(1,2)),"single number")
})  

test_that("hoCoef() messages",{
  ## fit some linear regression results
  lm1 <- lm(mirex~weight,data=Mirex)
  lm2 <- lm(mirex~weight+year,data=Mirex)
  # bad alt=
  expect_error(hoCoef(lm1,term=2,bo=0.1,alt="derek"),"should be one of")
  # bad term
  expect_error(hoCoef(lm1,term=-1,bo=0.1),"positive")
  expect_error(hoCoef(lm1,term=5,bo=0.1),"greater")
  expect_error(hoCoef(lm2,term=5,bo=0.1),"greater")
  
  ## fit some non-linear regression results
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
  expect_error(lagratio(0:5),"zeros")
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

test_that("rSquared messages",{
  tmp <- data.frame(x=0:5,y=c(0,1.5,3,5,9,15))
  tmp <- nls(y~a*x^b,data=tmp,start=list(a=1,b=2))
  expect_error(rSquared(tmp),"only works with 'lm'")
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


## Test Output Types ----
test_that("capFirst() results",{
  ## simulate data set
  set.seed(345234534)
  dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),
                    tl=round(rnorm(30,1900,300),0))
  dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
  dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),
                    tl=round(rnorm(30,130,50),0))
  dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
  dlb <- data.frame(species=factor(rep(c("LMB"),30)),
                    tl=round(rnorm(30,350,60),0))
  dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
  df <- rbind(dbt,dbg,dlb)
  df$rnd <- runif(nrow(df))
  df$junk <- sample(c("Derek","Hugh","Ogle"),nrow(df),replace=TRUE)
  ## actual tests
  expect_equivalent(levels(factor(capFirst(df$species))),
                    c("Bluefin Tuna","Bluegill","Lmb"))
  expect_equivalent(levels(factor(capFirst(df$species,which="first"))),
                    c("Bluefin tuna","Bluegill","Lmb"))
})

test_that("capFirst() returned classes",{
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

test_that("chooseColors() return values",{
  ## check return values
  n <- 10
  tmp <- chooseColors(num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp2 <- chooseColors(num=n,rev=TRUE)
  expect_equal(length(tmp2),n)
  expect_is(tmp2,"character")
  expect_equal(tmp2,rev(tmp))
  n <- 20
  tmp <- chooseColors("gray",num=n)
  expect_equal(length(tmp),n)
  expect_is(tmp,"character")
  tmp2 <- chooseColors("gray",num=n,rev=TRUE)
  expect_equal(length(tmp2),n)
  expect_is(tmp2,"character")
  expect_equal(tmp2,rev(tmp))
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

test_that("col2rgbt() results",{
  expect_equal(col2rgbt("black",10),rgb(0,0,0,1/10))
  expect_equal(col2rgbt("black",1/10),rgb(0,0,0,1/10))
  expect_equal(col2rgbt("red",10),rgb(1,0,0,1/10))
  expect_equal(col2rgbt("blue",1/10),rgb(0,0,1,1/10))
  expect_equal(col2rgbt("black",1),rgb(0,0,0,1))
})

test_that("diags() results",{
  mat1 <- matrix(seq_len(16),nrow=4)
  colnames(mat1) <- LETTERS[seq_len(ncol(mat1))]
  rownames(mat1) <- seq_len(nrow(mat1))
  mat2 <- matrix(seq_len(20),nrow=4)
  colnames(mat2) <- LETTERS[seq_len(ncol(mat2))]
  rownames(mat2) <- seq_len(nrow(mat2))
  # main diagonal, no labels
  tmp <- diags(mat1)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat1))
  expect_equal(ncol(tmp),1)
  expect_equal(tmp$value,c(1,6,11,16))
  tmp <- diags(mat2)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat2))
  expect_equal(ncol(tmp),1)
  expect_equal(tmp$value,c(1,6,11,16))
  # main diagonal, labels
  tmp <- diags(mat1,incl.labels="row")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat1))
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(1,6,11,16))
  expect_equal(tmp$label,seq_len(4))
  tmp <- diags(mat1,incl.labels="column")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat1))
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(1,6,11,16))
  expect_equal(tmp$label,LETTERS[seq_len(4)])
  tmp <- diags(mat2,incl.labels="row")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat2))
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(1,6,11,16))
  expect_equal(tmp$label,seq_len(4))
  tmp <- diags(mat2,incl.labels="column")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat2))
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(1,6,11,16))
  expect_equal(tmp$label,LETTERS[seq_len(4)])
  ## Off diagonal, no labels
  tmp <- diags(mat1,which=-1)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat1)-1)
  expect_equal(ncol(tmp),1)
  expect_equal(tmp$value,c(5,10,15))
  tmp <- diags(mat2,which=-1)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat2))
  expect_equal(ncol(tmp),1)
  expect_equal(tmp$value,c(5,10,15,20))
  tmp <- diags(mat1,which=3)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),1)
  expect_equal(tmp$value,4)
  tmp <- diags(mat2,which=-3)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),2)
  expect_equal(ncol(tmp),1)
  expect_equal(tmp$value,c(13,18))
  ## Off diagonal, with labels
  tmp <- diags(mat1,which=1,incl.labels="row")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat1)-1)
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(2,7,12))
  expect_equal(tmp$label,2:4)
  tmp <- diags(mat2,which=-2,incl.labels="col")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(9,14,19))
  expect_equal(tmp$label,c("C","D","E"))
  ## Make sure data types are OK
  tmp <- diags(mat1,incl.labels="row")
  expect_is(tmp$value,"integer")
  expect_is(tmp$label,"numeric")
  tmp <- diags(mat1,incl.labels="col")
  expect_is(tmp$value,"integer")
  expect_is(tmp$label,"character")
  mat3 <- matrix(LETTERS[seq_len(24)],nrow=3)
  rownames(mat3) <- letters[seq_len(nrow(mat3))]
  colnames(mat3) <- seq_len(ncol(mat3))
  tmp <- diags(mat3,incl.labels="row")
  expect_is(tmp$value,"character")
  expect_is(tmp$label,"character")
  tmp <- diags(mat3,incl.labels="col")
  expect_is(tmp$value,"character")
  expect_is(tmp$label,"numeric")
})

test_that("fact2num() results",{
  nums <- c(1,2,6,9,3)
  tmp <- fact2num(factor(nums))
  expect_equal(tmp,nums)
  expect_is(tmp,"numeric")
  expect_true(is.vector(tmp))
})

test_that("filterD() results",{
  # limit to two groups
  grp <- c("setosa","versicolor")
  tmp <- filterD(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),100)
  # limit to one group
  grp <- c("versicolor")
  tmp <- filterD(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  # make sure that levels are not reordered
  iris$Species1 <- factor(iris$Species,levels=c("virginica","versicolor","setosa"))
  grp <- c("setosa","versicolor")
  tmp <- filterD(iris,Species1 %in% grp)
  expect_equal(levels(tmp$Species1),rev(grp))
  # check usage of except
  tmp <- filterD(iris,Species1 %in% grp,except="Species")
  expect_equal(levels(tmp$Species1),rev(grp))
  expect_equal(levels(tmp$Species),c("setosa","versicolor","virginica"))
})  

test_that("Subset() results",{
  # limit to two groups
  grp <- c("setosa","versicolor")
  tmp <- Subset(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),100)
  # limit to one group
  grp <- c("versicolor")
  tmp <- Subset(iris,Species %in% grp)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  # does Subset still work if columns are selected
  tmp <- Subset(iris,Species %in% grp,select=4:5)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  expect_equal(ncol(tmp),2)
  # does Subset still work if rows are not renumbered
  tmp <- Subset(iris,Species %in% grp,resetRownames=FALSE)
  expect_equal(levels(tmp$Species),grp)
  expect_equal(nrow(tmp),50)
  expect_equal(rownames(tmp),as.character(51:100))
  # make sure that levels are not reordered
  iris$Species1 <- factor(iris$Species,levels=c("virginica","versicolor","setosa"))
  grp <- c("setosa","versicolor")
  tmp <- Subset(iris,Species1 %in% grp)
  expect_equal(levels(tmp$Species1),rev(grp))
})  

test_that("fishR() return values",{
  expect_equal(fishR(),"http://derekogle.com/fishR")
  expect_equal(fishR("IFAR"),"http://derekogle.com/IFAR")
  expect_equal(fishR("general"),"http://derekogle.com/fishR/examples")
  expect_equal(fishR("AIFFD"),"http://derekogle.com/aiffd2007")
  expect_equal(fishR("posts"),"http://derekogle.com/fishR/blog")
  expect_equal(fishR("books"),"http://derekogle.com/fishR/examples")
  expect_equal(fishR("news"),"http://derekogle.com/fishR/blog")
})

test_that("geomean() / geosd() results",{
  ## Geometric mean
  # match wikipedia example
  expect_equivalent(geomean(c(1/32,1,4)),1/2)
  # match http://www.thinkingapplied.com/means_folder/deceptive_means.htm
  tmp <- c(1.0978,1.1174,1.1341,0.9712,1.1513,1.2286,1.0930,0.9915,1.0150)
  tmp2 <- c(NA,tmp)
  expect_equivalent(round(geomean(tmp),4),1.0861)
  expect_equivalent(round(geosd(tmp),4),1.0795)
  # match geometric.mean in psych package
  if (require(psych)) {
    expect_equivalent(geomean(tmp),psych::geometric.mean(tmp))
    expect_equivalent(geomean(tmp2,na.rm=TRUE),psych::geometric.mean(tmp2))
  }
  if (require(DescTools)) {
    expect_equivalent(geomean(tmp),DescTools::Gmean(tmp))
    expect_equivalent(geomean(tmp2,na.rm=TRUE),DescTools::Gmean(tmp2,na.rm=TRUE))
    expect_equivalent(geosd(tmp),DescTools::Gsd(tmp))
    expect_equivalent(geosd(tmp2,na.rm=TRUE),DescTools::Gsd(tmp2,na.rm=TRUE))
  }
})

test_that("headtail() return values",{
  n <- 3
  tmp <- FSA::headtail(iris)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(iris))
  expect_equal(names(tmp),names(iris))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,rbind(head(iris,n=n),tail(iris,n=n)))
  ## check more rows
  n <- 6
  tmp <- FSA::headtail(iris,n=n)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(iris))
  expect_equal(names(tmp),names(iris))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,rbind(head(iris,n=n),tail(iris,n=n)))
  ## check of restricted columns
  n <- 3
  cols <- 2:3
  tmp <- FSA::headtail(iris,which=cols)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),length(cols))
  expect_equal(names(tmp),names(iris)[cols])
  expect_is(tmp,"data.frame")
  
  ## check for matrix
  miris <- as.matrix(iris[,seq_len(4)])
  tmp <- FSA::headtail(miris)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(miris))
  expect_equal(names(tmp),names(miris))
  expect_is(tmp,"matrix")
  expect_equivalent(tmp,rbind(head(miris,n=n),tail(miris,n=n)))
  # check of addrownums
  tmp <- FSA::headtail(miris,addrownums=FALSE)
  expect_true(is.null(rownames(tmp)))
  
  ## check how it handles tbl_df object
  if (require(dplyr)) {
    iris2 <- tbl_df(iris)
    tmp <- FSA::headtail(iris2,n=15)
    expect_is(tmp,"data.frame")
  }
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

test_that("logbtcf() output",{
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
})

test_that("oddeven() return values",{
  expect_true(is.odd(1))
  expect_false(is.odd(2))
  expect_true(is.even(2))
  expect_false(is.even(1))
  expect_equal(is.odd(1:4),c(TRUE,FALSE,TRUE,FALSE))
  expect_equal(is.even(1:4),c(FALSE,TRUE,FALSE,TRUE))
  expect_is(is.odd(1:4),"logical")
})

test_that("perc() return values",{
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

test_that("rSquared() return values",{
  tmp <- lm(y~x,data=data.frame(x=0:5,y=c(0,1.5,3,5,9,15)))
  expect_equal(round(summary(tmp)$r.squared,9),
               rSquared(tmp,digits=9))
})

test_that("pcumsum()/rcumsum() return values",{
  tmp <- 1:3
  expect_equal(pcumsum(tmp),c(0,1,3))
  expect_equal(rcumsum(tmp),c(6,5,3))
})

test_that("se() return values",{
  ## If an NA value occurs then return NA if na.rm=FALSE
  expect_true(is.na(se(c(1,2,NA),na.rm=FALSE)))
  ## check results
  tmp <- seq_len(10)
  expect_equal(se(tmp),sd(tmp)/sqrt(length(tmp)))
})

test_that("validn() return values",{
  expect_equal(validn(c(1,7,2,4,3,10,NA)),6)
  expect_equal(validn(c("Derek","Hugh","Ogle","Santa","Claus","Nick",NA,NA)),6)
  expect_equal(validn(factor(c("Derek","Hugh","Ogle","Santa","Claus",
                               "Nick",NA,NA))),6)
  expect_equal(validn(c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,NA,NA)),6)
})


## Validate Results ----

