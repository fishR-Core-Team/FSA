context("FSA utilities OUTPUT")

test_that("capFirst() results",{
  ## simulate data set
  set.seed(345234534)
  dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),
                    tl=round(rnorm(30,1900,300),0))
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
  mat1 <- matrix(1:16,nrow=4)
  colnames(mat1) <- LETTERS[1:ncol(mat1)]
  rownames(mat1) <- 1:nrow(mat1)
  mat2 <- matrix(1:20,nrow=4)
  colnames(mat2) <- LETTERS[1:ncol(mat2)]
  rownames(mat2) <- 1:nrow(mat2)
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
  expect_equal(tmp$label,1:4)
  tmp <- diags(mat1,incl.labels="column")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat1))
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(1,6,11,16))
  expect_equal(tmp$label,LETTERS[1:4])
  tmp <- diags(mat2,incl.labels="row")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat2))
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(1,6,11,16))
  expect_equal(tmp$label,1:4)
  tmp <- diags(mat2,incl.labels="column")
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),nrow(mat2))
  expect_equal(ncol(tmp),2)
  expect_equal(tmp$value,c(1,6,11,16))
  expect_equal(tmp$label,LETTERS[1:4])
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
  mat3 <- matrix(LETTERS[1:24],nrow=3)
  rownames(mat3) <- letters[1:nrow(mat3)]
  colnames(mat3) <- 1:ncol(mat3)
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
  miris <- as.matrix(iris[,1:4])
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
  tmp <- c(1:10)
  expect_equal(se(tmp),sd(tmp)/sqrt(length(tmp)))
})

test_that("validn() return values",{
  expect_equal(validn(c(1,7,2,4,3,10,NA)),6)
  expect_equal(validn(c("Derek","Hugh","Ogle","Santa","Claus","Nick",NA,NA)),6)
  expect_equal(validn(factor(c("Derek","Hugh","Ogle","Santa","Claus","Nick",NA,NA))),6)
  expect_equal(validn(c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,NA,NA)),6)
})
