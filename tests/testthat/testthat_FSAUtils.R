## Test Messages ----
test_that("col2rgbt() messages",{
  expect_error(col2rgbt("black",-1),"must be greater than 0")
  expect_error(col2rgbt("black",0),"must be greater than 0")
  expect_error(col2rgbt(c("black","blue","red"),c(2,3)),
               "must be 1 or same as length")
})

test_that("fact2num() messages",{
  expect_error(fact2num(0:5),"purpose")
  expect_error(fact2num(data.frame(x=0:5)),"purpose")
  expect_error(fact2num(factor(c("A","B","C"))),"aborted")
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

test_that("repeatedRows2Keep() messages",{
  tmp <- data.frame(x=0:5,y=c(0,1.5,3,5,9,15))
  expect_error(repeatedRows2Keep(tmp,keep="derek"),"should be one of")
  expect_error(repeatedRows2Keep(tmp,cols2use="derek"),"None of columns")
  expect_error(repeatedRows2Keep(tmp,cols2ignore="derek"),"None of columns")
  expect_error(repeatedRows2Keep(tmp,cols2use="V1",cols2ignore="V2"),
               "Cannot use both")
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
  expect_equal(levels(factor(capFirst(df$species))),
               c("Bluefin Tuna","Bluegill","Lmb"))
  expect_equal(levels(factor(capFirst(df$species,which="first"))),
               c("Bluefin tuna","Bluegill","Lmb"))
})

test_that("capFirst() returned classes",{
  ## simulate vector of names
  vec <- c("Derek Ogle","derek ogle","Derek ogle","derek Ogle","DEREK OGLE",NA)
  fvec <- factor(vec)
  ## first example of non-factor vector
  vec1 <- capFirst(vec)
  expect_equal(class(vec),class(vec1))
  expect_equal(class(vec1),"character")
  ## second example of factored vector
  fvec1 <- capFirst(fvec)
  expect_equal(class(fvec),class(fvec1))
  expect_equal(class(fvec1),"factor")
})

test_that("col2rgbt() results",{
  expect_equal(col2rgbt("black",10),rgb(0,0,0,1/10))
  expect_equal(col2rgbt("black",1/10),rgb(0,0,0,1/10))
  expect_equal(col2rgbt("red",10),rgb(1,0,0,1/10))
  expect_equal(col2rgbt("blue",1/10),rgb(0,0,1,1/10))
  expect_equal(col2rgbt("black",1),rgb(0,0,0,1))
})

test_that("fact2num() results",{
  nums <- c(1,2,6,9,3)
  tmp <- fact2num(factor(nums))
  expect_equal(tmp,nums)
  expect_equal(class(tmp),"numeric")
  expect_true(is.vector(tmp))
})

test_that("fishR() return values",{
  expect_equal(fishR(open=FALSE),
               "https://fishr-core-team.github.io/fishR/")
  expect_equal(fishR("posts",open=FALSE),
               "https://fishr-core-team.github.io/fishR/blog/")
  expect_equal(fishR("books",open=FALSE),
               "https://fishr-core-team.github.io/fishR/pages/books.html")
  expect_equal(fishR("IFAR",open=FALSE),
               "https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r")
  expect_equal(fishR("AIFFD",open=FALSE),
               "https://fishr-core-team.github.io/fishR/pages/books.html#analysis-and-interpretation-of-freshwater-fisheries-data-i")
  expect_equal(fishR("packages",open=FALSE),
               "https://fishr-core-team.github.io/fishR/pages/packages.html")
  expect_equal(fishR("data",open=FALSE),
               "https://fishr-core-team.github.io/fishR/pages/data_fishR_alpha.html")
})

test_that("geomean() / geosd() results",{
  ## Geometric mean
  # match wikipedia example
  expect_equal(geomean(c(1/32,1,4)),1/2)
  # match http://www.thinkingapplied.com/means_folder/deceptive_means.htm
  tmp <- c(1.0978,1.1174,1.1341,0.9712,1.1513,1.2286,1.0930,0.9915,1.0150)
  tmp2 <- c(NA,tmp)
  expect_equal(round(geomean(tmp),4),1.0861)
  expect_equal(round(geosd(tmp),4),1.0795)
  # match geometric.mean in psych package
  #expect_equal(geomean(tmp),psych::geometric.mean(tmp))
  #expect_equal(geomean(tmp2,na.rm=TRUE),psych::geometric.mean(tmp2))
  #expect_equal(geomean(tmp),DescTools::Gmean(tmp))
  #expect_equal(geomean(tmp2,na.rm=TRUE),DescTools::Gmean(tmp2,na.rm=TRUE))
  #expect_equal(geosd(tmp),DescTools::Gsd(tmp))
  #expect_equal(geosd(tmp2,na.rm=TRUE),DescTools::Gsd(tmp2,na.rm=TRUE))
})

test_that("headtail() return values",{
  n <- 3
  tmp <- FSA::headtail(iris)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(iris))
  expect_equal(names(tmp),names(iris))
  expect_equal(class(tmp),"data.frame")
  expect_equal(tmp,rbind(head(iris,n=n),tail(iris,n=n)))
  ## check more rows
  n <- 6
  tmp <- FSA::headtail(iris,n=n)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(iris))
  expect_equal(names(tmp),names(iris))
  expect_equal(class(tmp),"data.frame")
  expect_equal(tmp,rbind(head(iris,n=n),tail(iris,n=n)))
  ## check of restricted columns
  n <- 3
  cols <- 2:3
  tmp <- FSA::headtail(iris,which=cols)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),length(cols))
  expect_equal(names(tmp),names(iris)[cols])
  expect_equal(class(tmp),"data.frame")
  
  ## check for matrix
  miris <- as.matrix(iris[,seq_len(4)])
  tmp <- FSA::headtail(miris)
  expect_equal(nrow(tmp),2*n)
  expect_equal(ncol(tmp),ncol(miris))
  expect_equal(names(tmp),names(miris))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(tmp,rbind(head(miris,n=n),tail(miris,n=n)),ignore_attr=TRUE)
  # check of addrownums
  tmp <- FSA::headtail(miris,addrownums=FALSE)
  expect_true(is.null(rownames(tmp)))
  
  ## check how it handles tbl_df object
#  iris2 <- tibble::as_tibble(iris)
#  tmp <- FSA::headtail(iris2,n=15)
#  expect_equal(class(tmp),"data.frame")
})

test_that("peek() return values",{
  n <- 20
  tmp <- FSA::peek(iris)
  expect_equal(nrow(tmp),n)
  expect_equal(ncol(tmp),ncol(iris))
  expect_equal(names(tmp),names(iris))
  expect_equal(class(tmp),"data.frame")
  ## check more rows
  n <- 10
  tmp <- FSA::peek(iris,n=n)
  expect_equal(nrow(tmp),n)
  expect_equal(ncol(tmp),ncol(iris))
  expect_equal(names(tmp),names(iris))
  expect_equal(class(tmp),"data.frame")
  ## check of restricted columns
  n <- 20
  cols <- 2:3
  tmp <- FSA::peek(iris,which=cols)
  expect_equal(nrow(tmp),n)
  expect_equal(ncol(tmp),length(cols))
  expect_equal(names(tmp),names(iris)[cols])
  expect_equal(class(tmp),"data.frame")
  
  ## check for matrix
  miris <- as.matrix(iris[,seq_len(4)])
  tmp <- FSA::peek(miris)
  expect_equal(nrow(tmp),n)
  expect_equal(ncol(tmp),ncol(miris))
  expect_equal(names(tmp),names(miris))
  expect_equal(class(tmp),c("matrix","array"))
  # check of addrownums
  tmp <- FSA::peek(miris,addrownums=FALSE)
  expect_true(is.null(rownames(tmp)))
  
  ## check how it handles tbl_df object
#  iris2 <- tibble::as_tibble(iris)
#  tmp <- FSA::peek(iris2,n=15)
#  expect_equal(class(tmp),"data.frame")
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
  expect_equal(class(cfe),"numeric")
  expect_equal(class(cf10),"numeric")
  
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
  expect_equal(class(is.odd(1:4)),"logical")
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

test_that("repeatedRows2Keep() return values",{
  test1 <- data.frame(ID=1:10,
                      KEEP=c("First","Last","Both","Both","Both",
                             "Both","First","Neither","Last","Both"),
                      V1=c("a","a","a","B","b","B","A","A","A","a"),
                      V2=c("a","a","A","B","B","b","A","A","A","a"),
                      stringsAsFactors=FALSE)
  keepFirst <- repeatedRows2Keep(test1,cols2ignore=1:2)
  keepLast <- repeatedRows2Keep(test1,cols2use=3:4,keep="last")
  expect_equal(class(keepFirst),"logical")
  expect_equal(class(keepLast),"logical")
  tmp <- droplevels(subset(test1,keepFirst))
  expect_equal(tmp$ID,c(1,3:7,10))
  expect_true(all(tmp$KEEP %in% c("First","Both")))
  tmp <- droplevels(subset(test1,keepLast))
  expect_equal(tmp$ID,c(2:6,9,10))
  expect_true(all(tmp$KEEP %in% c("Last","Both")))
  
  test2 <- data.frame(ID=1:10,
                      KEEP=c("Both","Both","Both","Both","Both",
                             "Both","First","Neither","Neither","Last"),
                      V1=c("a","b","a","B","b","B","A","A","A","A"),
                      V2=c("a","b","A","B","B","b","A","A","A","A"),
                      stringsAsFactors=FALSE)
  keepFirst <- repeatedRows2Keep(test2,cols2ignore=1:2)
  keepLast <- repeatedRows2Keep(test2,cols2use=3:4,keep="last")
  tmp <- droplevels(subset(test2,keepFirst))
  expect_equal(tmp$ID,c(1:7))
  expect_true(all(tmp$KEEP %in% c("First","Both")))
  tmp <- droplevels(subset(test2,keepLast))
  expect_equal(tmp$ID,c(1:6,10))
  expect_true(all(tmp$KEEP %in% c("Last","Both")))
  
  test3 <- data.frame(ID=1:10,
                      KEEP=c("First","Neither","Last","First","Neither",
                             "Last","First","Neither","Neither","Last"),
                      V1=c("a","a","a","B","B","B","A","A","A","A"),
                      V2=c("a","a","a","B","B","B","A","A","A","A"),
                      stringsAsFactors=FALSE)
  keepFirst <- repeatedRows2Keep(test3,cols2ignore=1:2)
  keepLast <- repeatedRows2Keep(test3,cols2use=3:4,keep="last")
  tmp <- droplevels(subset(test3,keepFirst))
  expect_equal(tmp$ID,c(1,4,7))
  expect_true(all(tmp$KEEP %in% c("First","Both")))
  tmp <- droplevels(subset(test3,keepLast))
  expect_equal(tmp$ID,c(3,6,10))
  expect_true(all(tmp$KEEP %in% c("Last","Both")))
  
  ## Use just one column
  keepFirst <- repeatedRows2Keep(test3,cols2ignore=1:3)
  keepLast <- repeatedRows2Keep(test3,cols2use=3:4,keep="last")
  tmp <- droplevels(subset(test3,keepFirst))
  expect_equal(tmp$ID,c(1,4,7))
  expect_true(all(tmp$KEEP %in% c("First","Both")))
  tmp <- droplevels(subset(test3,keepLast))
  expect_equal(tmp$ID,c(3,6,10))
  expect_true(all(tmp$KEEP %in% c("Last","Both")))
  
  ## None to remove
  test4 <- data.frame(ID=1:10,KEEP=rep("Both",10),
                      V1=LETTERS[1:10],V2=LETTERS[2:11],
                      stringsAsFactors=FALSE)
  keepFirst <- repeatedRows2Keep(test4,cols2ignore=1:2)
  keepLast <- repeatedRows2Keep(test4,cols2use=3:4,keep="last")
  tmp <- droplevels(subset(test4,keepFirst))
  expect_equal(tmp$ID,1:10)
  expect_true(all(tmp$KEEP %in% c("First","Both")))
  tmp <- droplevels(subset(test4,keepLast))
  expect_equal(tmp$ID,1:10)
  expect_true(all(tmp$KEEP %in% c("Last","Both")))
  
  ## Factor variables
  test5 <- data.frame(ID=1:10,
                      KEEP=c("Both","Both","Both","Both","Both",
                             "Both","First","Neither","Neither","Last"),
                      V1=c("a","b","a","B","b","B","A","A","A","A"),
                      V2=c("a","b","A","B","B","b","A","A","A","A"),
                      stringsAsFactors=FALSE)
  test5$V2 <- factor(test5$V2)
  keepFirst <- repeatedRows2Keep(test5,cols2ignore=1:2)
  keepLast <- repeatedRows2Keep(test5,cols2use=3:4,keep="last")
  tmp <- droplevels(subset(test5,keepFirst))
  expect_equal(tmp$ID,c(1:7))
  expect_true(all(tmp$KEEP %in% c("First","Both")))
  tmp <- droplevels(subset(test5,keepLast))
  expect_equal(tmp$ID,c(1:6,10))
  expect_true(all(tmp$KEEP %in% c("Last","Both")))
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

