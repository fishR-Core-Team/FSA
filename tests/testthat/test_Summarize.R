context("Summmarize() tests")

## Create simple dummy data
d1 <- data.frame(f1=factor(c("A","A","A","B","B","B","C","C","C")),
                 f2=factor(c("a","b","c","a","b","c","a","b","c")),
                 f3=factor(c("A","A","A","B","B","B","C","C",NA)),
                 c1=c("x","x","x","x","x","y","y","y","y"),
                 c2=c("A","A","A","B","B","B","C","C",NA),
                 q1=1:9,q2=11:19,q3=c(1:3,1:3,1,2,2),q4=c(NA,1:8),
                 stringsAsFactors=FALSE)

## Result labels
qnms <- c("n","nvalid","mean","sd","min","Q1","median","Q3","max","percZero")

test_that("Summarize() messages",{
  ## From formula method
  expect_error(Summarize(~f1,data=d1),"only works with a numeric variable")
  expect_error(Summarize(~c1,data=d1),"only works with a numeric variable")
  expect_error(Summarize(f1~1,data=d1),"only works with a numeric variable")
  expect_error(Summarize(c1~1,data=d1),"only works with a numeric variable")
  expect_error(Summarize(q1+q2~f1+f2,data=d1),"more than one variable on the LHS")
  expect_error(Summarize(q1~f1+f2+c1,data=d1),"may contain only one or two factors")
  expect_error(Summarize(q1~f1+f2+c1,data=d1),"may contain only one or two factors")
  expect_error(Summarize(~f1+f2,data=d1),"Must have one variable on LHS of formula")
  expect_error(Summarize(~q1+q2,data=d1),"Must have one variable on LHS of formula")
  expect_error(Summarize(f1~f2,data=d1),"numeric variable on LHS")
  expect_error(Summarize(c1~f2,data=d1),"numeric variable on LHS")
  
  ## From default method
  expect_error(Summarize(d1),"does not work with a data.frame")
  expect_error(Summarize(as.matrix(d1[,c("q1","q2")])),"does not work with matrices")
  expect_error(Summarize(as.matrix(d1[,c("f1","f2","c1")])),"does not work with matrices")
})

test_that("Summarize() results without NAs",{
  ## Single quantitative variable
  tmp <- Summarize(~q1,data=d1)
  expect_is(tmp,"numeric")
  exp <- c(9,9,5,sd(d1$q1),1,3,5,7,9,0)
  names(exp) <- qnms
  expect_equal(tmp,exp)
  
  ## Quantitative variables separated by one factor
  tmp <- Summarize(q1~f1,data=d1)
  expect_is(tmp,"data.frame")
  exp <- data.frame(f1=c("A","B","C"),n=rep(3,3),nvalid=rep(3,3),
                    mean=c(2,5,8),sd=rep(1,3),min=c(1,4,7),
                    Q1=c(1,4,7)+0.5,median=c(2,5,8),Q3=c(2,5,8)+0.5,
                    max=c(3,6,9),percZero=rep(0,3))
  expect_equal(tmp,exp)

  # assure that the "by" variable is of the expected mode/type  
  expect_is(tmp$f1,"factor")
  tmp <- Summarize(q1~c1,data=d1)
  expect_is(tmp$c1,"character")
  tmp <- Summarize(q1~q3,data=d1)
  expect_is(tmp$q3,"numeric")
  
  ## Quantitative variables separated by two factors
  tmp <- Summarize(q1~f2+f1,data=d1)
  expect_is(tmp,"data.frame")
  exp <- data.frame(f2=rep(c("a","b","c"),3),f1=rep(c("A","B","C"),each=3),
                    n=rep(1,9),nvalid=rep(1,9),mean=1:9,sd=as.numeric(rep(NA,9)),
                    min=1:9,Q1=1:9,median=1:9,Q3=1:9,max=1:9,percZero=rep(0,9))
  expect_equal(tmp,exp)
 
  # assure that the "by" variables are of the expected mode/type  
  expect_is(tmp$f1,"factor")
  expect_is(tmp$f2,"factor")
  tmp <- Summarize(q1~c1+f1,data=d1)
  expect_is(tmp$c1,"character")
  expect_is(tmp$f1,"factor")
  tmp <- Summarize(q1~c1+q3,data=d1)
  expect_is(tmp$c1,"character")
  expect_is(tmp$q3,"numeric")
})

test_that("Summarize() results with NAs",{
  ## Single quantitative variable
  tmp <- Summarize(~q4,data=d1)
  exp <- c(9,8,4.5,sd(d1$q4,na.rm=TRUE),1,2.75,4.5,6.25,8,0)
  names(exp) <- qnms
  expect_is(tmp,"numeric")
  expect_equal(round(tmp,7),round(exp,7))  # minor difference in sd @ 8 decimals
 
  ## Quantitative variables separated by one factor
  tmp <- Summarize(q4~f1,data=d1)
  exp <- data.frame(f1=c("A","B","C"),n=c(3,3,3),nvalid=c(2,3,3),
                    mean=c(1.5,4,7.0),sd=c(0.7071068,1,1),min=c(1,3,6),
                    Q1=c(1.25,3.5,6.5),median=c(1.5,4,7),Q3=c(1.75,4.5,7.5),
                    max=c(2,5,8),percZero=rep(0,3))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp)
  
})


test_that("Summarize() results using exclude=",{
  tmp <- Summarize(q4~f1,data=d1,exclude="A")
  exp <- data.frame(f1=c("B","C"),n=c(3,3),nvalid=c(3,3),mean=c(4,7.0),sd=c(1,1),min=c(3,6),
                    Q1=c(3.5,6.5),median=c(4,7),Q3=c(4.5,7.5),max=c(5,8),percZero=rep(0,2))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp)
})


test_that("Summarize() Results from 1-d matrices and data.frames",{
  ## Matrix
  d <- matrix(1:9,ncol=1)
  tmp <- Summarize(d)
  exp <- c(9,9,5,sd(d),1,3,5,7,9,0)
  names(exp) <- qnms
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp)

  ## data.frame
  d <- as.data.frame(matrix(1:9,ncol=1))
  tmp <- Summarize(~V1,data=d)
  exp <- c(9,9,5,sd(d$V1),1,3,5,7,9,0)
  names(exp) <- qnms
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp)
})
