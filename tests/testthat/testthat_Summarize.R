## Create simple dummy data ----
d1 <- data.frame(f1=factor(c("A","A","A","B","B","B","C","C","C")),
                 f2=factor(c("a","b","c","a","b","c","a","b","c")),
                 f3=factor(c("A","A","A","B","B","B","C","C",NA)),
                 c1=c("A","A","A","B","B","B","C","C","C"),
                 c2=c("A","A","A","B","B","B","C","C",NA),
                 q1=0:8,q2=11:19,q3=c(1:3,1:3,1,2,2),q4=c(NA,0:7),
                 stringsAsFactors=FALSE)

# Result labels
qnms1 <- c("n","nvalid","mean","sd","min","Q1","median","Q3","max","percZero")


## Test Messages ----
test_that("Summarize(), formula method messages",{
  expect_error(Summarize(~f1,data=d1),
               "only works with a numeric variable")
  expect_error(Summarize(~c1,data=d1),
               "only works with a numeric variable")
  expect_error(Summarize(f1~1,data=d1),
               "only works with a numeric variable")
  expect_error(Summarize(c1~1,data=d1),
               "only works with a numeric variable")
  expect_error(Summarize(q1+q2~f1+f2,data=d1),
               "more than one variable on the LHS")
  expect_error(Summarize(q1~f1+f2+c1,data=d1),
               "may contain only one or two factors")
  expect_error(Summarize(q1~f1+f2+c1,data=d1),
               "may contain only one or two factors")
  expect_error(Summarize(~f1+f2,data=d1),
               "Must have one variable on LHS of formula")
  expect_error(Summarize(~q1+q2,data=d1),
               "Must have one variable on LHS of formula")
  expect_error(Summarize(f1~f2,data=d1),
               "numeric variable on LHS")
  expect_error(Summarize(c1~f2,data=d1),
               "numeric variable on LHS")
  expect_error(Summarize(~q1,data=d1,nvalid="derek"),
               "should be one of")
  expect_error(Summarize(~q1,data=d1,percZero="derek"),
               "should be one of")
})

test_that("Summarize(), default method, messages",{
  expect_error(Summarize(d1),
               "does not work with a data.frame")
  expect_error(Summarize(as.matrix(d1[,c("q1","q2")])),
               "does not work with matrices")
  expect_error(Summarize(as.matrix(d1[,c("f1","f2","c1")])),
               "does not work with matrices")
  expect_error(Summarize(d1$q1,nvalid="derek"),
               "should be one of")
  expect_error(Summarize(d1$q1,percZero="derek"),
               "should be one of")
})


## Test Output Types ----


## Validate Results ----
test_that("Summarize() results, single quantitative variable",{
  ## no NAs or zeros
  exp <- c(9,9,15,sd(d1$q2),11,13,15,17,19,0)  # all possible results
  names(exp) <- qnms1
  tmp <- Summarize(~q2,data=d1)
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(2,10)])  # drop nvalid & percZero from expectations
  tmp <- Summarize(~q2,data=d1,nvalid="always")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(10)])    # drop percZero from expectations
  tmp <- Summarize(~q2,data=d1,percZero="always")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(2)])     # drop nvalid from expectations
  tmp <- Summarize(~q2,data=d1,nvalid="always",percZero="always")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp)
  
  ## no NAs, but zeros
  exp <- c(9,9,4,sd(d1$q1),0,2,4,6,8,1/9*100)  # all possible results
  names(exp) <- qnms1
  tmp <- Summarize(~q1,data=d1)
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(2)])     # drop nvalid from expectations
  tmp <- Summarize(~q1,data=d1,percZero="never")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(2,10)])  # drop nvalid & percZero from expectations
  tmp <- Summarize(~q1,data=d1,nvalid="always",percZero="never")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(10)])    # drop percZero from expectations
  tmp <- Summarize(~q1,data=d1,nvalid="always")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp)
  
  ## NAs and zeros
  exp <- c(9,8,3.5,round(sd(d1$q4,na.rm=TRUE),getOption("digits"))
           ,0,1.75,3.5,5.25,7,1/8*100)  # all possible results
  names(exp) <- qnms1
  tmp <- Summarize(~q4,data=d1)
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp)
  tmp <- Summarize(~q4,data=d1,nvalid="never",percZero="never")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(2,10)])  # drop nvalid & percZero from expectations
  tmp <- Summarize(~q4,data=d1,percZero="never")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(10)])    # drop percZero from expectations
  tmp <- Summarize(~q4,data=d1,nvalid="never")
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(2)])     # drop nvalid from expectations
})

test_that("Summarize() results, quantitative variable by single factor",{
  ## no NAs or zeros
  exp <- data.frame(f1=c("A","B","C"),n=rep(3,3),nvalid=rep(3,3),
                    mean=c(12,15,18),sd=rep(1,3),min=c(11,14,17),
                    Q1=c(11,14,17)+0.5,median=c(12,15,18),Q3=c(12,15,18)+0.5,
                    max=c(13,16,19),percZero=rep(0,3))
  tmp <- Summarize(q2~f1,data=d1)
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp[,-c(3,11)])
  tmp <- Summarize(q2~f1,data=d1,nvalid="always")
  expect_equal(tmp,exp[,-c(11)])   # drop percZero from expectations
  tmp <- Summarize(q2~f1,data=d1,percZero="always")
  expect_equal(tmp,exp[,-c(3)])    # drop nvalid from expectations
  tmp <- Summarize(q2~f1,data=d1,nvalid="always",percZero="always")
  expect_equal(tmp,exp)            # drop nvalid & percZero from expectations
  
  ## no NAs, but zeros
  exp <- data.frame(f1=c("A","B","C"),n=rep(3,3),nvalid=rep(3,3),
                    mean=c(1,4,7),sd=rep(1,3),min=c(0,3,6),
                    Q1=c(0,3,6)+0.5,median=c(1,4,7),Q3=c(1,4,7)+0.5,
                    max=c(2,5,8),percZero=c(1,0,0)/3*100)
  tmp <- Summarize(q1~f1,data=d1)
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp[,-c(3)])    # drop nvalid from expectations
  tmp <- Summarize(q1~f1,data=d1,percZero="never")
  expect_equal(tmp,exp[,-c(3,11)]) # drop nvalid & percZero from expectations
  tmp <- Summarize(q1~f1,data=d1,nvalid="always",percZero="never")
  expect_equal(tmp,exp[,-c(11)])   # drop percZero from expectations
  tmp <- Summarize(q1~f1,data=d1,nvalid="always")
  expect_equal(tmp,exp)
  
  ## NAs and zeros
  exp <- data.frame(f1=c("A","B","C"),n=rep(3,3),nvalid=c(2,3,3),
                    mean=c(0.5,3,6),sd=round(c(sqrt(2)/2,1,1),
                                             getOption("digits")),min=c(0,2,5),
                    Q1=c(0.25,2.5,5.5),median=c(0.5,3,6),Q3=c(0.75,3.5,6.5),
                    max=c(1,4,7),percZero=c(50,0,0))
  tmp <- Summarize(q4~f1,data=d1)
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp)
  tmp <- Summarize(q4~f1,data=d1,nvalid="never",percZero="never")
  expect_equal(tmp,exp[,-c(3,11)]) # drop nvalid & percZero from expectations
  tmp <- Summarize(q4~f1,data=d1,percZero="never")
  expect_equal(tmp,exp[,-c(11)])   # drop percZero from expectations
  tmp <- Summarize(q4~f1,data=d1,nvalid="never")
  expect_equal(tmp,exp[,-c(3)])    # drop nvalid from expectations
})

test_that("Summarize() results, quantitative variable by single character",{
  exp <- data.frame(c1=c("A","B","C"),n=rep(3,3),nvalid=rep(3,3),
                    mean=c(12,15,18),sd=rep(1,3),min=c(11,14,17),
                    Q1=c(11,14,17)+0.5,median=c(12,15,18),Q3=c(12,15,18)+0.5,
                    max=c(13,16,19),percZero=rep(0,3),stringsAsFactors=FALSE)
  tmp <- Summarize(q2~c1,data=d1)
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp[,-c(3,11)])
  tmp <- Summarize(q2~c1,data=d1,nvalid="always")
  expect_equal(tmp,exp[,-c(11)])   # drop percZero from expectations
  tmp <- Summarize(q2~c1,data=d1,percZero="always")
  expect_equal(tmp,exp[,-c(3)])    # drop nvalid from expectations
  tmp <- Summarize(q2~c1,data=d1,nvalid="always",percZero="always")
  expect_equal(tmp,exp)            # drop nvalid & percZero from expectations
})

test_that("Summarize() results, quantitative variable by two factors",{
  exp <- data.frame(f2=rep(c("a","b","c"),3),f1=rep(c("A","B","C"),each=3),
                    n=rep(1,9),nvalid=rep(1,9),mean=11:19,
                    sd=as.numeric(rep(NA,9)),min=11:19,Q1=11:19,median=11:19,
                    Q3=11:19,max=11:19,percZero=rep(0,9))
  tmp <- Summarize(q2~f2+f1,data=d1)
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp[,-c(4,12)])  # drop nvalid & percZero from expectations
  tmp <- Summarize(q2~f2+f1,data=d1,nvalid="always")
  expect_equal(tmp,exp[,-c(12)])  # drop percZero from expectations
  tmp <- Summarize(q2~f2+f1,data=d1,percZero="always")
  expect_equal(tmp,exp[,-c(4)])  # drop nvalid from expectations
  tmp <- Summarize(q2~f2+f1,data=d1,nvalid="always",percZero="always")
  expect_equal(tmp,exp)
})

test_that("Summarize() results using exclude=",{
  exp <- data.frame(f1=c("B","C"),n=c(3,3),nvalid=c(3,3),
                    mean=c(3,6),sd=c(1,1),min=c(2,5),
                    Q1=c(2.5,5.5),median=c(3,6),Q3=c(3.5,6.5),
                    max=c(4,7),percZero=rep(0,2))
  tmp <- Summarize(q4~f1,data=d1,exclude="A")
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp[,-c(3,11)])  # drop nvalid & percZero from expectations
})


test_that("Summarize() Results from 1-d matrices and data.frames",{
  ## Matrix
  d <- matrix(1:9,ncol=1)
  tmp <- Summarize(d)
  exp <- c(9,9,5,sd(d),1,3,5,7,9,0)
  names(exp) <- qnms1
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(2,10)])  # drop nvalid and percZero from expectations
  tmp <- Summarize(d,nvalid="always")
  expect_equal(tmp,exp[-c(10)])    # drop percZero from expectations
  
  ## data.frame
  d <- as.data.frame(matrix(1:9,ncol=1))
  tmp <- Summarize(~V1,data=d)
  exp <- c(9,9,5,sd(d$V1),1,3,5,7,9,0)
  names(exp) <- qnms1
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp[-c(2,10)])  # drop nvalid and percZero from expectations
  tmp <- Summarize(~V1,data=d,percZero="always")
  expect_equal(tmp,exp[-c(2)])     # drop nvalid from expectations
})

test_that("Summarize() results, assure 'by' variable is the expected mode/type",{
  expect_is(Summarize(q1~f1,data=d1)$f1,"factor")
  expect_is(Summarize(q1~c1,data=d1)$c1,"character")
  expect_is(Summarize(q1~q3,data=d1)$q3,"numeric")
  tmp <- Summarize(q1~f1+f2,data=d1)
  expect_is(tmp$f1,"factor")
  expect_is(tmp$f2,"factor")
  tmp <- Summarize(q1~c1+f1,data=d1)
  expect_is(tmp$c1,"character")
  expect_is(tmp$f1,"factor")
  tmp <- Summarize(q1~c1+q3,data=d1)
  expect_is(tmp$c1,"character")
  expect_is(tmp$q3,"numeric")
  tmp <- Summarize(q1~f1+q3,data=d1)
  expect_is(tmp$f1,"factor")
  expect_is(tmp$q3,"numeric")
})

