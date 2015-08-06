context("Summmarize() tests")

## Create simple dummy data
d <- data.frame(g1=factor(c("A","A","A","B","B","B","C","C","C")),
                g2=factor(c("a","b","c","a","b","c","a","b","c")),
                g3=factor(c("x","x","x","x","x","y","y","y","y")),
                dat=1:9,junk=11:19)

test_that("Summarize() messages",{
  ## From formula method
  expect_error(Summarize(dat+junk~g1+g2,data=d),"more than one variable on the LHS")
  expect_error(Summarize(dat~g1+g2+g3,data=d),"must contain only one or two factors")
  expect_error(Summarize(dat~g1+g2+g3,data=d),"must contain only one or two factors")
  expect_error(Summarize(~g1+g2,data=d),"Must have one variable on LHS of formula")
  expect_error(Summarize(~dat+junk,data=d),"Must have one variable on LHS of formula")
  expect_warning(Summarize(dat~junk,data=d),"RHS variable was converted to a factor")
  expect_warning(Summarize(dat~junk*g1,data=d),"First RHS variable was converted to a factor")
  expect_warning(Summarize(dat~g1*junk,data=d),"Second RHS variable was converted to a factor")
  expect_warning(Summarize(g1~dat,data=d),"RHS variable was converted to a factor")
  expect_error(Summarize(g1~g2,data=d,percent="derek"),"should be one of")
  
  ## From default method
  expect_error(Summarize(d),"does not work with a data.frame")
  expect_error(Summarize(as.matrix(d[,c("dat","junk")])),"does not work with matrices")
  expect_error(Summarize(as.matrix(d[,c("g1","g2","g3")])),"does not work with matrices")
})

test_that("Summarize() Results without NAs",{
  qnms <- c("n","nvalid","mean","sd","min","Q1","median","Q3","max","percZero")
  ## Single quantitative variable
  tmp <- Summarize(~dat,data=d)
  exp <- c(9,9,5,sd(d$dat),1,3,5,7,9,0)
  names(exp) <- qnms
  expect_is(tmp,"numeric")
  expect_equal(tmp,exp)
  
  ## Quantitative variables separated by one factor
  tmp <- Summarize(dat~g1,data=d)
  exp <- data.frame(g1=c("A","B","C"),n=rep(3,3),nvalid=rep(3,3),
                    mean=c(2,5,8),sd=rep(1,3),min=c(1,4,7),
                    Q1=c(1,4,7)+0.5,median=c(2,5,8),Q3=c(2,5,8)+0.5,
                    max=c(3,6,9),percZero=rep(0,3))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp)
  
  ## Quantitative variables separated by two factors
  tmp <- Summarize(dat~g2*g1,data=d)
  exp <- data.frame(g2=rep(c("a","b","c"),3),g1=rep(c("A","B","C"),each=3),
                    n=rep(1,9),nvalid=rep(1,9),mean=1:9,sd=as.numeric(rep(NA,9)),
                    min=1:9,Q1=1:9,median=1:9,Q3=1:9,max=1:9,percZero=rep(0,9))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp)
  
  ## Single factor variable
  # not as percents, without marginal totals
  exp1 <- table(d$g1)
  tmp <- Summarize(~g1,data=d,percent="none",addtotal=FALSE)
  expect_equivalent(tmp,exp1)
  # not as percents, with marginal totals
  exp <- addmargins(exp1,margin=1)
  tmp <- Summarize(~g1,data=d,percent="none")
  expect_equivalent(tmp,exp)
  # with percents, without marginal totals
  exp <- cbind(freq=exp1,perc=round(prop.table(exp1)*100,2))
  tmp <- Summarize(~g1,data=d,addtotal=FALSE)  
  # with percents, with marginal totals
  exp <- addmargins(exp,margin=1)
  tmp <- Summarize(~g1,data=d)
  expect_equivalent(tmp,exp)
  
  ## Two factor variables
  # not as percents, without marginal totals
  exp1 <- table(d$g2,d$g1)  
  tmp <- Summarize(g1~g2,data=d,percent="none",addtotal=FALSE)
  expect_equivalent(tmp,exp1)
  # Table percents, without marginal totals
  exp <- round(prop.table(exp1)*100,2)
  tmp <- Summarize(g1~g2,data=d,percent="total",addtotal=FALSE)
  expect_equivalent(tmp,exp)
  # Column percents, without marginal totals
  exp <- round(prop.table(exp1,margin=2)*100,2)
  tmp <- Summarize(g1~g2,data=d,percent="column",addtotal=FALSE)
  expect_equivalent(tmp,exp)
  # Row percents, without marginal totals
  exp <- round(prop.table(exp1,margin=1)*100,2)
  tmp <- Summarize(g1~g2,data=d,percent="row",addtotal=FALSE)
  expect_equivalent(tmp,exp)
  # Table percents, with marginal totals
  exp <- prop.table(exp1)*100
  exp <- round(addmargins(exp),2)
  tmp <- Summarize(g1~g2,data=d,percent="total")
  expect_equivalent(tmp,exp)
  # Column percents, without marginal totals
  exp <- prop.table(exp1,margin=2)*100
  exp <- round(addmargins(exp,margin=1),2)
  tmp <- Summarize(g1~g2,data=d,percent="column")
  expect_equivalent(tmp,exp)
  # Row percents, without marginal totals
  exp <- prop.table(exp1,margin=1)*100
  exp <- round(addmargins(exp,margin=2),2)
  tmp <- Summarize(g1~g2,data=d,percent="row")
  expect_equivalent(tmp,exp)
})

test_that("Summarize() Results with NAs",{
  qnms <- c("n","nvalid","mean","sd","min","Q1","median","Q3","max","percZero")
  d <- data.frame(g1=factor(c("A","A","A","B","B","B","C","C",NA)),
                  g2=factor(c("a","b","c","a","b","c","a","b","c")),
                  g3=factor(c("x","x","x","x","x","y","y","y","y")),
                  dat=c(NA,1:8),junk=11:19)
  ## Single quantitative variable
  tmp <- Summarize(~dat,data=d)
  exp <- c(9,8,4.5,sd(d$dat,na.rm=TRUE),1,2.75,4.5,6.25,8,0)
  names(exp) <- qnms
  expect_is(tmp,"numeric")
  expect_equal(round(tmp,7),round(exp,7))  # minor difference in sd @ 8 decimals
  
  ## Quantitative variables separated by one factor
  tmp <- Summarize(dat~g1,data=d)
  exp <- data.frame(g1=c("A","B","C"),n=c(3,3,2),nvalid=c(2,3,2),
                    mean=c(1.5,4,6.5),sd=c(0.7071068,1,0.7071068),min=c(1,3,6),
                    Q1=c(1.25,3.5,6.25),median=c(1.5,4,6.5),Q3=c(1.75,4.5,6.75),
                    max=c(2,5,7),percZero=rep(0,3))
  expect_is(tmp,"data.frame")
  expect_equal(tmp,exp)

  ## Single factor variable
  # not as percents, without marginal totals
  exp1 <- table(d$g1)
  tmp <- Summarize(~g1,data=d,percent="none",addtotal=FALSE)
  expect_equivalent(tmp,exp1)
  # not as percents, with marginal totals
  exp <- addmargins(exp1,margin=1)
  tmp <- Summarize(~g1,data=d,percent="none")
  expect_equivalent(tmp,exp)
  # with percents, without marginal totals
  exp <- cbind(freq=exp1,perc=round(prop.table(exp1)*100,2))
  tmp <- Summarize(~g1,data=d,addtotal=FALSE)  
  # with percents, with marginal totals
  exp <- addmargins(exp,margin=1)
  tmp <- Summarize(~g1,data=d)
  expect_equivalent(tmp,exp)
  
  ## Two factor variables
  # not as percents, without marginal totals
  exp1 <- table(d$g2,d$g1)  
  tmp <- Summarize(g1~g2,data=d,percent="none",addtotal=FALSE)
  expect_equivalent(tmp,exp1)
  # Table percents, without marginal totals
  exp <- round(prop.table(exp1)*100,2)
  tmp <- Summarize(g1~g2,data=d,percent="total",addtotal=FALSE)
  expect_equivalent(tmp,exp)
  # Column percents, without marginal totals
  exp <- round(prop.table(exp1,margin=2)*100,2)
  tmp <- Summarize(g1~g2,data=d,percent="column",addtotal=FALSE)
  expect_equivalent(tmp,exp)
  # Row percents, without marginal totals
  exp <- round(prop.table(exp1,margin=1)*100,2)
  tmp <- Summarize(g1~g2,data=d,percent="row",addtotal=FALSE)
  expect_equivalent(tmp,exp)
  # Table percents, with marginal totals
  exp <- prop.table(exp1)*100
  exp <- round(addmargins(exp),2)
  tmp <- Summarize(g1~g2,data=d,percent="total")
  expect_equivalent(tmp,exp)
  # Column percents, without marginal totals
  exp <- prop.table(exp1,margin=2)*100
  exp <- round(addmargins(exp,margin=1),2)
  tmp <- Summarize(g1~g2,data=d,percent="column")
  expect_equivalent(tmp,exp)
  # Row percents, without marginal totals
  exp <- prop.table(exp1,margin=1)*100
  exp <- round(addmargins(exp,margin=2),2)
  tmp <- Summarize(g1~g2,data=d,percent="row")
  expect_equivalent(tmp,exp)
})
