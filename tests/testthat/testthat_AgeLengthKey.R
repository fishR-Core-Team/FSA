## Data for testing ----
# Create a "good" small ALK matrix
alk <- matrix(c(0.4,0.3,0.3,0.0,
                0.2,0.4,0.3,0.1,
                0.1,0.2,0.4,0.3,
                0.0,0.1,0.4,0.5,
                0.0,0.0,0.2,0.8),
              nrow=5,byrow=TRUE)
rownames(alk) <- c(10,20,30,40,50)
colnames(alk) <- c(2,3,4,5)

# Real data
WR1 <- WR79
WR1$LCat <- lencat(WR1$len,w=5)
len.n <- xtabs(~LCat,data=WR1)
WR1.age <- filterD(WR1,!is.na(age))
lenA.n <- xtabs(~LCat,data=WR1.age)
WR1.len <- filterD(WR1,is.na(age))
WR1.key <- prop.table(xtabs(~LCat+age,data=WR1.age),margin=1)



## Test Messages ----
test_that("iCheckALK() messages",{
  ## one row does not sum to 1
  tmp <- alk
  tmp[2,2] <- 0.6  # sum >1
  expect_warning(FSA:::iCheckALK(tmp),
                 "Key contained a row that does not sum to 1.")
  tmp[2,2] <- 0.1  # sum <1
  expect_warning(FSA:::iCheckALK(tmp),
                 "Key contained a row that does not sum to 1.")
  ## table looks like frequencies, gives warning but converted to row proportions
  tmp <- 10*alk
  expect_warning(FSA:::iCheckALK(tmp),
                 "'key' contained values >1")
  ## table contains a row that sums to 0
  tmp <- alk
  tmp[2,] <- 0
  expect_warning(FSA:::iCheckALK(tmp),
                 "Key contained rows that sum to 0.")
  # give warning that the row was removed
  expect_warning(FSA:::iCheckALK(tmp,remove0rows=TRUE),
                 "these rows were removed from the table")
  ## bad row names
  tmp <- alk
  rownames(tmp) <- paste0("Len",rownames(alk))
  expect_error(suppressWarnings(FSA:::iCheckALK(tmp)),
               "The row names of 'key' must be numeric")
  ## bad column names
  tmp <- alk
  colnames(tmp) <- paste0("Age",colnames(alk))
  expect_error(suppressWarnings(FSA:::iCheckALK(tmp)),
               "The column names of 'key' must be numeric")
})  

test_that("alkPlot() messages",{
  ## Bad argument choices
  expect_error(alkPlot(alk,type="derek"),"should be one of")
  expect_error(alkPlot(alk,pal="derek"),"should be one of")
  ## one row is all zeros
  tmp <- alk
  tmp[2,] <- 0
  expect_warning(alkPlot(tmp),"sum to 0")
  expect_warning(alkPlot(tmp,type="area"),"sum to 0")
  expect_warning(alkPlot(tmp,type="lines"),"sum to 0")
  expect_warning(alkPlot(tmp,type="bubble"),"sum to 0")
  ## one column is all zeros with restrictive xlim
  tmp <- alk
  tmp["40",] <- c(0,0,0,1)
  tmp["50",] <- c(0,0,0,1)
  expect_error(alkPlot(tmp,type="bar",xlim=c(40,50)),"too restrictive")
})

test_that("alkIndivAge() messages",{
  ## bad types
  expect_error(alkIndivAge(WR1.key,age~len,data=WR1.len,type="derek"),
               "should be one of")
  ## bad formulae
  expect_error(alkIndivAge(WR1.key,~age+len,data=WR1.len),
               "must have only one RHS variable")
  expect_error(alkIndivAge(WR1.key,age~len+ID,data=WR1.len),
               "must have only one variable")
  expect_error(alkIndivAge(WR1.key,age+ID~len,data=WR1.len),
               "more than one variable on the LHS")
  expect_error(alkIndivAge(WR1.key,age~as.factor(len),data=WR1.len),
               "RHS variable must be numeric")
  expect_error(alkIndivAge(WR1.key,as.factor(age)~len,data=WR1.len),
               "LHS variable must be numeric")
  ## bad key
  expect_warning(alkIndivAge(10*WR1.key,age~len,data=WR1.len),
                 "contained values >1")
  expect_warning(alkIndivAge(0.1*WR1.key,age~len,data=WR1.len),
                 "does not sum to 1")
  expect_warning(alkIndivAge(WR1.key[1:5,],age~len,data=WR1.len),
                 "will be treated as all-inclusive")
  expect_error(alkIndivAge(WR1.key[-c(1:5),],age~len,data=WR1.len),
               "minimum observed length in the length")
})

test_that("alkMeanVar() errors and warnings",{
  ## bad key
  expect_warning(alkMeanVar(WR1.key*10,len~LCat+age,WR1.age,len.n),
                 "contained values >1")
  ## bad len.n
  tmp <- len.n[-1]
  expect_error(alkMeanVar(WR1.key,len~LCat+age,WR1.age,tmp),
               "different numbers of length intervals")
  ## bad formulas
  expect_error(alkMeanVar(WR1.key,~LCat+age,WR1.age,len.n),
               "must have a LHS")
  expect_error(alkMeanVar(WR1.key,factor(len)~LCat+age,WR1.age,len.n),
               "must be numeric")
  expect_error(alkMeanVar(WR1.key,len~LCat,WR1.age,len.n),
               "must have two and only two")
  expect_error(alkMeanVar(WR1.key,len~LCat+age+ID,WR1.age,len.n),
               "must have two and only two")
})

test_that("alkAgeDist() errors and warnings",{
  ## bad key
  expect_warning(alkAgeDist(WR1.key*10,lenA.n,len.n),
                 "contained values >1")
  ## bad len.n
  expect_error(alkAgeDist(WR1.key,lenA.n,len.n[-1]),
               "different numbers of length intervals")
  ## bad lenA.n
  expect_error(alkAgeDist(WR1.key,lenA.n[-1],len.n),
               "different numbers of length intervals")
})


## Test Output Types ----
test_that("Does age variable get added with alkIndivAge()",{
  WR1 <- WR79
  WR1$LCat <- lencat(WR1$len,w=5)
  WR1.age <- subset(WR1, !is.na(age))
  WR1.len <- subset(WR1, is.na(age))
  # remove age variable
  WR1.len <- WR1.len[,-which(names(WR1.len)=="age")]
  WR1.key <- prop.table(xtabs(~LCat+age,data=WR1.age),margin=1)
  tmp <- alkIndivAge(WR1.key,~len,data=WR1.len)
  expect_true(any(names(tmp)=="age"))
  tmp <- alkIndivAge(WR1.key,~len,data=WR1.len,type="CR")
  expect_true(any(names(tmp)=="age"))
})

test_that("Does 'seed=' work in alkIndivAge()",{
  WR1.comb <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,
                                         seed=1234343))
  WR1.comb2 <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,
                                          seed=1234343))
  suppressWarnings(sum1 <- Summarize(len~age,data=WR1.comb))
  suppressWarnings(sum2 <- Summarize(len~age,data=WR1.comb))
  diff <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff,matrix(0,nrow=nrow(diff),ncol=ncol(diff)))
  WR1.comb <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,type="CR",
                                         seed=1234343))
  WR1.comb2 <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,type="CR",
                                          seed=1234343))
  suppressWarnings(sum1 <- Summarize(len~age,data=WR1.comb))
  suppressWarnings(sum2 <- Summarize(len~age,data=WR1.comb))
  diff <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff,matrix(0,nrow=nrow(diff),ncol=ncol(diff)))
})

test_that("Are same results achieved when handling a missing row differently",{
  WR1 <- WR79
  ## Create a missing row in the ALK
  WR1 <- subset(WR1,len<100 | len>105)
  ## Create different versions of the length intervals variable
  WR1$LCat1 <- lencat(WR1$len,w=5)
  WR1$LCat2 <- lencat(WR1$len,w=5,as.fact=TRUE)
  WR1$LCat3 <- lencat(WR1$len,w=5,as.fact=TRUE,droplevels=TRUE)
  ## Get keys with different length interval variables
  WR1.age <- subset(WR1, !is.na(age))
  WR1.len <- subset(WR1, is.na(age))
  WR1.key1 <- prop.table(xtabs(~LCat1+age,data=WR1.age),margin=1)
  WR1.key2 <- prop.table(xtabs(~LCat2+age,data=WR1.age),margin=1)
  WR1.key3 <- prop.table(xtabs(~LCat3+age,data=WR1.age),margin=1)
  ## Apply the different ALKs with alkIndivAge
  suppressWarnings(WR1.comb1 <- rbind(WR1.age, 
                                      alkIndivAge(WR1.key1,age~len,
                                                  data=WR1.len,seed=1234343)))
  suppressWarnings(WR1.comb2 <- rbind(WR1.age,
                                      alkIndivAge(WR1.key2,age~len,
                                                  data=WR1.len,seed=1234343)))
  suppressWarnings(WR1.comb3 <- rbind(WR1.age,
                                      alkIndivAge(WR1.key3,age~len,
                                                  data=WR1.len,seed=1234343)))
  ## Make the combined data.frames
  suppressWarnings(sum1 <- Summarize(len~age,data=WR1.comb1))
  suppressWarnings(sum2 <- Summarize(len~age,data=WR1.comb2))
  suppressWarnings(sum3 <- Summarize(len~age,data=WR1.comb3))
  ## Compare the different results
  diff12 <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff12,matrix(0,nrow=nrow(diff12),ncol=ncol(diff12)))
  diff23 <- as.matrix(sum2[,-1]-sum3[,-1])
  expect_equivalent(diff23,matrix(0,nrow=nrow(diff23),ncol=ncol(diff23)))
  
  ## Apply the different ALKs with alkAgeDist
  len.n1 <- xtabs(~LCat1,data=WR1)
  len.An1 <- xtabs(~LCat1,data=WR1.age)
  sum1 <- alkAgeDist(WR1.key1,len.An1,len.n1)
  len.n2 <- xtabs(~LCat2,data=WR1)
  len.An2 <- xtabs(~LCat2,data=WR1.age)
  suppressWarnings(sum2 <- alkAgeDist(WR1.key2,len.An2,len.n2))
  len.n3 <- xtabs(~LCat3,data=WR1)
  len.An3 <- xtabs(~LCat3,data=WR1.age)
  sum3 <- alkAgeDist(WR1.key3,len.An3,len.n3)
  ## Compare the different results
  diff12 <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff12,matrix(0,nrow=nrow(diff12),ncol=ncol(diff12)))
  diff23 <- as.matrix(sum2[,-1]-sum3[,-1])
  expect_equivalent(diff23,matrix(0,nrow=nrow(diff23),ncol=ncol(diff23)))
  
  ## Apply the different ALKs with alkMeanVar
  suppressWarnings(sum1 <- alkMeanVar(WR1.key1,len~LCat1+age,WR1.age,len.n1))
  suppressWarnings(sum2 <- alkMeanVar(WR1.key2,len~LCat2+age,WR1.age,len.n2))
  suppressWarnings(sum3 <- alkMeanVar(WR1.key3,len~LCat3+age,WR1.age,len.n3))
  ## Compare the different results
  diff12 <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff12,matrix(0,nrow=nrow(diff12),ncol=ncol(diff12)))
  diff23 <- as.matrix(sum2[,-1]-sum3[,-1])
  expect_equivalent(diff23,matrix(0,nrow=nrow(diff23),ncol=ncol(diff23)))
  
  suppressWarnings(sum1 <- alkMeanVar(WR1.key1,len~LCat1+age,WR1.age,len.n1,
                                      method="QuinnDeriso"))
  suppressWarnings(sum2 <- alkMeanVar(WR1.key2,len~LCat2+age,WR1.age,len.n2,
                                      method="QuinnDeriso"))
  suppressWarnings(sum3 <- alkMeanVar(WR1.key3,len~LCat3+age,WR1.age,len.n3,
                                      method="QuinnDeriso"))
  ## Compare the different results
  diff12 <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff12,matrix(0,nrow=nrow(diff12),ncol=ncol(diff12)))
  diff23 <- as.matrix(sum2[,-1]-sum3[,-1])
  expect_equivalent(diff23,matrix(0,nrow=nrow(diff23),ncol=ncol(diff23)))
})


## Validate Results ----
test_that("Assigned ages are correct (within rounding) with semi-random alkIndivAge()",{
  ## Make a simple age-sample for a simple ALK
  tmp.age <- data.frame(len=c(10,10,10,10,20,20,20,20,30,30,30),
                        age=c(1,1,2,2,1,2,2,3,2,3,3))
  tmp.key <- prop.table(xtabs(~len+age,data=tmp.age),margin=1)
  ## Make a simple length sample ... with simple assertions
  ## -- for 10-cm fish ... one age-1 and one age-2
  ## -- for 20-cm fish ... one or two age-1 and age-3
  ##                   ... two or three age-2
  ## -- for 30-cm fish ... zero or one age-2 fish
  ## --                ... one or two age-3 fish
  tmp.len <- data.frame(len=c(10,10,20,20,20,20,20,30,30))
  new.age <- alkIndivAge(tmp.key,~len,data=tmp.len)
  new.sum <- xtabs(~len+age,data=new.age)
  ## do rowSums match un-aged lengths
  expect_equal(as.numeric(rowSums(new.sum)),as.numeric(xtabs(~len,data=tmp.len)))
  ## do rows match assertions from above
  expect_true(all(as.numeric(new.sum[1,])==c(1,1,0)))
  expect_true(all(as.numeric(new.sum[2,])==c(2,2,1))|
                all(as.numeric(new.sum[2,])==c(1,3,1))|
                all(as.numeric(new.sum[2,])==c(1,2,2)))
  expect_true(all(as.numeric(new.sum[3,])==c(0,0,2))|
                all(as.numeric(new.sum[3,])==c(0,1,1)))
})

test_that("alkAgeDist() reproduces results from Table 8.4 (left) of Quinn and Deriso (1999)",{
  if (require(fishmethods)) {
    ## Q&D (1999) data are alkdata and alkprop reproduces Table 8.4 results
    data(alkdata,package="fishmethods")
    tmp1 <- alkprop(alkdata)$results
  }
  if (require(FSAdata)) {
    ## Same data in SnapperHG2 in a different format
    ## create ALK and intermediate results
    data(SnapperHG2,package="FSAdata")
    len.n <- xtabs(~len,data=SnapperHG2)
    sn.age <- subset(SnapperHG2,!is.na(age))
    agekey <- prop.table(xtabs(~len+age,data=sn.age),1)
    lenA.n <- xtabs(~len,data=sn.age)
    ## get ALKAgeDist results
    tmp2 <- alkAgeDist(agekey,lenA.n,len.n)
    
    ## Find difference in results
    diff <- as.matrix(tmp2[,-1]-tmp1[,-3])
    expect_equivalent(diff,matrix(0,nrow=nrow(diff),ncol=ncol(diff)))
    
    ## enter Q&D results as a guard against fishmethods changing
    props <- c(0.0003,0.0213,0.1624,0.0926,0.1533,0.1461,0.1260,
               0.0133,0.0277,0.0763,0.0298,0.0332,0.0162,0.1017)
    ses <- c(0.0003,0.0056,0.0157,0.0158,0.0185,0.0182,0.0150,
             0.0050,0.0074,0.0083,0.0047,0.0050,0.0031,0.0063)
    diff <- as.matrix(round(tmp2[,-1],4)-cbind(props,ses))
    expect_equivalent(diff,matrix(0,nrow=nrow(diff),ncol=ncol(diff)))
  }
})
