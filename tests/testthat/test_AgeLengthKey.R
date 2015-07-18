context("age-length key related Messages")

## create a "good" small ALK matrix for testing
alk <- matrix(c(0.4,0.3,0.3,0.0,
                0.2,0.4,0.3,0.1,
                0.1,0.2,0.4,0.3,
                0.0,0.1,0.4,0.5,
                0.0,0.0,0.2,0.8),
              nrow=5,byrow=TRUE)
rownames(alk) <- c(10,20,30,40,50)
colnames(alk) <- c(2,3,4,5)

## Get some real data
data(WR79)


##############################################################
# internal function
##############################################################
test_that("iCheckALK() errors and warnings",{
  ## one row does not sum to 1
  tmp <- alk
  tmp[2,2] <- 0.6  # sum >1
  expect_warning(FSA:::iCheckALK(tmp))
  tmp[2,2] <- 0.1  # sum <1
  expect_warning(FSA:::iCheckALK(tmp))
  ## table looks like frequencies, gives warning but converted to row proportions
  tmp <- 10*alk
  expect_warning(FSA:::iCheckALK(tmp))
  ## table contains a row that sums to 0
  tmp <- alk
  tmp[2,] <- 0
  # give warning of this
  expect_warning(FSA:::iCheckALK(tmp))
  # give warning that the row was removed
  expect_warning(FSA:::iCheckALK(tmp,remove0rows=TRUE))
  ## bad row names
  tmp <- alk
  rownames(tmp) <- paste0("Len",rownames(alk))
  expect_error(FSA:::iCheckALK(tmp))
  ## bad column names
  tmp <- alk
  colnames(tmp) <- paste0("Age",colnames(alk))
  expect_error(FSA:::iCheckALK(tmp))
})  


##############################################################
# alkPlot function
##############################################################
test_that("alkPlot() errors and warnings",{
  ## one row is all zeroes
  tmp <- alk
  tmp[2,] <- 0
  expect_warning(alkPlot(tmp),"sum to 0")
  expect_warning(alkPlot(tmp,type="area"),"sum to 0")
  ## deprecation message
  expect_warning(ageKeyPlot(alk),"deprecated")
})


##############################################################
# alkIndivAge() also see below
##############################################################
test_that("alkIndivAge() errors and warnings",{
  WR1 <- WR79
  WR1$LCat <- lencat(WR1$len,w=5)
  WR1.age <- subset(WR1,!is.na(age))
  WR1.len <- subset(WR1,is.na(age))
  WR1.key <- prop.table(xtabs(~LCat+age,data=WR1.age),margin=1)
  ## bad types
  expect_error(alkIndivAge(WR1.key,age~len,data=WR1.len,type="derek"),"should be one of")
  ## bad formulae
  expect_error(alkIndivAge(WR1.key,~age+len,data=WR1.len),"must have only one RHS variable")
  expect_error(alkIndivAge(WR1.key,age~len+ID,data=WR1.len),"must have only one variable")
  expect_error(alkIndivAge(WR1.key,age+ID~len,data=WR1.len),"more than one variable on the LHS")
  expect_error(alkIndivAge(WR1.key,age~as.factor(len),data=WR1.len),"RHS variable must be numeric")
  expect_error(alkIndivAge(WR1.key,as.factor(age)~len,data=WR1.len),"LHS variable must be numeric")
  ## bad key
  expect_warning(alkIndivAge(10*WR1.key,age~len,data=WR1.len),"contained values >1")
  expect_warning(alkIndivAge(0.1*WR1.key,age~len,data=WR1.len),"does not sum to 1")
  expect_warning(alkIndivAge(WR1.key[1:5,],age~len,data=WR1.len),"will be treated as all-inclusive")
  expect_error(alkIndivAge(WR1.key[-c(1:5),],age~len,data=WR1.len),"minimum observed length in the length")
})

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
  WR1 <- WR79
  WR1$LCat <- lencat(WR1$len,w=5)
  WR1.age <- subset(WR1, !is.na(age))
  WR1.len <- subset(WR1, is.na(age))
  WR1.key <- prop.table(xtabs(~LCat+age,data=WR1.age),margin=1)
  WR1.comb <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,seed=1234343))
  WR1.comb2 <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,seed=1234343))
  suppressWarnings(sum1 <- Summarize(len~age,data=WR1.comb))
  suppressWarnings(sum2 <- Summarize(len~age,data=WR1.comb))
  diff <- sum1[,-1]-sum2[,-1]
  expect_true(all(diff==0))
  WR1.comb <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,type="CR",seed=1234343))
  WR1.comb2 <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,type="CR",seed=1234343))
  suppressWarnings(sum1 <- Summarize(len~age,data=WR1.comb))
  suppressWarnings(sum2 <- Summarize(len~age,data=WR1.comb))
  diff <- sum1[,-1]-sum2[,-1]
  expect_true(all(diff==0))
})


##############################################################
# alkMeanVar function also see below
##############################################################
test_that("alkMeanVar() errors and warnings",{
  WR1 <- WR79
  WR1$LCat <- lencat(WR1$len,w=5)
  len.n <- xtabs(~LCat,data=WR1)
  WR1.age <- Subset(WR1,!is.na(age))
  alk <- prop.table(xtabs(~LCat+age,data=WR1.age),margin=1)
  ## bad key
  expect_warning(alkMeanVar(alk*10,len~LCat+age,WR1.age,len.n),"contained values >1")
  ## bad len.n
  tmp <- len.n[-1]
  expect_error(alkMeanVar(alk,len~LCat+age,WR1.age,tmp),"different numbers of length intervals")
  ## bad formulas
  # no LHS
  expect_error(alkMeanVar(alk,~LCat+age,WR1.age,len.n),"must have a LHS")
  # factor on LHS
  expect_error(alkMeanVar(alk,factor(len)~LCat+age,WR1.age,len.n),"must be numeric")
  # only one variable on RHS
  expect_error(alkMeanVar(alk,len~LCat,WR1.age,len.n),"must have two and only two")
  # three variables on RHS
  expect_error(alkMeanVar(alk,len~LCat+age+ID,WR1.age,len.n),"must have two and only two")
})


##############################################################
# alkAgeDist() also see below
##############################################################
test_that("alkAgeDist() errors and warnings",{
  WR1 <- WR79
  WR1$LCat <- lencat(WR1$len,w=5)
  len.n <- xtabs(~LCat,data=WR1)
  WR1.age <- Subset(WR1,!is.na(age))
  alk <- prop.table(xtabs(~LCat+age,data=WR1.age),margin=1)
  lenA.n <- xtabs(~LCat,data=WR1.age)
  ## bad key
  expect_warning(alkAgeDist(alk*10,lenA.n,len.n),"contained values >1")
  ## bad len.n
  expect_error(alkAgeDist(alk,lenA.n,len.n[-1]),"different numbers of length intervals")
  ## bad lenA.n
  expect_error(alkAgeDist(alk,lenA.n[-1],len.n),"different numbers of length intervals")
})

test_that("alkAgeDist() reproduces results from Table 8.4 (left) of Quinn and Deriso (1999)",{
  if (require(fishmethods)) {
    ## Quinn and Deriso (1999) data are alkdata and alkprop reproduces
    ##   Table 8.4 results
    data(alkdata)
    tmp1 <- alkprop(alkdata)$results
  }
  if (require(FSAdata)) {
    ## Same data in SnapperHG2 in a different format
    ## create ALK and intermediate results
    data(SnapperHG2)
    len.n <- xtabs(~len,data=SnapperHG2)
    sn.age <- subset(SnapperHG2,!is.na(age))
    agekey <- prop.table(xtabs(~len+age,data=sn.age),1)
    lenA.n <- xtabs(~len,data=sn.age)
    ## get ALKAgeDist results
    tmp2 <- alkAgeDist(agekey,lenA.n,len.n)
    
    ## Find difference in results
    diff <- tmp2[,-1]-tmp1[,-3]
    expect_true(all(diff==0))
    
    ## enter Q&D results as a guard against fishmethods changing
    props <- c(0.0003,0.0213,0.1624,0.0926,0.1533,0.1461,0.1260,0.0133,0.0277,0.0763,0.0298,0.0332,0.0162,0.1017)
    ses <- c(0.0003,0.0056,0.0157,0.0158,0.0185,0.0182,0.0150,0.0050,0.0074,0.0083,0.0047,0.0050,0.0031,0.0063)
    expect_true(all(round(tmp2$prop,4)-props==0))
    expect_true(all(round(tmp2$se,4)-ses==0))
  }
})


# ############################################################
# ############################################################
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
  suppressWarnings(WR1.comb1 <- rbind(WR1.age, alkIndivAge(WR1.key1,age~len,data=WR1.len,seed=1234343)))
  suppressWarnings(WR1.comb2 <- rbind(WR1.age, alkIndivAge(WR1.key2,age~len,data=WR1.len,seed=1234343)))
  suppressWarnings(WR1.comb3 <- rbind(WR1.age, alkIndivAge(WR1.key3,age~len,data=WR1.len,seed=1234343)))
  ## Make the combined data.frames
  suppressWarnings(sum1 <- Summarize(len~age,data=WR1.comb1))
  suppressWarnings(sum2 <- Summarize(len~age,data=WR1.comb2))
  suppressWarnings(sum3 <- Summarize(len~age,data=WR1.comb3))
  ## Compare the different results
  diff12 <- sum1[,-1]-sum2[,-1]
  diff23 <- sum2[,-1]-sum3[,-1]
  expect_true(all(diff12==0))
  expect_true(all(diff23==0))  
  
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
  diff12 <- sum1-sum2
  diff23 <- sum2-sum3
  expect_true(all(diff12==0))
  expect_true(all(diff23==0))  
  
  ## Apply the different ALKs with alkMeanVar
  sum1 <- alkMeanVar(WR1.key1,len~LCat1+age,WR1.age,len.n1)
  suppressWarnings(sum2 <- alkMeanVar(WR1.key2,len~LCat2+age,WR1.age,len.n2))
  sum3 <- alkMeanVar(WR1.key3,len~LCat3+age,WR1.age,len.n3)
  ## Compare the different results
  diff12 <- sum1-sum2
  diff23 <- sum2-sum3
  expect_true(all(diff12==0))
  expect_true(all(diff23==0))  
  
  sum1 <- alkMeanVar(WR1.key1,len~LCat1+age,WR1.age,len.n1,method="QuinnDeriso")
  suppressWarnings(sum2 <- alkMeanVar(WR1.key2,len~LCat2+age,WR1.age,len.n2,method="QuinnDeriso"))
  sum3 <- alkMeanVar(WR1.key3,len~LCat3+age,WR1.age,len.n3,method="QuinnDeriso")
  ## Compare the different results
  diff12 <- sum1-sum2
  diff23 <- sum2-sum3
  expect_true(all(diff12==0))
  expect_true(all(diff23==0))  
})

