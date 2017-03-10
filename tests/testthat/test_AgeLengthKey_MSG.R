context("age-length key related MESSAGES")
source("EXS_AgeLengthKey.R")

test_that("iCheckALK() messages",{
  ## one row does not sum to 1
  tmp <- alk
  tmp[2,2] <- 0.6  # sum >1
  expect_warning(FSA:::iCheckALK(tmp),"Key contained a row that does not sum to 1.")
  tmp[2,2] <- 0.1  # sum <1
  expect_warning(FSA:::iCheckALK(tmp),"Key contained a row that does not sum to 1.")
  ## table looks like frequencies, gives warning but converted to row proportions
  tmp <- 10*alk
  expect_warning(FSA:::iCheckALK(tmp),"'key' contained values >1")
  ## table contains a row that sums to 0
  tmp <- alk
  tmp[2,] <- 0
  expect_warning(FSA:::iCheckALK(tmp),"Key contained rows that sum to 0.")
  # give warning that the row was removed
  expect_warning(FSA:::iCheckALK(tmp,remove0rows=TRUE),"these rows were removed from the table")
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

test_that("alkMeanVar() errors and warnings",{
  ## bad key
  expect_warning(alkMeanVar(WR1.key*10,len~LCat+age,WR1.age,len.n),"contained values >1")
  ## bad len.n
  tmp <- len.n[-1]
  expect_error(alkMeanVar(WR1.key,len~LCat+age,WR1.age,tmp),"different numbers of length intervals")
  ## bad formulas
  expect_error(alkMeanVar(WR1.key,~LCat+age,WR1.age,len.n),"must have a LHS")
  expect_error(alkMeanVar(WR1.key,factor(len)~LCat+age,WR1.age,len.n),"must be numeric")
  expect_error(alkMeanVar(WR1.key,len~LCat,WR1.age,len.n),"must have two and only two")
  expect_error(alkMeanVar(WR1.key,len~LCat+age+ID,WR1.age,len.n),"must have two and only two")
})

test_that("alkAgeDist() errors and warnings",{
  ## bad key
  expect_warning(alkAgeDist(WR1.key*10,lenA.n,len.n),"contained values >1")
  ## bad len.n
  expect_error(alkAgeDist(WR1.key,lenA.n,len.n[-1]),"different numbers of length intervals")
  ## bad lenA.n
  expect_error(alkAgeDist(WR1.key,lenA.n[-1],len.n),"different numbers of length intervals")
})
