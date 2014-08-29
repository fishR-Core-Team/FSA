context("age-length key related Messages")

test_that("iCheckALK() errors and warnings",{
  ## create a "good" small ALK matrix
  alk <- matrix(c(0.4,0.3,0.3,0.0,
                 0.2,0.4,0.3,0.1,
                 0.1,0.2,0.4,0.3,
                 0.0,0.1,0.4,0.5,
                 0.0,0.0,0.2,0.8),
               nrow=5,byrow=TRUE)
  rownames(alk) <- c(10,20,30,40,50)
  colnames(alk) <- c(2,3,4,5)
  addmargins(alk,margin=2)
  
  ## one row does not sum to 1
  tmp <- alk
  tmp[2,2] <- 0.6  # sum >1
  expect_that(iCheckALK(tmp),gives_warning())
  tmp[2,2] <- 0.1  # sum <1
  expect_that(iCheckALK(tmp),gives_warning())
  ## table looks like frequencies, gives warning but converted to row proportions
  tmp <- 10*alk
  expect_that(iCheckALK(tmp),gives_warning())
  ## table contains a row that sums to 0
  tmp <- alk
  tmp[2,] <- 0
  # give warning of this
  expect_that(iCheckALK(tmp),gives_warning())
  # give warning that the row was removed
  expect_that(iCheckALK(tmp,remove0rows=TRUE),gives_warning())
  ## bad row names
  tmp <- alk
  rownames(tmp) <- paste0("Len",rownames(alk))
  expect_that(iCheckALK(tmp),throws_error())
  ## bad column names
  tmp <- alk
  colnames(tmp) <- paste0("Age",colnames(alk))
  expect_that(iCheckALK(tmp),throws_error())
})  

test_that("alkMeanVar() errors and warnings",{
  data(WR79)
  WR79$LCat <- lencat(WR79$len,w=5)
  len.n <- xtabs(~LCat,data=WR79)
  WR79.age <- Subset(WR79,!is.na(age))
  alk <- prop.table(xtabs(~LCat+age,data=WR79.age),margin=1)
  ## bad key
  expect_that(alkMeanVar(alk*10,len~LCat+age,WR79.age,len.n),gives_warning())
  ## bad len.n
  tmp <- len.n[-1]
  expect_that(alkMeanVar(alk,len~LCat+age,WR79.age,tmp),throws_error())
  ## bad formulas
  # no LHS
  expect_that(alkMeanVar(alk,~LCat+age,WR79.age,len.n),throws_error())
  # factor on LHS
  expect_that(alkMeanVar(alk,factor(len)~LCat+age,WR79.age,len.n),throws_error())
  # only one variable on RHS
  expect_that(alkMeanVar(alk,len~LCat,WR79.age,len.n),throws_error())
  # three variables on RHS
  expect_that(alkMeanVar(alk,len~LCat+age+ID,WR79.age,len.n),throws_error())
})

test_that("alkPlot() errors and warnings",{
  ## create a "good" small ALK matrix
  alk <- matrix(c(0.4,0.3,0.3,0.0,
                  0.2,0.4,0.3,0.1,
                  0.1,0.2,0.4,0.3,
                  0.0,0.1,0.4,0.5,
                  0.0,0.0,0.2,0.8),
                nrow=5,byrow=TRUE)
  rownames(alk) <- c(10,20,30,40,50)
  colnames(alk) <- c(2,3,4,5)
  addmargins(alk,margin=2)
  
  ## one row is all zeroes
  tmp <- alk
  tmp[2,] <- 0
  expect_that(alkPlot(tmp),gives_warning())
  expect_that(alkPlot(tmp,type="area"),gives_warning())
})