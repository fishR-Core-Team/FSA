context("age-length key related OUTPUT")
source("EXS_AgeLengthKey.R")

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
  WR1.comb <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,seed=1234343))
  WR1.comb2 <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,seed=1234343))
  suppressWarnings(sum1 <- Summarize(len~age,data=WR1.comb))
  suppressWarnings(sum2 <- Summarize(len~age,data=WR1.comb))
  diff <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff,matrix(0,nrow=nrow(diff),ncol=ncol(diff)))
  WR1.comb <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,type="CR",seed=1234343))
  WR1.comb2 <- rbind(WR1.age, alkIndivAge(WR1.key,age~len,data=WR1.len,type="CR",seed=1234343))
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
  suppressWarnings(WR1.comb1 <- rbind(WR1.age, alkIndivAge(WR1.key1,age~len,data=WR1.len,seed=1234343)))
  suppressWarnings(WR1.comb2 <- rbind(WR1.age, alkIndivAge(WR1.key2,age~len,data=WR1.len,seed=1234343)))
  suppressWarnings(WR1.comb3 <- rbind(WR1.age, alkIndivAge(WR1.key3,age~len,data=WR1.len,seed=1234343)))
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
  suppressWarnings(suppressWarnings(sum1 <- alkMeanVar(WR1.key1,len~LCat1+age,WR1.age,len.n1)))
  suppressWarnings(suppressWarnings(sum2 <- alkMeanVar(WR1.key2,len~LCat2+age,WR1.age,len.n2)))
  suppressWarnings(suppressWarnings(sum3 <- alkMeanVar(WR1.key3,len~LCat3+age,WR1.age,len.n3)))
  ## Compare the different results
  diff12 <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff12,matrix(0,nrow=nrow(diff12),ncol=ncol(diff12)))
  diff23 <- as.matrix(sum2[,-1]-sum3[,-1])
  expect_equivalent(diff23,matrix(0,nrow=nrow(diff23),ncol=ncol(diff23)))
  
  suppressMessages(suppressWarnings(sum1 <- alkMeanVar(WR1.key1,len~LCat1+age,WR1.age,len.n1,method="QuinnDeriso")))
  suppressMessages(suppressWarnings(sum2 <- alkMeanVar(WR1.key2,len~LCat2+age,WR1.age,len.n2,method="QuinnDeriso")))
  suppressMessages(suppressWarnings(sum3 <- alkMeanVar(WR1.key3,len~LCat3+age,WR1.age,len.n3,method="QuinnDeriso")))
  ## Compare the different results
  diff12 <- as.matrix(sum1[,-1]-sum2[,-1])
  expect_equivalent(diff12,matrix(0,nrow=nrow(diff12),ncol=ncol(diff12)))
  diff23 <- as.matrix(sum2[,-1]-sum3[,-1])
  expect_equivalent(diff23,matrix(0,nrow=nrow(diff23),ncol=ncol(diff23)))
})
