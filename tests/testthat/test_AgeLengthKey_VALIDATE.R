context("age-length key related VALIDATE")
source("EXS_AgeLengthKey.R")

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
    ## Quinn and Deriso (1999) data are alkdata and alkprop reproduces Table 8.4 results
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
