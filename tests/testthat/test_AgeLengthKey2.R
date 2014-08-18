context("Verification of Age-Length Key results")

test_that("ALKAgeDist() reporduces results from Table 8.4 (left) of Quinn and Deriso (1999)",{
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
    tmp2 <- ALKAgeDist(agekey,lenA.n,len.n)

    ## Find difference in results
    diff <- tmp2[,2:4]-tmp1
    expect_that(all(diff==0),is_true())
    
    ## enter Q&D results as a guard against fishmethods changing
    props <- c(0.0003,0.0213,0.1624,0.0926,0.1533,0.1461,0.1260,0.0133,0.0277,0.0763,0.0298,0.0332,0.0162,0.1017)
    ses <- c(0.0003,0.0056,0.0157,0.0158,0.0185,0.0182,0.0150,0.0050,0.0074,0.0083,0.0047,0.0050,0.0031,0.0063)
    expect_that(all(round(tmp2$prop,4)-props==0),is_true())
    expect_that(all(round(tmp2$se,4)-ses==0),is_true())
  }
})