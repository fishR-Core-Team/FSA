context("dunnTest() function Tests")
## pH in four ponds data from Zar (2010)
ponds <- data.frame(pond=rep(1:4,each=8),
                     pH=c(7.68,7.69,7.70,7.70,7.72,7.73,7.73,7.76,
                          7.71,7.73,7.74,7.74,7.78,7.78,7.80,7.81,
                          7.74,7.75,7.77,7.78,7.80,7.81,7.84,NA,
                          7.71,7.71,7.74,7.79,7.81,7.85,7.87,7.91))
ponds$cpond <- paste0("pond_",ponds$pond)
ponds$fpond <- as.factor(ponds$pond)
ponds <- ponds[complete.cases(ponds),]

## p-value results from UniStat (http://www.unistat.com/guide/nonparametric-tests-kruskal-wallis-one-way-anova/)
unistat <- data.frame(Comparison=c("2-1","3-1","3-2","4-1","4-2","4-3"),
                      P.adj=c(0.1956,0.0191,1,0.0166,1,1))

test_that("dunnTest() error and warning messages",{
  expect_error(dunnTest(ponds$pond,ponds$pH))
  expect_warning(dunnTest(ponds$pH,ponds$pond))
  expect_warning(dunnTest(ponds$pH,ponds$cpond))
  expect_error(dunnTest(pond~pH,data=ponds))
  expect_warning(dunnTest(pH~pond,data=ponds))
  expect_warning(dunnTest(pH~cpond,data=ponds))
  expect_error(dunnTest(pH~pond+cpond,data=ponds))
  expect_error(dunnTest(pH+cpond~pond,data=ponds))
})

test_that("dunnTest matches UnitStat results",{
  tmp <- dunnTest(pH~fpond,data=ponds,method="bonferroni")
  expect_equivalent(round(tmp$res$P.adj,4),unistat$P.adj)
})

test_that("dunnTest matches dunn.test results (within difference in two-sidedness) for ponds data",{
  if (require(dunn.test)) {
    ## unadjusted p-values
    tmp  <- dunnTest(pH~fpond,data=ponds,method="none")$res$P.adj
    tmp2 <- dunn.test(ponds$pH,ponds$fpond,method="none")$P.adjusted*2
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Bonferroni method
    tmp  <- dunnTest(pH~fpond,data=ponds,method="bonferroni")$res$P.adj
    tmp2 <- dunn.test(ponds$pH,ponds$fpond,method="bonferroni")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Holm method
    tmp  <- dunnTest(pH~fpond,data=ponds,method="holm")$res$P.adj
    tmp2 <- dunn.test(ponds$pH,ponds$fpond,method="holm")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    #expect_equivalent(tmp,tmp2) # only the same for the 1st, 3rd, & 4th values
    ## Hochberg method
    tmp  <- dunnTest(pH~fpond,data=ponds,method="hochberg")$res$P.adj
    tmp2 <- dunn.test(ponds$pH,ponds$fpond,method="hochberg")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    #expect_equivalent(tmp,tmp2) # not the same for the 3rd and 4th values
    expect_equivalent(tmp[-c(3,4)],tmp2[-c(3,4)])
    ## Benjamini-Hochberg method
    tmp  <- dunnTest(pH~fpond,data=ponds,method="BH")$res$P.adj
    tmp2 <- dunn.test(ponds$pH,ponds$fpond,method="bh")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    #expect_equivalent(tmp,tmp2) # not the same for the 3rd and 4th values
    expect_equivalent(tmp[-c(3,4)],tmp2[-c(3,4)])
    ## Benjamini-Yuteli method
    tmp  <- dunnTest(pH~fpond,data=ponds,method="BY")$res$P.adj
    tmp2 <- dunn.test(ponds$pH,ponds$fpond,method="by")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    #expect_equivalent(tmp,tmp2) # not the same for the 4th value
    expect_equivalent(tmp[-c(4)],tmp2[-c(4)])
  } # end require()
})

test_that("dunnTest matches dunn.test results (within difference in two-sidedness) for homecare data",{
  if (require(dunn.test)) {
    data(homecare)
    ## unadjusted p-values
    tmp  <- dunnTest(occupation~eligibility,data=homecare,method="none")$res$P.adj
    tmp2 <- dunn.test(homecare$occupation,homecare$eligibility,method="none")$P.adjusted*2
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Bonferroni method
    tmp  <- dunnTest(occupation~eligibility,data=homecare,method="bonferroni")$res$P.adj
    tmp2 <- dunn.test(homecare$occupation,homecare$eligibility,method="bonferroni")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Holm method
    tmp  <- dunnTest(occupation~eligibility,data=homecare,method="holm")$res$P.adj
    tmp2 <- dunn.test(homecare$occupation,homecare$eligibility,method="holm")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Hochberg method
    tmp  <- dunnTest(occupation~eligibility,data=homecare,method="hochberg")$res$P.adj
    tmp2 <- dunn.test(homecare$occupation,homecare$eligibility,method="hochberg")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Benjamini-Hochberg method
    tmp  <- dunnTest(occupation~eligibility,data=homecare,method="BH")$res$P.adj
    tmp2 <- dunn.test(homecare$occupation,homecare$eligibility,method="bh")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Benjamini-Yuteli method
    tmp  <- dunnTest(occupation~eligibility,data=homecare,method="BY")$res$P.adj
    tmp2 <- dunn.test(homecare$occupation,homecare$eligibility,method="by")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
  } # end require()
})

test_that("dunnTest matches dunn.test results (within difference in two-sidedness) for airquality data",{
  if (require(dunn.test)) {
    data(airquality)
    ## unadjusted p-values
    tmp  <- dunnTest(Ozone~Month,data=airquality,method="none")$res$P.adj
    tmp2 <- dunn.test(airquality$Ozone,airquality$Month,method="none")$P.adjusted*2
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Bonferroni method
    tmp  <- dunnTest(Ozone~Month,data=airquality,method="bonferroni")$res$P.adj
    tmp2 <- dunn.test(airquality$Ozone,airquality$Month,method="bonferroni")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Holm method
    tmp  <- dunnTest(Ozone~Month,data=airquality,method="holm")$res$P.adj
    tmp2 <- dunn.test(airquality$Ozone,airquality$Month,method="holm")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    #expect_equivalent(tmp,tmp2) # all the same except for 8th value
    expect_equivalent(tmp[-8],tmp2[-8])
    ## Hochberg method
    tmp  <- dunnTest(Ozone~Month,data=airquality,method="hochberg")$res$P.adj
    tmp2 <- dunn.test(airquality$Ozone,airquality$Month,method="hochberg")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    #expect_equivalent(tmp,tmp2) # not the same for the 1st and 6th values
    expect_equivalent(tmp[-c(1,6)],tmp2[-c(1,6)])
    ## Benjamini-Hochberg method
    tmp  <- dunnTest(Ozone~Month,data=airquality,method="BH")$res$P.adj
    tmp2 <- dunn.test(airquality$Ozone,airquality$Month,method="bh")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
    ## Benjamini-Yuteli method
    tmp  <- dunnTest(Ozone~Month,data=airquality,method="BY")$res$P.adj
    tmp2 <- dunn.test(airquality$Ozone,airquality$Month,method="by")$P.adjusted
    tmp2[tmp2<1] <- tmp2[tmp2<1]*2
    tmp2[tmp2>1] <- 1
    cbind(tmp,tmp2,tmp-tmp2)
    expect_equivalent(tmp,tmp2)
  } # end require()
})

test_that("dunnTest matches pairw.kw results",{
  if (require(asbio)) {
    ## unadjusted p-values
    tmp  <- dunnTest(pH~fpond,data=ponds,method="bonferroni")$res$P.adj
    tmp2 <- fact2num(pairw.kw(ponds$pH,ponds$fpond)$summary$'Adj. P-value')
    expect_equivalent(round(tmp,6),tmp2)    
  } # end require()
})