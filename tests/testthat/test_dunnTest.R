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
  expect_error(dunnTest(ponds$pond,ponds$pH),"to a factor")
  expect_warning(dunnTest(ponds$pH,ponds$pond),"to a factor")
  expect_warning(dunnTest(ponds$pH,ponds$cpond),"to a factor")
  expect_error(dunnTest(pond~pH,data=ponds),"must be a factor")
  expect_warning(dunnTest(pH~pond,data=ponds),"to a factor")
  expect_warning(dunnTest(pH~cpond,data=ponds),"to a factor")
  expect_error(dunnTest(pH~pond+cpond,data=ponds),"only one RHS variable")
  expect_error(dunnTest(pH+cpond~pond,data=ponds))
})

test_that("dunnTest matches UnitStat results",{
  tmp <- dunnTest(pH~fpond,data=ponds,method="bonferroni")
  expect_equivalent(round(tmp$res$P.adj,4),unistat$P.adj)
})

test_that("dunnTest matches dunn.test results (within difference in two-sidedness) for ponds data",{
  if (require(dunn.test)) {
    ## Loop through all methods in p.adjustment.methods
    for (m in dunn.test::p.adjustment.methods) {
      tmp  <- dunnTest(pH~fpond,data=ponds,method=m,two.sided=FALSE)$res$P.adj
      junk <- utils::capture.output(tmp2 <- dunn.test(ponds$pH,ponds$fpond,method=m)$P.adjusted)
      expect_equivalent(tmp,tmp2)
    }
  } # end require()
})

test_that("dunnTest matches dunn.test results (within difference in two-sidedness) for homecare data",{
  if (require(dunn.test)) {
    data(homecare)
    ## Loop through all methods in p.adjustment.methods
    for (m in dunn.test::p.adjustment.methods) {
      tmp  <- dunnTest(occupation~eligibility,data=homecare,method=m,two.sided=FALSE)$res$P.adj
      junk <- utils::capture.output(tmp2 <- dunn.test(homecare$occupation,homecare$eligibility,method=m)$P.adjusted)
      expect_equivalent(tmp,tmp2)
    }
  } # end require()
})

test_that("dunnTest matches dunn.test results (within difference in two-sidedness) for airquality data",{
  if (require(dunn.test)) {
    data(airquality)
    ## Loop through all methods in p.adjustment.methods
    for (m in dunn.test::p.adjustment.methods) {
      suppressWarnings(tmp  <- dunnTest(Ozone~Month,data=airquality,method=m,two.sided=FALSE)$res$P.adj)
      junk <- utils::capture.output(tmp2 <- dunn.test(airquality$Ozone,airquality$Month,method=m)$P.adjusted)
      expect_equivalent(tmp,tmp2)
    }
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