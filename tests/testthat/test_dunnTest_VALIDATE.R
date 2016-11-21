context("dunnTest() VALIDATE")
source("EXS_dunnTest.R")

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