## Data for testing ----
## pH in four ponds data from Zar (2010)
ponds2 <- data.frame(pond=rep(1:4,each=8),
                     pH=c(7.68,7.69,7.70,7.70,7.72,7.73,7.73,7.76,
                          7.71,7.73,7.74,7.74,7.78,7.78,7.80,7.81,
                          7.74,7.75,7.77,7.78,7.80,7.81,7.84,NA,
                          7.71,7.71,7.74,7.79,7.81,7.85,7.87,7.91))
ponds2$cpond <- paste0("pond_",ponds2$pond)
ponds2$fpond <- as.factor(ponds2$pond)
ponds <- ponds2[complete.cases(ponds2),]

## p-value results from UniStat (http://www.unistat.com/guide/nonparametric-tests-kruskal-wallis-one-way-anova/)
unistat <- data.frame(Comparison=c("2-1","3-1","3-2","4-1","4-2","4-3"),
                      P.adj=c(0.1956,0.0191,1,0.0166,1,1))


## Test Messages ----
test_that("dunnTest() error and warning messages",{
  expect_error(dunnTest(ponds$pond,ponds$pH),"to a factor")
  expect_warning(dunnTest(ponds$pH,ponds$pond),"to a factor")
  expect_warning(dunnTest(ponds$pH,ponds$cpond),"to a factor")
  expect_error(dunnTest(pond~pH,data=ponds),"must be a factor")
  expect_warning(dunnTest(pH~pond,data=ponds),"to a factor")
  expect_warning(dunnTest(pH~cpond,data=ponds),"to a factor")
  expect_error(dunnTest(pH~pond+cpond,data=ponds),"only one RHS variable")
  expect_error(dunnTest(pH+cpond~pond,data=ponds))
  expect_warning(dunnTest(pH~fpond,data=ponds2),"Some rows deleted from")
})


## Test Output Types ----
test_that("dunnTest() output",{
  ## Loop through all methods in p.adjustment.methods
  lbls <- c("No Adjustment","Bonferroni","Sidak","Holm","Holm-Sidak",
            "Hochberg","Benjamini-Hochberg","Benjamini-Yekuteili")
  meths <- dunn.test::p.adjustment.methods
  for (i in seq_along(meths)) {  ## For two-sided cases
    tmp <- dunnTest(pH~fpond,data=ponds,method=meths[i],two.sided=TRUE)
    expect_true(is.list(tmp))
    expect_equal(names(tmp),c("method","res","dtres"))
    expect_equal(tmp$method,lbls[i])
    expect_equal(class(tmp$res),"data.frame")
    expect_equal(names(tmp$res),c("Comparison","Z","P.unadj","P.adj"))
  }
  for (i in seq_along(meths)) {  ## For one-sided cases
    tmp <- dunnTest(pH~fpond,data=ponds,method=meths[i],two.sided=FALSE)
    expect_true(is.list(tmp))
    expect_equal(names(tmp),c("method","res","dtres"))
    expect_equal(tmp$method,lbls[i])
    expect_equal(class(tmp$res),"data.frame")
    expect_equal(names(tmp$res),c("Comparison","Z","P.unadj","P.adj"))
  }
})

test_that("Check dunnTest() output labels",{
  ## This issue stems from a user complaint that labels for factors that don't
  ## exist (as would happen with subset() or filter()) mess up the results.
  ponds3 <- subset(ponds,pond!=3)
  tmp2 <- dunnTest(pH~fpond,data=subset(ponds,pond!=3))
  lvls <- as.character(unique(tmp2$res$Comparison))
  expect_false(any(grepl("3",lvls)))
  expect_true(any(grepl("1",lvls)))
  expect_true(any(grepl("2",lvls)))
  expect_true(any(grepl("4",lvls)))
})

## Validate Results ----
test_that("dunnTest matches UnitStat results",{
  tmp <- dunnTest(pH~fpond,data=ponds,method="bonferroni")
  expect_equal(round(tmp$res$P.adj,4),unistat$P.adj)
})

test_that("dunnTest matches dunn.test results for ponds data",{
  ## Loop through all methods in p.adjustment.methods
  for (m in dunn.test::p.adjustment.methods) { # for one-sided results
    tmp  <- dunnTest(pH~fpond,data=ponds,method=m,two.sided=FALSE)$res$P.adj
    junk <- utils::capture.output(
      tmp2 <- dunn.test::dunn.test(ponds$pH,ponds$fpond,method=m)$P.adjusted)
    expect_equal(tmp,tmp2)
  }
  for (m in dunn.test::p.adjustment.methods) { # for two-sided results
    tmp  <- dunnTest(pH~fpond,data=ponds,method=m,two.sided=TRUE)$res$P.adj
    junk <- utils::capture.output(
      tmp2 <- dunn.test::dunn.test(ponds$pH,ponds$fpond,method=m,altp=TRUE)$altP.adjusted)
    expect_equal(tmp,tmp2)
  }
  for (m in dunn.test::p.adjustment.methods) { # for one-sided results with missing data
    suppressWarnings(
      tmp  <- dunnTest(pH~fpond,data=ponds2,method=m,
                       two.sided=FALSE)$res$P.adj)
    junk <- utils::capture.output(
      tmp2 <- dunn.test::dunn.test(ponds2$pH,ponds2$fpond,method=m)$P.adjusted)
    expect_equal(tmp,tmp2)
  }
  for (m in dunn.test::p.adjustment.methods) { # for two-sided results with missing data
    suppressWarnings(
      tmp  <- dunnTest(pH~fpond,data=ponds2,method=m,
                       two.sided=TRUE)$res$P.adj)
    junk <- utils::capture.output(
      tmp2 <- dunn.test::dunn.test(ponds2$pH,ponds2$fpond,method=m,altp=TRUE)$altP.adjusted)
    expect_equal(tmp,tmp2)
  }
})

test_that("dunnTest matches dunn.test results for homecare data",{
  data(homecare,package="dunn.test")
  ## Loop through all methods in p.adjustment.methods
  for (m in dunn.test::p.adjustment.methods) { # for one-sided results
    tmp  <- dunnTest(occupation~eligibility,data=homecare,
                     method=m,two.sided=FALSE)$res$P.adj
    junk <- utils::capture.output(
      tmp2 <- dunn.test::dunn.test(homecare$occupation,homecare$eligibility,method=m)$P.adjusted)
    expect_equal(tmp,tmp2)
  }
  for (m in dunn.test::p.adjustment.methods) { # for two-sided results
    tmp  <- dunnTest(occupation~eligibility,data=homecare,method=m,
                     two.sided=TRUE)$res$P.adj
    junk <- utils::capture.output(
      tmp2 <- dunn.test::dunn.test(homecare$occupation,homecare$eligibility,
                                   method=m,altp=TRUE)$altP.adjusted)
    expect_equal(tmp,tmp2)
  }
})

test_that("dunnTest matches dunn.test results for airquality data",{
  data(airquality,package="datasets")
  ## Loop through all methods in p.adjustment.methods
  for (m in dunn.test::p.adjustment.methods) { # for one-sided results
    suppressWarnings(tmp <- dunnTest(Ozone~Month,data=airquality,
                                     method=m,two.sided=FALSE)$res$P.adj)
    junk <- utils::capture.output(
      tmp2 <- dunn.test::dunn.test(airquality$Ozone,airquality$Month,method=m)$P.adjusted)
    expect_equal(tmp,tmp2)
  }
  for (m in dunn.test::p.adjustment.methods) { # for two-sided results
    suppressWarnings(tmp <- dunnTest(Ozone~Month,data=airquality,
                                     method=m,two.sided=TRUE)$res$P.adj)
    junk <- utils::capture.output(
      tmp2 <- dunn.test::dunn.test(airquality$Ozone,airquality$Month,method=m,
                                   altp=TRUE)$altP.adjusted)
    expect_equal(tmp,tmp2)
  }
})
