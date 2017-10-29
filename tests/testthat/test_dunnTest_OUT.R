context("dunnTest() OUTPUT")
source("EXS_dunnTest.R")

test_that("dunnTest() output",{
  if (require(dunn.test)) {
    ## Loop through all methods in p.adjustment.methods
    lbls <- c("No Adjustment","Bonferroni","Sidak","Holm","Holm-Sidak","Hochberg","Benjamini-Hochberg","Benjamini-Yekuteili")
    meths <- dunn.test::p.adjustment.methods
    for (i in 1:length(meths)) {  ## For two-sided cases
      tmp <- dunnTest(pH~fpond,data=ponds,method=meths[i],two.sided=TRUE)
      expect_true(is.list(tmp))
      expect_equal(names(tmp),c("method","res","dtres"))
      expect_equal(tmp$method,lbls[i])
      expect_is(tmp$res,"data.frame")
      expect_equal(names(tmp$res),c("Comparison","Z","P.unadj","P.adj"))
    }
    for (i in 1:length(meths)) {  ## For one-sided cases
      tmp <- dunnTest(pH~fpond,data=ponds,method=meths[i],two.sided=FALSE)
      expect_true(is.list(tmp))
      expect_equal(names(tmp),c("method","res","dtres"))
      expect_equal(tmp$method,lbls[i])
      expect_is(tmp$res,"data.frame")
      expect_equal(names(tmp$res),c("Comparison","Z","P.unadj","P.adj"))
    }
  }
})
