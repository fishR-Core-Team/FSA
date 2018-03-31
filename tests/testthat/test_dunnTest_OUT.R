context("dunnTest() OUTPUT")
source("EXS_dunnTest.R")

test_that("dunnTest() output",{
  if (require(dunn.test)) {
    ## Loop through all methods in p.adjustment.methods
    lbls <- c("No Adjustment","Bonferroni","Sidak","Holm","Holm-Sidak",
              "Hochberg","Benjamini-Hochberg","Benjamini-Yekuteili")
    meths <- dunn.test::p.adjustment.methods
    for (i in seq_along(meths)) {  ## For two-sided cases
      tmp <- dunnTest(pH~fpond,data=ponds,method=meths[i],two.sided=TRUE)
      expect_true(is.list(tmp))
      expect_equal(names(tmp),c("method","res","dtres"))
      expect_equal(tmp$method,lbls[i])
      expect_is(tmp$res,"data.frame")
      expect_equal(names(tmp$res),c("Comparison","Z","P.unadj","P.adj"))
    }
    for (i in seq_along(meths)) {  ## For one-sided cases
      tmp <- dunnTest(pH~fpond,data=ponds,method=meths[i],two.sided=FALSE)
      expect_true(is.list(tmp))
      expect_equal(names(tmp),c("method","res","dtres"))
      expect_equal(tmp$method,lbls[i])
      expect_is(tmp$res,"data.frame")
      expect_equal(names(tmp$res),c("Comparison","Z","P.unadj","P.adj"))
    }
  }
})

test_that("Check dunnTest() output labels",{
  ## This issue stems from a user complaint that labels for factors that
  ## don't exist (as would happen from using subset() or filter() screw
  ## up the results.
  ponds3 <- subset(ponds,pond!=3)
  tmp2 <- dunnTest(pH~fpond,data=subset(ponds,pond!=3))
  expect_false(any(grepl("3",levels(tmp2$res$Comparison))))
  expect_true(any(grepl("1",levels(tmp2$res$Comparison))))
  expect_true(any(grepl("2",levels(tmp2$res$Comparison))))
  expect_true(any(grepl("4",levels(tmp2$res$Comparison))))
})