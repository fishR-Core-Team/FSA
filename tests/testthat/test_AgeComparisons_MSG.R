context("Age Precision and Bias MESSAGES")

test_that("ageBias() messages",{
  expect_error(ageBias(otolithC+scaleC~finrayC,data=WhitefishLC),
               "more than one variable on the LHS")
  expect_error(ageBias(otolithC~scaleC+finrayC,data=WhitefishLC),
               "must have only one RHS")
  
  suppressWarnings(ab1 <- ageBias(scaleC~otolithC,data=WhitefishLC))
  expect_error(summary(ab1,what="derek"),"should be one of")
  expect_error(summary(ab1,what="McNemar",cont.corr="derek"),"should be one of")
  expect_error(plot(ab1,xvals="derek"),"should be one of")
  expect_error(plotAB(ab1,what="derek"),"should be one of")
})

test_that("agePrecision() messages",{
  expect_error(agePrecision(~otolithC,data=WhitefishLC),
               "compare at least two sets of ages")
  expect_error(agePrecision(~otolithC+as.character(scaleC),data=WhitefishLC),
               "must be numeric")
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  expect_error(summary(ap1,what="agreement"),"should be one of")
  expect_error(summary(ap1,what="absolute",trunc.diff=0),"must be positive")
  expect_error(summary(ap1,what="absolute",trunc.diff=-2),"must be positive")
})
