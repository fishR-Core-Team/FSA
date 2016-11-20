context("Age Comparisons (Precision and Bias) MESSAGES")
data(WhitefishLC)

test_that("ageBias() messages",{
  expect_error(ageBias(otolithC+scaleC~finrayC,data=WhitefishLC),"more than one variable on the LHS")
  expect_error(ageBias(otolithC~scaleC+finrayC,data=WhitefishLC),"must have only one RHS")
  
  suppressWarnings(ab1 <- ageBias(scaleC~otolithC,data=WhitefishLC))
  expect_error(summary(ab1,what="derek"),"should be one of")
  expect_error(summary(ab1,what="McNemar",cont.corr="derek"),"should be one of")
  expect_error(plot(ab1,what="derek"),"should be one of")
  
  expect_message(summary(ab1),"Sample size in the age agreement table")
  expect_message(summary(ab1),"Summary of scaleC by otolithC")
  expect_message(summary(ab1),"Summary of scaleC-otolithC by otolithC")
  expect_message(summary(ab1),"square")
  expect_message(summary(ab1,flip.table=TRUE),"flipped")
  expect_message(summary(ab1),"Age agreement table symmetry test results")
})

test_that("agePrecision() messages",{
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  
  expect_error(summary(ap1,what="agreement"),"should be one of")
  expect_error(summary(ap1,what="absolute",trunc.diff=0),"must be positive")
  expect_error(summary(ap1,what="absolute",trunc.diff=-2),"must be positive")
  
  expect_message(summary(ap1),"Precision summary statistics")
  expect_message(summary(ap1),"Percentage of fish by absolute differences in ages")
  expect_message(summary(ap1),"Percentage of fish by differences in ages")
  expect_message(summary(ap1),"Intermediate calculations for each individual")
})
