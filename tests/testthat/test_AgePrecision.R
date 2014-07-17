context("Age Precision")

test_that("agePrecision gives correct precision values -- First Example",{
  data(WhitefishLC)
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  expect_that(ap1, is_a("agePrec"))
  expect_that(ap1$detail, is_a("data.frame"))
  expect_that(ap1$n, equals(151))
  expect_that(ap1$R, equals(2))
  expect_that(length(ap1$rawdiff), equals(17))
  expect_that(length(ap1$absdiff), equals(14))
  expect_that(round(ap1$APE,5), equals(14.92923))
  expect_that(round(ap1$CV,5), equals(21.11312))
  expect_that(round(ap1$PercAgree,5), equals(19.86755))
})

test_that("agePrecision gives correct precision values -- Second Example",{
  data(WhitefishLC)
  ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
  expect_that(ap2, is_a("agePrec"))
  expect_that(ap2$detail, is_a("data.frame"))
  expect_that(ap2$n, equals(151))
  expect_that(ap2$R, equals(3))
  expect_that(ap2$absdiff, is_a("table"))
  expect_that(dim(ap2$absdiff), equals(c(3,15)))
  expect_that(dim(ap2$rawdiff), equals(c(3,19)))
  expect_that(round(ap2$APE,5), equals(16.1851))
  expect_that(round(ap2$CV,5), equals(21.76877))
  expect_that(round(ap2$PercAgree,5), equals(12.58278))
})

test_that("agePrecision compared to http://www.nefsc.noaa.gov/fbp/age-prec/ calculations for AlewifeLH",{
  if (require(FSAdata)) {
    data(AlewifeLH)
    ap3 <- agePrecision(~otoliths+scales,data=AlewifeLH)
    expect_that(ap3$n, equals(104))
    expect_that(ap3$R, equals(2))
    expect_that(round(ap3$CV,2), equals(12.54))
    expect_that(round(ap3$PercAgree,1), equals(58.7))
  }
})

test_that("agePrecision errors and warnings",{
  data(WhitefishLC)
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  expect_that(summary(ap1,what="agreement"),throws_error())
})
