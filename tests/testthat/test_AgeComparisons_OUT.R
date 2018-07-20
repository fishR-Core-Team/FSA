context("Age Precision and Bias OUTPUT")

test_that("ageBias() summary() output titles",{
  suppressWarnings(ab1 <- ageBias(scaleC~otolithC,data=WhitefishLC))
  tmp <- capture.output( summary(ab1) )
  expect_true(any(grepl("Sample size in the age agreement table",tmp)))
  expect_true(any(grepl("Summary of scaleC by otolithC",tmp)))
  expect_true(any(grepl("Summary of scaleC-otolithC by otolithC",tmp)))
  expect_true(any(grepl("Raw agreement table",tmp)))
  expect_true(any(grepl("(square)",tmp)))
  expect_true(any(grepl("Age agreement table symmetry test results",tmp)))
  tmp <- capture.output( summary(ab1,flip.table=TRUE) )
  expect_true(any(grepl("Sample size in the age agreement table",tmp)))
  expect_true(any(grepl("Summary of scaleC by otolithC",tmp)))
  expect_true(any(grepl("Summary of scaleC-otolithC by otolithC",tmp)))
  expect_true(any(grepl("Raw agreement table",tmp)))
  expect_true(any(grepl("flipped",tmp)))
  expect_true(any(grepl("Age agreement table symmetry test results",tmp)))
  tmp <- capture.output( summary(ab1,what="n") )
  expect_true(any(grepl("Sample size in the age agreement table",tmp)))
  tmp <- capture.output( summary(ab1,what=c("n","bias")) )
  expect_true(any(grepl("Sample size in the age agreement table",tmp)))
  expect_true(any(grepl("Summary of scaleC by otolithC",tmp)))
})

test_that("agePrecision() summary() output titles",{
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  tmp <- capture.output( summary(ap1) )
  expect_true(any(grepl("Precision summary statistics",tmp)))
  expect_true(any(grepl("Percentage of fish by absolute differences in ages",tmp)))
  expect_true(any(grepl("Percentage of fish by differences in ages",tmp)))
  expect_true(any(grepl("Intermediate calculations for each individual",tmp)))
  tmp <- capture.output( summary(ap1,percent=FALSE) )
  expect_true(any(grepl("Precision summary statistics",tmp)))
  expect_true(any(grepl("Frequency of fish by absolute differences in ages",tmp)))
  expect_true(any(grepl("Frequency of fish by differences in ages",tmp)))
  expect_true(any(grepl("Intermediate calculations for each individual",tmp)))
  tmp <- capture.output( summary(ap1,what=c("precision","absolute")) )
  expect_true(any(grepl("Precision summary statistics",tmp)))
  expect_true(any(grepl("Percentage of fish by absolute differences in ages",tmp)))
})

test_that("agePrecision() types and specifics",{
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  expect_is(ap1,"agePrec")
  expect_equal(names(ap1),c("detail","rawdiff","absdiff","ASD","ACV","ACV2",
                            "AAAD","AAPE","AAPE2","PercAgree","R","n","validn"))
  expect_is(ap1$detail,"data.frame")
  expect_equal(names(ap1$detail),
               c("otolithC","scaleC","mean","median","mode",
                 "SD","CV","CV2","AAD","APE","APE2"))
  expect_is(ap1$rawdiff,"table")
  expect_is(ap1$absdiff,"table")
  
  ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
  expect_is(ap2,"agePrec")
  expect_equal(names(ap2),c("detail","rawdiff","absdiff","ASD","ACV","ACV2",
                            "AAAD","AAPE","AAPE2","PercAgree","R","n","validn"))
  expect_is(ap2$detail,"data.frame")
  expect_equal(names(ap2$detail),
               c("otolithC","finrayC","scaleC","mean","median","mode",
                 "SD","CV","CV2","AAD","APE","APE2"))
  expect_is(ap2$rawdiff,"table")
  expect_is(ap2$absdiff,"table")
})