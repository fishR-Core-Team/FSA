
## Test Messages ----
test_that("ageBias() messages",{
  expect_error(ageBias(otolithC+scaleC~finrayC,data=WhitefishLC),
               "more than one variable on the LHS")
  expect_error(ageBias(otolithC~scaleC+finrayC,data=WhitefishLC),
               "must have only one RHS")
  
  suppressWarnings(ab1 <- ageBias(scaleC~otolithC,data=WhitefishLC))
  expect_error(summary(ab1,what="derek"),
               "should be one of")
  expect_error(summary(ab1,what="McNemar",cont.corr="derek"),
               "should be one of")
  expect_error(plot(ab1,xvals="derek"),
               "should be one of")
  expect_error(plotAB(ab1,what="derek"),
               "should be one of")
})

test_that("agePrecision() messages",{
  expect_error(agePrecision(~otolithC,data=WhitefishLC),
               "compare at least two sets of ages")
  expect_error(agePrecision(~otolithC+as.character(scaleC),data=WhitefishLC),
               "must be numeric")
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  expect_error(summary(ap1,what="agreement"),
               "should be one of")
  expect_error(summary(ap1,what="absolute",trunc.diff=0),
               "must be positive")
  expect_error(summary(ap1,what="absolute",trunc.diff=-2),
               "must be positive")
})


## Test Output Types ----
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
                            "AAD","APE","APE2","AD","PercAgree","R","n","validn"))
  expect_is(ap1$detail,"data.frame")
  expect_equal(names(ap1$detail),
               c("otolithC","scaleC","mean","median","mode",
                 "SD","CV","CV2","AD","PE","PE2","D"))
  expect_is(ap1$rawdiff,"table")
  expect_is(ap1$absdiff,"table")
  
  ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
  expect_is(ap2,"agePrec")
  expect_equal(names(ap2),c("detail","rawdiff","absdiff","ASD","ACV","ACV2",
                            "AAD","APE","APE2","AD","PercAgree","R","n","validn"))
  expect_is(ap2$detail,"data.frame")
  expect_equal(names(ap2$detail),
               c("otolithC","finrayC","scaleC","mean","median","mode",
                 "SD","CV","CV2","AD","PE","PE2","D"))
  expect_is(ap2$rawdiff,"table")
  expect_is(ap2$absdiff,"table")
})

## Validate Results ----
test_that("ageBias() symmetry tests match results in Evans and Hoenig (2008)",{
  ######## Create Evans & Hoenig (2008) X, Y, and Z matrices and check
  ########   against the results in table 1.
  X.dat <- data.frame(ageR=c(2,2,2,2,2,2,2,2),
                      ageC=c(1,1,1,1,3,3,3,3))
  suppressWarnings(X <- ageBias(ageR~ageC,data=X.dat))
  junk <- capture.output( Xsum <- summary(X,what="symmetry") )
  expect_equal(Xsum[Xsum$symTest=="McNemar","df"], 1)
  expect_equal(Xsum[Xsum$symTest=="McNemar","chi.sq"], 0)
  expect_equal(round(Xsum[Xsum$symTest=="McNemar","p"],4), 1.0000)
  ## note that Evans & Hoenigs (2008) show a df=2 for this case, but compare2()
  ##   in the fishmethods package, which was written by Hoenig, shows df=1.  I,
  ##   therefore, tested against df=1.
  expect_equal(Xsum[Xsum$symTest=="EvansHoenig","df"], 1)
  expect_equal(Xsum[Xsum$symTest=="EvansHoenig","chi.sq"], 0)
  expect_equal(round(Xsum[Xsum$symTest=="EvansHoenig","p"],4), 1.0000)
  expect_equal(Xsum[Xsum$symTest=="Bowker","df"], 2)
  expect_equal(Xsum[Xsum$symTest=="Bowker","chi.sq"], 8)
  expect_equal(round(Xsum[Xsum$symTest=="Bowker","p"],4), 0.0183)
  
  Y.dat <- data.frame(ageR=c(1,1,1,2,2,2),
                      ageC=c(2,2,3,3,3,3))
  suppressWarnings(Y <- ageBias(ageR~ageC,data=Y.dat))
  junk <- capture.output( Ysum <- summary(Y,what="symmetry") )
  expect_equal(Ysum[Ysum$symTest=="McNemar","df"], 1)
  expect_equal(Ysum[Ysum$symTest=="McNemar","chi.sq"], 6)
  expect_equal(round(Ysum[Ysum$symTest=="McNemar","p"],4), 0.0143)
  expect_equal(Ysum[Ysum$symTest=="EvansHoenig","df"], 2)
  expect_equal(Ysum[Ysum$symTest=="EvansHoenig","chi.sq"], 6)
  expect_equal(round(Ysum[Ysum$symTest=="EvansHoenig","p"],4), 0.0498)
  expect_equal(Ysum[Ysum$symTest=="Bowker","df"], 3)
  expect_equal(Ysum[Ysum$symTest=="Bowker","chi.sq"], 6)
  expect_equal(round(Ysum[Ysum$symTest=="Bowker","p"],4), 0.1116)
  
  Z.dat <- data.frame(ageR=c(1,1,1,2,2,2,3),
                      ageC=c(2,2,2,3,3,3,1))
  suppressWarnings(Z <- ageBias(ageR~ageC,data=Z.dat))
  junk <- capture.output( Zsum <- summary(Z,what="symmetry") )
  expect_equal(Zsum[Zsum$symTest=="McNemar","df"], 1)
  expect_equal(round(Zsum[Zsum$symTest=="McNemar","chi.sq"],2), 3.57)
  expect_equal(round(Zsum[Zsum$symTest=="McNemar","p"],4), 0.0588)
  expect_equal(Zsum[Zsum$symTest=="EvansHoenig","df"], 2)
  expect_equal(Zsum[Zsum$symTest=="EvansHoenig","chi.sq"], 7)
  expect_equal(round(Zsum[Zsum$symTest=="EvansHoenig","p"],4), 0.0302)
  expect_equal(Zsum[Zsum$symTest=="Bowker","df"], 3)
  expect_equal(Zsum[Zsum$symTest=="Bowker","chi.sq"], 7)
  expect_equal(round(Zsum[Zsum$symTest=="Bowker","p"],4), 0.0719)
})

test_that("test ageBias() against compare2() with AlewifeLH data",{
  if (require(FSAdata) & require(fishmethods)) {
    data(AlewifeLH,package="FSAdata")
    ab2 <- compare2(AlewifeLH,barplot=FALSE)
    ## no continuity correction
    suppressWarnings(ab1 <- ageBias(scales~otoliths,data=AlewifeLH,
                                    ref.lab="Otolith Age",nref.lab="Scale Age"))
    junk <- capture.output( ab1sum <- summary(ab1) )
    expect_equal(ab1sum[ab1sum$symTest=="McNemar","chi.sq"], ab2$McNemar$Chisq)
    expect_equal(ab1sum[ab1sum$symTest=="McNemar","p"], ab2$McNemar$pvalue)
    expect_equal(ab1sum[ab1sum$symTest=="EvansHoenig","chi.sq"],
                 ab2$Evans_Hoenig$Chisq)
    expect_equal(ab1sum[ab1sum$symTest=="EvansHoenig","p"],
                 ab2$Evans_Hoenig$pvalue)
    expect_equal(ab1sum[ab1sum$symTest=="EvansHoenig","df"],
                 ab2$Evans_Hoenig$df)
    ## Yates continuity correction
    junk <- capture.output( ab1sum2 <- summary(ab1,what="McNemar",
                                               cont.corr="Yates") )
    expect_equal(ab1sum2[1,"chi.sq"], ab2$McNemar_continuity_correction$Chisq)
    expect_equal(ab1sum2[1,"p"], ab2$McNemar_continuity_correction$pvalue)
    ## Edwards continuity correction
    ab3 <- compare2(AlewifeLH,correct="Edwards",barplot=FALSE)
    junk <- capture.output( ab1sum3 <- summary(ab1,what="McNemar",
                                               cont.corr="Edwards") )
    expect_equal(ab1sum3[1,"chi.sq"], ab3$McNemar_continuity_correction$Chisq)
    expect_equal(ab1sum3[1,"p"], ab3$McNemar_continuity_correction$pvalue)
  }
})

test_that("ageBias() compared to http://www.nefsc.noaa.gov/fbp/age-prec/ calculations for AlewifeLH data",{
  if (require(FSAdata)) {
    data(AlewifeLH,package="FSAdata")
    suppressWarnings(ab1 <- ageBias(scales~otoliths,data=AlewifeLH,
                                    ref.lab="Otolith Age",nref.lab="Scale Age"))
    expect_equal(ab1$bias$n, c(2,18,20,13,18,10,8,7,5,1,2))
    ## the fbp result is actually 4.62 for age-6
    expect_equal(round(ab1$bias$mean,2), c(0.00,1.11,2.20,2.85,3.78,4.20,
                                           4.62,5.00,4.80,6.00,6.00))
  }
})

test_that("agePrecision() gives correct precision values -- First Example",{
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  expect_equal(ap1$n, 151)
  expect_equal(ap1$R, 2)
  expect_equal(length(ap1$rawdiff), 17)
  expect_equal(length(ap1$absdiff), 14)
  expect_equal(round(ap1$APE,5), 14.92923)
  expect_equal(round(ap1$ACV,5), 21.11312)
  expect_equal(round(ap1$PercAgree,5), 19.86755)
})

test_that("agePrecision() gives correct precision values -- Second Example",{
  ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
  expect_equal(ap2$n, 151)
  expect_equal(ap2$R, 3)
  expect_equal(dim(ap2$absdiff), c(3,15))
  expect_equal(dim(ap2$rawdiff), c(3,19))
  expect_equal(round(ap2$APE,5), 16.1851)
  expect_equal(round(ap2$ACV,5), 21.76877)
  expect_equal(round(ap2$PercAgree,5), 12.58278)
  
  ## testing truncation of absolute differences
  # with multiple pairs of structures
  junk <- capture.output( sum1 <- summary(ap2,what="absolute") )
  expect_equal(dim(sum1), c(3,15))
  junk <- capture.output( sum2 <- summary(ap2,what="absolute",trunc.diff=4) )
  expect_equal(dim(sum2), c(3,5))
  expect_equal(sum1[,1:4],sum2[,1:4])
  expect_equal(rowSums(sum1[,5:15]),sum2[,5])
  # with one pair of structures
  ap2 <- agePrecision(~otolithC+finrayC,data=WhitefishLC)
  junk <- capture.output( sum1 <- summary(ap2,what="absolute") )
  expect_equal(dim(sum1),15)
  junk <- capture.output( sum2 <- summary(ap2,what="absolute",trunc.diff=4) )
  expect_equal(dim(sum2),5)
  expect_equal(sum1[1:4],sum2[1:4])
  expect_equivalent(sum(sum1[5:15]),sum2[5])
})

test_that("agePrecision() compared to http://www.nefsc.noaa.gov/fbp/age-prec/ calculations for AlewifeLH data",{
  if (require(FSAdata)) {
    data(AlewifeLH,package="FSAdata")
    ap3 <- agePrecision(~otoliths+scales,data=AlewifeLH)
    expect_equal(ap3$n, 104)
    expect_equal(ap3$R, 2)
    expect_equal(round(ap3$ACV,2), 12.54)
    expect_equal(round(ap3$PercAgree,1), 58.7)
  }
})

test_that("agePrecision() differences for simple data",{
  tmp <- data.frame(age1=c(1,1,1,1,2,2),
                    age2=c(1,1,2,2,2,3))
  ap4 <- agePrecision(~age1+age2,data=tmp)
  expect_equal(ap4$n, 6)
  expect_equal(ap4$R, 2)
  expect_equal(names(ap4$rawdiff), c("-1","0"))
  expect_equal(as.numeric(ap4$rawdiff), c(3,3))
  expect_equal(names(ap4$absdiff), c("0","1"))
  expect_equal(as.numeric(ap4$absdiff), c(3,3))
  tmp2 <- ap4$detail
  expect_equal(tmp2$mode,c(1,1,NA,NA,2,NA))
  expect_equal(tmp2$mean,as.vector(rowMeans(tmp[,1:2])))
  expect_equal(tmp2$mean,tmp2$median)
})

test_that("agePrecision() differences for simple data with NA values",{
  tmp <- data.frame(age1=c(1,1,1,1,2,2,2,2,3,3,3,3),
                    age2=c(1,1,2,2,2,2,3,3,3,3,4,4),
                    age3=c(1,1,2,NA,2,2,3,NA,3,3,4,NA),
                    age4=c(1,1,2,NA,2,2,3,NA,3,3,4,NA),
                    age5=c(NA,1,2,2,NA,2,3,3,NA,3,4,4))
  ap12 <- agePrecision(~age1+age2,data=tmp)
  expect_equal(ap12$PercAgree,50)
  ap13 <- agePrecision(~age1+age3,data=tmp)
  expect_equal(round(ap13$PercAgree,4),66.6667)
  ap14 <- agePrecision(~age1+age4,data=tmp)
  expect_equal(round(ap14$PercAgree,4),66.6667)
  ap15 <- agePrecision(~age1+age5,data=tmp)
  expect_equal(round(ap15$PercAgree,4),33.3333)
  ap23 <- agePrecision(~age2+age3,data=tmp)
  expect_equal(ap23$PercAgree,100)
  ap24 <- agePrecision(~age2+age4,data=tmp)
  expect_equal(ap24$PercAgree,100)
  ap25 <- agePrecision(~age2+age5,data=tmp)
  expect_equal(ap25$PercAgree,100)
  ap34 <- agePrecision(~age3+age4,data=tmp)
  expect_equal(ap34$PercAgree,100)
  ap35 <- agePrecision(~age3+age5,data=tmp)
  expect_equal(ap35$PercAgree,100)
  ap45 <- agePrecision(~age4+age5,data=tmp)
  expect_equal(ap45$PercAgree,100)
  ap123 <- agePrecision(~age1+age2+age3,data=tmp)
  expect_equal(round(ap123$PercAgree,4),66.6667)
  expect_equal(ap123$detail$mode,c(1,1,2,NA,2,2,3,NA,3,3,4,NA))
  ap125 <- agePrecision(~age1+age2+age5,data=tmp)
  expect_equal(round(ap125$PercAgree,4),33.3333)
  ap135 <- agePrecision(~age1+age3+age5,data=tmp)
  expect_equal(round(ap135$PercAgree,4),50.0000)
})

