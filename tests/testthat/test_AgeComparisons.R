context("Age Comparisons (Precision and Bias)")

# ############################################################
# ============================================================
# Messaging
# ============================================================
# ############################################################

test_that("agePrecision errors and warnings",{
  data(WhitefishLC)
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  expect_error(summary(ap1,what="agreement"))
})

test_that("ageBias errors and warnings",{
  data(WhitefishLC)
  ## Two variables on LHS
  expect_error(ageBias(otolithC+scaleC~finrayC,data=WhitefishLC))
  ## Two variables on RHS
  expect_error(ageBias(otolithC~scaleC+finrayC,data=WhitefishLC))
})


# ############################################################
# ============================================================
# Analytical Results
# ============================================================
# ############################################################

test_that("agePrecision gives correct precision values -- First Example",{
  data(WhitefishLC)
  ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
  expect_is(ap1,"agePrec")
  expect_is(ap1$detail,"data.frame")
  expect_equal(ap1$n, 151)
  expect_equal(ap1$R, 2)
  expect_equal(length(ap1$rawdiff), 17)
  expect_equal(length(ap1$absdiff), 14)
  expect_equal(round(ap1$APE,5), 14.92923)
  expect_equal(round(ap1$ACV,5), 21.11312)
  expect_equal(round(ap1$PercAgree,5), 19.86755)
})

test_that("agePrecision gives correct precision values -- Second Example",{
  data(WhitefishLC)
  ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
  expect_is(ap2,"agePrec")
  expect_is(ap2$detail,"data.frame")
  expect_equal(ap2$n, 151)
  expect_equal(ap2$R, 3)
  expect_is(ap2$absdiff,"table")
  expect_equal(dim(ap2$absdiff), c(3,15))
  expect_equal(dim(ap2$rawdiff), c(3,19))
  expect_equal(round(ap2$APE,5), 16.1851)
  expect_equal(round(ap2$ACV,5), 21.76877)
  expect_equal(round(ap2$PercAgree,5), 12.58278)
})

test_that("agePrecision compared to http://www.nefsc.noaa.gov/fbp/age-prec/ calculations for AlewifeLH",{
  if (require(FSAdata)) {
    data(AlewifeLH)
    ap3 <- agePrecision(~otoliths+scales,data=AlewifeLH)
    expect_equal(ap3$n, 104)
    expect_equal(ap3$R, 2)
    expect_equal(round(ap3$ACV,2), 12.54)
    expect_equal(round(ap3$PercAgree,1), 58.7)
  }
})

test_that("ageBias symmetry tests match the results in Evans and Hoenig (2008)",{
  ######## Create Evans & Hoenig (2008) X, Y, and Z matrices and check
  ########   against the results in table 1.
  X.dat <- data.frame(ageR=c(2,2,2,2,2,2,2,2),
                      ageC=c(1,1,1,1,3,3,3,3))
  X <- ageBias(ageR~ageC,data=X.dat)
  Xsum <- summary(X,what="symmetry")
  expect_equal(Xsum[Xsum$symTest=="McNemars","df"], 1)
  expect_equal(Xsum[Xsum$symTest=="McNemars","chi.sq"], 0)
  expect_equal(round(Xsum[Xsum$symTest=="McNemars","p"],4), 1.0000)
  ## note that Evans & Hoenigs (2008) show a df=2 for this case, but compare2()
  ##   in the fishmethods package, which was written by Hoenig, shows df=1.  I,
  ##   therefore, tested against df=1.
  expect_equal(Xsum[Xsum$symTest=="EvansHoenig","df"], 1)
  expect_equal(Xsum[Xsum$symTest=="EvansHoenig","chi.sq"], 0)
  expect_equal(round(Xsum[Xsum$symTest=="EvansHoenig","p"],4), 1.0000)
  expect_equal(Xsum[Xsum$symTest=="Bowkers","df"], 2)
  expect_equal(Xsum[Xsum$symTest=="Bowkers","chi.sq"], 8)
  expect_equal(round(Xsum[Xsum$symTest=="Bowkers","p"],4), 0.0183)
  
  Y.dat <- data.frame(ageR=c(1,1,1,2,2,2),
                      ageC=c(2,2,3,3,3,3))
  Y <- ageBias(ageR~ageC,data=Y.dat)
  Ysum <- summary(Y,what="symmetry")
  expect_equal(Ysum[Ysum$symTest=="McNemars","df"], 1)
  expect_equal(Ysum[Ysum$symTest=="McNemars","chi.sq"], 6)
  expect_equal(round(Ysum[Ysum$symTest=="McNemars","p"],4), 0.0143)
  expect_equal(Ysum[Ysum$symTest=="EvansHoenig","df"], 2)
  expect_equal(Ysum[Ysum$symTest=="EvansHoenig","chi.sq"], 6)
  expect_equal(round(Ysum[Ysum$symTest=="EvansHoenig","p"],4), 0.0498)
  expect_equal(Ysum[Ysum$symTest=="Bowkers","df"], 3)
  expect_equal(Ysum[Ysum$symTest=="Bowkers","chi.sq"], 6)
  expect_equal(round(Ysum[Ysum$symTest=="Bowkers","p"],4), 0.1116)
  
  Z.dat <- data.frame(ageR=c(1,1,1,2,2,2,3),
                      ageC=c(2,2,2,3,3,3,1))
  Z <- ageBias(ageR~ageC,data=Z.dat)
  Zsum <- summary(Z,what="symmetry")
  expect_equal(Zsum[Zsum$symTest=="McNemars","df"], 1)
  expect_equal(round(Zsum[Zsum$symTest=="McNemars","chi.sq"],2), 3.57)
  expect_equal(round(Zsum[Zsum$symTest=="McNemars","p"],4), 0.0588)
  expect_equal(Zsum[Zsum$symTest=="EvansHoenig","df"], 2)
  expect_equal(Zsum[Zsum$symTest=="EvansHoenig","chi.sq"], 7)
  expect_equal(round(Zsum[Zsum$symTest=="EvansHoenig","p"],4), 0.0302)
  expect_equal(Zsum[Zsum$symTest=="Bowkers","df"], 3)
  expect_equal(Zsum[Zsum$symTest=="Bowkers","chi.sq"], 7)
  expect_equal(round(Zsum[Zsum$symTest=="Bowkers","p"],4), 0.0719)
})

test_that("test AlewifeLH data against compare2() results",{
  if (require(FSAdata) & require(fishmethods)) {
    data(AlewifeLH)
    ab2 <- compare2(AlewifeLH,barplot=FALSE)
    ## no continuity correction
    ab1 <- ageBias(scales~otoliths,data=AlewifeLH,ref.lab="Otolith Age",nref.lab="Scale Age")
    ab1sum <- summary(ab1)
    expect_equal(ab1sum[ab1sum$symTest=="McNemars","chi.sq"], ab2$McNemar$Chisq)
    expect_equal(ab1sum[ab1sum$symTest=="McNemars","p"], ab2$McNemar$pvalue)
    expect_equal(ab1sum[ab1sum$symTest=="EvansHoenig","chi.sq"], ab2$Evans_Hoenig$Chisq)
    expect_equal(ab1sum[ab1sum$symTest=="EvansHoenig","p"], ab2$Evans_Hoenig$pvalue)
    expect_equal(ab1sum[ab1sum$symTest=="EvansHoenig","df"], ab2$Evans_Hoenig$df)
    ## Yates continuity correction
    ab1sum2 <- summary(ab1,what="McNemars",cont.corr="Yates")
    expect_equal(ab1sum2[1,"chi.sq"], ab2$McNemar_continuity_correction$Chisq)
    expect_equal(ab1sum2[1,"p"], ab2$McNemar_continuity_correction$pvalue)
    ## Edwards continuity correction
    ab3 <- compare2(AlewifeLH,correct="Edwards",barplot=FALSE)
    ab1sum3 <- summary(ab1,what="McNemars",cont.corr="Edwards")
    expect_equal(ab1sum3[1,"chi.sq"], ab3$McNemar_continuity_correction$Chisq)
    expect_equal(ab1sum3[1,"p"], ab3$McNemar_continuity_correction$pvalue)
  }
})

test_that("ageBias compared to http://www.nefsc.noaa.gov/fbp/age-prec/ calculations for AlewifeLH",{
  if (require(FSAdata)) {
    data(AlewifeLH)
    ab1 <- ageBias(scales~otoliths,data=AlewifeLH,ref.lab="Otolith Age",nref.lab="Scale Age")
    expect_equal(ab1$bias$n, c(2,18,20,13,18,10,8,7,5,1,2))
    ## the fbp result is actually 4.62 for age-6
    expect_equal(round(ab1$bias$mean,2), c(0.00,1.11,2.20,2.85,3.78,4.20,4.62,5.00,4.80,6.00,6.00))
  }
})
