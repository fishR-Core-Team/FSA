context("Age Bias")

test_that("ageBias symmetry tests match the results in Evans and Hoenig (2008)",{
  ######## Create Evans & Hoenig (2008) X, Y, and Z matrices and check
  ########   against the results in table 1.
  X.dat <- data.frame(ageR=c(2,2,2,2,2,2,2,2),
                      ageC=c(1,1,1,1,3,3,3,3))
  X <- ageBias(ageC~ageR,data=X.dat)
  Xsum <- summary(X,what="symmetry")
  expect_that(Xsum[Xsum$symTest=="McNemars","df"], equals(1))
  expect_that(Xsum[Xsum$symTest=="McNemars","chi.sq"], equals(0))
  expect_that(round(Xsum[Xsum$symTest=="McNemars","p"],4), equals(1.0000))
  ## note that Evans & Hoenigs (2008) show a df=2 for this case, but compare2()
  ##   in the fishmethods package, which was written by Hoenig, shows df=1.  I,
  ##   therefore, tested against df=1.
  expect_that(Xsum[Xsum$symTest=="EvansHoenig","df"], equals(1))
  expect_that(Xsum[Xsum$symTest=="EvansHoenig","chi.sq"], equals(0))
  expect_that(round(Xsum[Xsum$symTest=="EvansHoenig","p"],4), equals(1.0000))
  expect_that(Xsum[Xsum$symTest=="Bowkers","df"], equals(2))
  expect_that(Xsum[Xsum$symTest=="Bowkers","chi.sq"], equals(8))
  expect_that(round(Xsum[Xsum$symTest=="Bowkers","p"],4), equals(0.0183))
  
  Y.dat <- data.frame(ageR=c(1,1,1,2,2,2),
                      ageC=c(2,2,3,3,3,3))
  Y <- ageBias(ageC~ageR,data=Y.dat)
  summary(Y,what="table",zero.print=0)
  Ysum <- summary(Y,what="symmetry")
  expect_that(Ysum[Ysum$symTest=="McNemars","df"], equals(1))
  expect_that(Ysum[Ysum$symTest=="McNemars","chi.sq"], equals(6))
  expect_that(round(Ysum[Ysum$symTest=="McNemars","p"],4), equals(0.0143))
  expect_that(Ysum[Ysum$symTest=="EvansHoenig","df"], equals(2))
  expect_that(Ysum[Ysum$symTest=="EvansHoenig","chi.sq"], equals(6))
  expect_that(round(Ysum[Ysum$symTest=="EvansHoenig","p"],4), equals(0.0498))
  expect_that(Ysum[Ysum$symTest=="Bowkers","df"], equals(3))
  expect_that(Ysum[Ysum$symTest=="Bowkers","chi.sq"], equals(6))
  expect_that(round(Ysum[Ysum$symTest=="Bowkers","p"],4), equals(0.1116))
  
  Z.dat <- data.frame(ageR=c(1,1,1,2,2,2,3),
                      ageC=c(2,2,2,3,3,3,1))
  Z <- ageBias(ageC~ageR,data=Z.dat)
  Zsum <- summary(Z,what="symmetry")
  expect_that(Zsum[Zsum$symTest=="McNemars","df"], equals(1))
  expect_that(round(Zsum[Zsum$symTest=="McNemars","chi.sq"],2), equals(3.57))
  expect_that(round(Zsum[Zsum$symTest=="McNemars","p"],4), equals(0.0588))
  expect_that(Zsum[Zsum$symTest=="EvansHoenig","df"], equals(2))
  expect_that(Zsum[Zsum$symTest=="EvansHoenig","chi.sq"], equals(7))
  expect_that(round(Zsum[Zsum$symTest=="EvansHoenig","p"],4), equals(0.0302))
  expect_that(Zsum[Zsum$symTest=="Bowkers","df"], equals(3))
  expect_that(Zsum[Zsum$symTest=="Bowkers","chi.sq"], equals(7))
  expect_that(round(Zsum[Zsum$symTest=="Bowkers","p"],4), equals(0.0719))
})

test_that("test AlewifeLH data against compare2() results",{
  if (require(FSAdata) & require(fishmethods)) {
    data(AlewifeLH)
    ab2 <- compare2(AlewifeLH,barplot=FALSE)
    ## no continuity correction
    ab1 <- ageBias(otoliths~scales,data=AlewifeLH,ref.lab="Otolith Age",nref.lab="Scale Age")
    ab1sum <- summary(ab1)
    expect_that(ab1sum[ab1sum$symTest=="McNemars","chi.sq"],equals(ab2$McNemar$Chisq))
    expect_that(ab1sum[ab1sum$symTest=="McNemars","p"],equals(ab2$McNemar$pvalue))
    expect_that(ab1sum[ab1sum$symTest=="EvansHoenig","chi.sq"],equals(ab2$Evans_Hoenig$Chisq))
    expect_that(ab1sum[ab1sum$symTest=="EvansHoenig","p"],equals(ab2$Evans_Hoenig$pvalue))
    expect_that(ab1sum[ab1sum$symTest=="EvansHoenig","df"],equals(ab2$Evans_Hoenig$df))
    ## Yates continuity correction
    ab1sum2 <- summary(ab1,what="McNemars",cont.corr="Yates")
    expect_that(ab1sum2[1,"chi.sq"],equals(ab2$McNemar_continuity_correction$Chisq))
    expect_that(ab1sum2[1,"p"],equals(ab2$McNemar_continuity_correction$pvalue))
    ## Edwards continuity correction
    ab3 <- compare2(AlewifeLH,correct="Edwards",barplot=FALSE)
    ab1sum3 <- summary(ab1,what="McNemars",cont.corr="Edwards")
    expect_that(ab1sum3[1,"chi.sq"],equals(ab3$McNemar_continuity_correction$Chisq))
    expect_that(ab1sum3[1,"p"],equals(ab3$McNemar_continuity_correction$pvalue))
  }
})

test_that("ageBias compared to http://www.nefsc.noaa.gov/fbp/age-prec/ calculations for AlewifeLH",{
  if (require(FSAdata)) {
    data(AlewifeLH)
    ab1 <- ageBias(otoliths~scales,data=AlewifeLH,ref.lab="Otolith Age",nref.lab="Scale Age")
    expect_that(ab1$bias$n,equals(c(2,18,20,13,18,10,8,7,5,1,2)))
    ## the fbp result is actually 4.62 for age-6
    expect_that(round(ab1$bias$mean,2),equals(c(0.00,1.11,2.20,2.85,3.78,4.20,4.62,5.00,4.80,6.00,6.00)))
  }
})

test_that("ageBias errors and warnings",{
  data(WhitefishLC)
  ## Two variables on LHS
  expect_that(ageBias(otolithC+scaleC~finrayC,data=WhitefishLC),throws_error())
  ## Two variables on RHS
  expect_that(ageBias(otolithC~scaleC+finrayC,data=WhitefishLC),throws_error())
})