context("Von Bertalanffy Results")

test_that("vbFuns and vbStarts fit to Kimura match Haddon book (2nd ed, p237) results (Excel).",{
  if (require(fishmethods)) {
    data(Kimura)
    ## Get typical Von B function
    vbT <- vbFuns("typical")
    ## Examine females
    KimuraF <- Subset(Kimura,sex=="F")
    svF <- vbStarts(length~age,data=KimuraF,type="typical")
    fitF <- nls(length~vbT(age,Linf,K,t0),data=KimuraF,start=svF)
    cfF <- coef(fitF)
    # double brackets to remove name attribute
    expect_that(round(cfF[["Linf"]],2),equals(61.23))
    expect_that(round(cfF[["K"]],4),equals(0.2963))
    expect_that(round(cfF[["t0"]],4),equals(-0.0573))
    ## Examine males
    KimuraM <- Subset(Kimura,sex=="M")
    svM <- vbStarts(length~age,data=KimuraM,type="typical")
    fitM <- nls(length~vbT(age,Linf,K,t0),data=KimuraM,start=svM)
    cfM <- coef(fitM)
    expect_that(round(cfM[["Linf"]],2),equals(55.98))
    expect_that(round(cfM[["K"]],4),equals(0.3856))
    expect_that(round(cfM[["t0"]],4),equals(0.1713))
  }
})

test_that("vbFuns and vbStarts fit to AIFFD book (Box 5.4) results (SAS).",{
  # This is a weak test because of the messiness of the data.
  if (require(FSAdata)) {
    data(SpottedSucker1)
    ## Get typical Von B function
    vbT <- vbFuns("typical")
    sv <- list(Linf=max(SpottedSucker1$tl),K=0.3,t0=0)
    fit <- nls(tl~vbT(age,Linf,K,t0),data=SpottedSucker1,start=sv)
    cf <- coef(fit)
    # double brackets to remove name attribute
    expect_that(round(cf[["Linf"]],0),equals(516))
    expect_that(round(cf[["K"]],3),equals(0.190))
    expect_that(round(cf[["t0"]],2),equals(-4.54))
  }
})
