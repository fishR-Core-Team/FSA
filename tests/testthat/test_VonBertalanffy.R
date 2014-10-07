context("Von Bertalanffy")

# ############################################################
# ============================================================
# Messaging
# ============================================================
# ############################################################

test_that("vbFuns errors and warnings",{
  ## wrong types
  expect_that(vbFuns(type="Derek"),throws_error())
})
  
test_that("vbStarts errors and warnings",{
  ## Get some data for the following attempts
  if (require(fishmethods)) {
    data(Kimura)
    ## wrong types
    expect_that(vbStarts(length~age,data=Kimura,type="Derek"),throws_error())
    expect_that(vbStarts(length~age,data=Kimura,type="Francis",methEV="Derek"),throws_error())
    expect_that(vbStarts(length~age,data=Kimura,type="Schnute",methEV="Derek"),throws_error())
    expect_that(vbStarts(length~age,data=Kimura,type="typical",meth0="Derek"),throws_error())
    expect_that(vbStarts(length~age,data=Kimura,type="original",meth0="Derek"),throws_error())
    ## Two variables on LHS
    expect_that(vbStarts(length+age~age,data=Kimura,type="typical"),throws_error())
    ## Two variables on RHS
    expect_that(vbStarts(length~age+sex,data=Kimura,type="typical"),throws_error())
    ## LHS is a factor
    expect_that(vbStarts(sex~age,data=Kimura,type="typical"),throws_error())
    ## RHS is a factor
    expect_that(vbStarts(length~sex,data=Kimura,type="typical"),throws_error())
    ## not two ages2use given
    expect_that(vbStarts(length~age,data=Kimura,type="Francis",ages2use=2),throws_error())
    expect_that(vbStarts(length~age,data=Kimura,type="Francis",ages2use=c(2,5,10)),throws_error())
    expect_that(vbStarts(length~age,data=Kimura,type="Schnute",ages2use=2),throws_error())
    expect_that(vbStarts(length~age,data=Kimura,type="Schnute",ages2use=c(2,5,10)),throws_error())
    ## ages2use in wrong order
    expect_that(vbStarts(length~age,data=Kimura,type="Francis",ages2use=c(10,2)),gives_warning())
    expect_that(vbStarts(length~age,data=Kimura,type="Schnute",ages2use=c(10,2)),gives_warning())
  }
  ## gives warning about a poor estimate for K and Linf
  if (require(FSAdata)) {
    data(SpottedSucker1)
    sv <- list(Linf=max(SpottedSucker1$tl),K=0.3,t0=0)
    expect_that(vbStarts(tl~age,data=SpottedSucker1,type="typical"),gives_warning())
  }
})

test_that("growthModelSim errors and warnings",{
  ## Get some data for the following attempts
  if (require(fishmethods)) {
    data(Kimura)
    ## Two variables on LHS
    expect_that(growthModelSim(length+age~age,data=Kimura),throws_error())
    ## Two variables on RHS
    expect_that(growthModelSim(length~age+sex,data=Kimura),throws_error())
    ## LHS is a factor
    expect_that(growthModelSim(sex~age,data=Kimura),throws_error())
    ## RHS is a factor
    expect_that(growthModelSim(length~sex,data=Kimura),throws_error())
  }
})

test_that("walfordPlot and chapmanPlot errors and warnings",{
  ## Get some data for the following attempts
  if (require(fishmethods)) {
    data(Kimura)
    ## Two variables on LHS
    expect_that(walfordPlot(length+age~age,data=Kimura),throws_error())
    expect_that(chapmanPlot(length+age~age,data=Kimura),throws_error())
    ## Two variables on RHS
    expect_that(walfordPlot(length~age+sex,data=Kimura),throws_error())
    expect_that(chapmanPlot(length~age+sex,data=Kimura),throws_error())
    ## LHS is a factor
    expect_that(walfordPlot(sex~age,data=Kimura),throws_error())
    expect_that(chapmanPlot(sex~age,data=Kimura),throws_error())
    ## RHS is a factor
    expect_that(walfordPlot(length~sex,data=Kimura),throws_error())
    expect_that(chapmanPlot(length~sex,data=Kimura),throws_error())
  }
})



# ############################################################
# ============================================================
# Analytical Results
# ============================================================
# ############################################################

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


test_that("vbFuns and vbStarts fit to Kimura separated by sex match fishmethods (and Kimura) results.",{
  if (require(fishmethods) & require(lmtest)) {
    data(Kimura)
    
    ### get fishmethods results (straight from example)
    fm1 <- vblrt(len=Kimura$length,age=Kimura$age,group=Kimura$sex,error=2,select=1)
    fm1$results
    
    ### fit with my methods
    ## Is this the same as fishmethods results for Ho vs H4
    # Common model
    vbCom <- length~Linf*(1-exp(-K*(age-t0)))
    svCom <- vbStarts(length~age,data=Kimura)
    fitCom <- nls(vbCom,data=Kimura,start=svCom)
    # General model
    vbGen <- length~Linf[sex]*(1-exp(-K[sex]*(age-t0[sex])))
    svGen <- lapply(svCom,rep,2)
    fitGen <- nls(vbGen,data=Kimura,start=svGen)
    # LRT
    lr04 <- lrtest(fitCom,fitGen)
    expect_that(lr04$Df[2],equals(fm1$results$df[fm1$results$tests=="Ho vs H4"]))
    expect_that(round(lr04$Chisq[2],2),equals(round(fm1$results$chisq[fm1$results$tests=="Ho vs H4"],2)))
    expect_that(round(lr04$'Pr(>Chisq)'[2],3),equals(round(fm1$results$p[fm1$results$tests=="Ho vs H4"],3)))
    ## Is this the same as fishmethods results for Ho vs H3
    vb2LK <- length~Linf[sex]*(1-exp(-K[sex]*(age-t0)))
    sv2LK <- mapply(rep,svCom,c(2,2,1))
    fit2LK <- nls(vb2LK,data=Kimura,start=sv2LK)
    lr03 <- lrtest(fit2LK,fitGen)
    expect_that(lr03$Df[2],equals(fm1$results$df[fm1$results$tests=="Ho vs H3"]))
    expect_that(round(lr03$Chisq[2],2),equals(round(fm1$results$chisq[fm1$results$tests=="Ho vs H3"],2)))
    # only to two decimals in p-value (likely just rounding error)
    expect_that(round(lr03$'Pr(>Chisq)'[2],2),equals(round(fm1$results$p[fm1$results$tests=="Ho vs H3"],2)))
    
    ## Is this the same as fishmethods results for Ho vs H2
    vb2Lt <- length~Linf[sex]*(1-exp(-K*(age-t0[sex])))
    sv2Lt <- mapply(rep,svCom,c(2,1,2))
    fit2Lt <- nls(vb2Lt,data=Kimura,start=sv2Lt)
    lr02 <- lrtest(fit2Lt,fitGen)
    expect_that(lr02$Df[2],equals(fm1$results$df[fm1$results$tests=="Ho vs H2"]))
    expect_that(round(lr02$Chisq[2],2),equals(round(fm1$results$chisq[fm1$results$tests=="Ho vs H2"],2)))
    expect_that(round(lr02$'Pr(>Chisq)'[2],3),equals(round(fm1$results$p[fm1$results$tests=="Ho vs H2"],3)))
    
    ## Is this the same as fishmethods results for Ho vs H1
    vb2Kt <- length~Linf*(1-exp(-K[sex]*(age-t0[sex])))
    sv2Kt <- mapply(rep,svCom,c(1,2,2))
    fit2Kt <- nls(vb2Kt,data=Kimura,start=sv2Kt)
    lr01 <- lrtest(fit2Kt,fitGen)
    expect_that(lr01$Df[2],equals(fm1$results$df[fm1$results$tests=="Ho vs H1"]))
    expect_that(round(lr01$Chisq[2],2),equals(round(fm1$results$chisq[fm1$results$tests=="Ho vs H1"],2)))
    expect_that(round(lr01$'Pr(>Chisq)'[2],3),equals(round(fm1$results$p[fm1$results$tests=="Ho vs H1"],3)))
    
    ## Do parameter estimates match those in Kimura (Table 3)
    # general model
    expect_that(round(coef(fitGen)[1:2],2),is_equivalent_to(c(61.23,55.98)))
    expect_that(round(coef(fitGen)[3:6],3),is_equivalent_to(c(0.296,0.386,-0.057,0.171)))
    # Linf equivalent model (H3)
    expect_that(round(coef(fit2Kt)[1],2),is_equivalent_to(c(59.40)))
    expect_that(round(coef(fit2Kt)[2:5],3),is_equivalent_to(c(0.337,0.297,0.087,-0.111)))
    # K equivalent model (H2) Linf slightly off in 2nd decimal for 2nd value
    expect_that(round(coef(fit2Lt)[1:2],1),is_equivalent_to(c(60.1,57.4)))
    expect_that(round(coef(fit2Lt)[3:5],3),is_equivalent_to(c(0.330,0.095,-0.021)))  
    # t0 equivalent model (H1)
    expect_that(round(coef(fit2LK)[1:2],2),is_equivalent_to(c(60.77,56.45)))
    expect_that(round(coef(fit2LK)[3:5],3),is_equivalent_to(c(0.313,0.361,0.057)))
    # common model (H4)
    expect_that(round(coef(fitCom),2),is_equivalent_to(c(59.29,0.32,0.01)))
  }
})
