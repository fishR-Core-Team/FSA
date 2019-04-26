# also see test_growthFuns

## Test Messages ----
test_that("vbStarts() messages",{
  ## Get some data for the following attempts
  if (require(fishmethods)) {
    data(Kimura,package="fishmethods")
    ## Asked for a dynamicPlot, which now does not exist
    expect_warning(vbStarts(length~age,data=Kimura,dynamicPlot=TRUE),
                   "functionality has been moved to")
    ## wrong types
    expect_error(vbStarts(length~age,data=Kimura,param="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,type="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,param="Francis",methEV="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,param="Schnute",methEV="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,param="typical",meth0="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,param="original",meth0="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,methLinf="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,methLinf="oldAge",num4Linf=-1),
                 "must be at least 1")
    expect_error(vbStarts(length~age,data=Kimura,methLinf="longFish",num4Linf=-1),
                 "must be at least 1")
    expect_error(vbStarts(length~age,data=Kimura,methLinf="oldAge",num4Linf=30),
                 "less than the number of observed ages")
    expect_error(vbStarts(length~age,data=Kimura,methLinf="longFish",num4Linf=500),
                 "less than the number of recorded lengths")
    ## Two variables on LHS
    expect_error(vbStarts(length+age~age,data=Kimura,param="typical"),
                 "more than one variable on the LHS")
    ## Two variables on RHS
    expect_error(vbStarts(length~age+sex,data=Kimura,param="typical"),
                 "must have only one RHS variable")
    ## LHS is a factor
    expect_error(vbStarts(sex~age,data=Kimura,param="typical"),
                 "LHS variable must be numeric")
    ## RHS is a factor
    expect_error(vbStarts(length~sex,data=Kimura,param="typical"),
                 "RHS variable must be numeric")
    ## not two ages2use given
    expect_error(vbStarts(length~age,data=Kimura,param="Francis",ages2use=2),
                 "have only two ages")
    expect_error(vbStarts(length~age,data=Kimura,param="Francis",
                          ages2use=c(2,5,10)),
                 "have only two ages")
    expect_error(vbStarts(length~age,data=Kimura,param="Schnute",ages2use=2),
                 "have only two ages")
    expect_error(vbStarts(length~age,data=Kimura,param="Schnute",
                          ages2use=c(2,5,10)),
                 "have only two ages")
    ## ages2use in wrong order
    expect_warning(vbStarts(length~age,data=Kimura,param="Francis",
                            ages2use=c(10,2)),
                   "order reversed to continue")
    expect_warning(vbStarts(length~age,data=Kimura,param="Schnute",
                            ages2use=c(10,2)),
                   "order reversed to continue")
    ## problems with fixed argument
    expect_error(vbStarts(length~age,data=Kimura,fixed=c(Linf=3)),
                 "must be a list")
    expect_error(vbStarts(length~age,data=Kimura,fixed=list(Linf=3,7)),
                 "must be named")
    ## problems with valOgle argument
    expect_error(vbStarts(length~age,data=Kimura,param="Ogle"),
                 "must contain a value for 'Lr' or 'tr'")
    expect_error(vbStarts(length~age,data=Kimura,param="Ogle",valOgle=3),
                 "must be a named vector")
    expect_error(vbStarts(length~age,data=Kimura,param="Ogle",valOgle="3"),
                 "must be numeric")
    expect_error(vbStarts(length~age,data=Kimura,param="Ogle",valOgle=c(3,4)),
                 "must contain only one value")
    expect_error(vbStarts(length~age,data=Kimura,param="Ogle",valOgle=c(a=3)),
                 "must be 'Lr' or 'tr'")
    expect_warning(vbStarts(length~age,data=Kimura,param="Ogle",valOgle=c(tr=0)),
                   "less than minimum observed age")
    expect_warning(vbStarts(length~age,data=Kimura,param="Ogle",valOgle=c(Lr=0)),
                   "less than minimum observed length")
    ## too few ages to estimate Linf
    expect_error(vbStarts(length~age,data=subset(Kimura,age<3)),
                 "cannot be automatically determined")
  }
  if (require(FSAdata)) {
    data(SpottedSucker1,package="FSAdata")
    ## gives warning about a poor estimate for K and Linf
    sv <- list(Linf=max(SpottedSucker1$tl),K=0.3,t0=0)
    expect_warning(vbStarts(tl~age,data=SpottedSucker1,param="typical"))
    ## too few ages to estimate Linf
    expect_error(vbStarts(tl~age,data=subset(SpottedSucker1,age<5)),
                 "cannot be automatically determined")
  }
})


## Test Output Types ----
test_that("vbStarts() output",{
  ## Get some data for the following attempts
  if (require(fishmethods)) {
    data(Kimura,package="fishmethods")
    ## Returns a list with proper names
    tmp <- vbStarts(length~age,data=Kimura,param="typical")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="typical",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="typical",
                    fixed=list(Linf=30))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0"))
    expect_equal(tmp[["Linf"]],30)
    tmp <- vbStarts(length~age,data=Kimura,param="typical",
                    fixed=list(Linf=30,K=0.3))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0"))
    expect_equal(tmp[["Linf"]],30)
    expect_equal(tmp[["K"]],0.3)
    tmp <- vbStarts(length~age,data=Kimura,param="typical",
                    fixed=list(Linf=30,K=0.3,t0=0))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0"))
    expect_equal(tmp[["Linf"]],30)
    expect_equal(tmp[["K"]],0.3)
    expect_equal(tmp[["t0"]],0)
    tmp <- vbStarts(length~age,data=Kimura,param="BevertonHolt")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="BevertonHolt",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="original")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","L0"))
    tmp <- vbStarts(length~age,data=Kimura,param="original",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","L0"))
    tmp <- vbStarts(length~age,data=Kimura,param="original",
                    fixed=list(Linf=30,K=0.3,L0=2))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","L0"))
    expect_equal(tmp[["Linf"]],30)
    expect_equal(tmp[["K"]],0.3)
    expect_equal(tmp[["L0"]],2)
    tmp <- vbStarts(length~age,data=Kimura,param="vonBertalanffy")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","L0"))
    tmp <- vbStarts(length~age,data=Kimura,param="vonBertalanffy",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","L0"))
    tmp <- vbStarts(length~age,data=Kimura,param="GQ")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("omega","K","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="GQ",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("omega","K","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="GQ",
                    fixed=list(omega=20,K=0.3,t0=0))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("omega","K","t0"))
    expect_equal(tmp[["omega"]],20)
    expect_equal(tmp[["K"]],0.3)
    expect_equal(tmp[["t0"]],0)
    tmp <- vbStarts(length~age,data=Kimura,param="GallucciQuinn")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("omega","K","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="GallucciQuinn",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("omega","K","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="Mooij")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","L0","omega"))
    tmp <- vbStarts(length~age,data=Kimura,param="Mooij",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","L0","omega"))
    tmp <- vbStarts(length~age,data=Kimura,param="Mooij",
                    fixed=list(Linf=30,L0=2,omega=20))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","L0","omega"))
    expect_equal(tmp[["Linf"]],30)
    expect_equal(tmp[["L0"]],2)
    expect_equal(tmp[["omega"]],20)
    tmp <- vbStarts(length~age,data=Kimura,param="Weisberg")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","t50","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="Weisberg",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","t50","t0"))
    tmp <- vbStarts(length~age,data=Kimura,param="Weisberg",
                    fixed=list(Linf=30,t50=2,t0=0))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","t50","t0"))
    expect_equal(tmp[["Linf"]],30)
    expect_equal(tmp[["t50"]],2)
    expect_equal(tmp[["t0"]],0)
    tmp <- vbStarts(length~age,data=Kimura,param="Schnute",ages2use=c(1,10))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("L1","L3","K"))
    tmp <- vbStarts(length~age,data=Kimura,param="Schnute",ages2use=c(1,10),
                    methEV="means")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("L1","L3","K"))
    tmp <- vbStarts(length~age,data=Kimura,param="Schnute",ages2use=c(1,10),
                    fixed=list(L1=15,L3=60,K=0.3))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("L1","L3","K"))
    expect_equal(tmp[["L1"]],15)
    expect_equal(tmp[["L3"]],60)
    expect_equal(tmp[["K"]],0.3)
    tmp <- vbStarts(length~age,data=Kimura,param="Francis",ages2use=c(1,10))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("L1","L2","L3"))
    tmp <- vbStarts(length~age,data=Kimura,param="Francis",ages2use=c(1,10),
                    methEV="means")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("L1","L2","L3"))
    tmp <- vbStarts(length~age,data=Kimura,param="Francis",ages2use=c(1,10),
                    fixed=list(L1=15,L2=40,L3=60))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("L1","L2","L3"))
    expect_equal(tmp[["L1"]],15)
    expect_equal(tmp[["L2"]],40)
    expect_equal(tmp[["L3"]],60)
    tmp <- vbStarts(length~age,data=Kimura,param="Somers")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0","C","ts"))
    tmp <- vbStarts(length~age,data=Kimura,param="Somers",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0","C","ts"))
    tmp <- vbStarts(length~age,data=Kimura,param="Somers",
                    fixed=list(Linf=30,K=0.3,t0=0,C=0.3,ts=0.5))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0","C","ts"))
    expect_equal(tmp[["Linf"]],30)
    expect_equal(tmp[["K"]],0.3)
    expect_equal(tmp[["t0"]],0)
    expect_equal(tmp[["C"]],0.3)
    expect_equal(tmp[["ts"]],0.5)
    tmp <- vbStarts(length~age,data=Kimura,param="Somers2")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0","C","WP"))
    tmp <- vbStarts(length~age,data=Kimura,param="Somers2",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0","C","WP"))
    tmp <- vbStarts(length~age,data=Kimura,param="Somers2",
                    fixed=list(Linf=30,K=0.3,t0=0,C=0.3,WP=0.5))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","K","t0","C","WP"))
    expect_equal(tmp[["Linf"]],30)
    expect_equal(tmp[["K"]],0.3)
    expect_equal(tmp[["t0"]],0)
    expect_equal(tmp[["C"]],0.3)
    expect_equal(tmp[["WP"]],0.5)
    tmp <- vbStarts(length~age,data=Kimura,param="Pauly")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","Kpr","t0","ts","NGT"))
    tmp <- vbStarts(length~age,data=Kimura,param="Pauly",meth0="yngAge")
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","Kpr","t0","ts","NGT"))
    tmp <- vbStarts(length~age,data=Kimura,param="Pauly",
                    fixed=list(Linf=30,Kpr=0.3,t0=0,ts=0.5,NGT=0.2))
    expect_is(tmp,"list")
    expect_equal(names(tmp),c("Linf","Kpr","t0","ts","NGT"))
    expect_equal(tmp[["Linf"]],30)
    expect_equal(tmp[["Kpr"]],0.3)
    expect_equal(tmp[["t0"]],0)
    expect_equal(tmp[["ts"]],0.5)
    expect_equal(tmp[["NGT"]],0.2)
  }  
})


## Validate Results ----
test_that("vbFuns() and vbStarts() fit to Kimura match Haddon book (2nd ed, p237) results (Excel).",{
  if (require(fishmethods)) {
    data(Kimura,package="fishmethods")
    ## Get typical Von B function
    vbT <- vbFuns("typical")
    ## Examine females
    KimuraF <- filterD(Kimura,sex=="F")
    svF <- vbStarts(length~age,data=KimuraF,param="typical")
    fitF <- nls(length~vbT(age,Linf,K,t0),data=KimuraF,start=svF)
    cfF <- coef(fitF)
    # double brackets to remove name attribute
    expect_equal(round(cfF[["Linf"]],2),61.23)
    expect_equal(round(cfF[["K"]],4),0.2963)
    expect_equal(round(cfF[["t0"]],4),-0.0573)
    ## Examine males
    KimuraM <- filterD(Kimura,sex=="M")
    svM <- vbStarts(length~age,data=KimuraM,param="typical")
    fitM <- nls(length~vbT(age,Linf,K,t0),data=KimuraM,start=svM)
    cfM <- coef(fitM)
    expect_equal(round(cfM[["Linf"]],2),55.98)
    expect_equal(round(cfM[["K"]],4),0.3856)
    expect_equal(round(cfM[["t0"]],4),0.1713)
  }
})

test_that("vbFuns() and vbStarts() fit to AIFFD book (Box 5.4) results (SAS).",{
  # This is a weak test because of the messiness of the data.
  if (require(FSAdata)) {
    data(SpottedSucker1,package="FSAdata")
    ## Get typical Von B function
    vbT <- vbFuns("typical")
    sv <- list(Linf=max(SpottedSucker1$tl),K=0.3,t0=0)
    fit <- nls(tl~vbT(age,Linf,K,t0),data=SpottedSucker1,start=sv)
    cf <- coef(fit)
    # double brackets to remove name attribute
    expect_equal(round(cf[["Linf"]],0),516)
    expect_equal(round(cf[["K"]],3),0.190)
    expect_equal(round(cf[["t0"]],2),-4.54)
  }
})


test_that("vbFuns() and vbStarts() fit to Kimura separated by sex match fishmethods (and Kimura) results.",{
  if (require(fishmethods) & require(lmtest)) {
    data(Kimura,package="fishmethods")
    
    ### get fishmethods results (straight from example)
    fm1 <- growthlrt(len=Kimura$length,age=Kimura$age,group=Kimura$sex,
                     error=2,select=1)
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
    expect_equal(lr04$Df[2],fm1$results$df[fm1$results$tests=="Ho vs H4"])
    expect_equal(round(lr04$Chisq[2],2),
                 round(fm1$results$chisq[fm1$results$tests=="Ho vs H4"],2))
    expect_equal(round(lr04$'Pr(>Chisq)'[2],3),
                 round(fm1$results$p[fm1$results$tests=="Ho vs H4"],3))
    ## Is this the same as fishmethods results for Ho vs H3
    vb2LK <- length~Linf[sex]*(1-exp(-K[sex]*(age-t0)))
    sv2LK <- mapply(rep,svCom,c(2,2,1))
    fit2LK <- nls(vb2LK,data=Kimura,start=sv2LK)
    lr03 <- lrtest(fit2LK,fitGen)
    expect_equal(lr03$Df[2],fm1$results$df[fm1$results$tests=="Ho vs H3"])
    expect_equal(round(lr03$Chisq[2],2),
                 round(fm1$results$chisq[fm1$results$tests=="Ho vs H3"],2))
    # only to two decimals in p-value (likely just rounding error)
    expect_equal(round(lr03$'Pr(>Chisq)'[2],2),
                 round(fm1$results$p[fm1$results$tests=="Ho vs H3"],2))
    
    ## Is this the same as fishmethods results for Ho vs H2
    vb2Lt <- length~Linf[sex]*(1-exp(-K*(age-t0[sex])))
    sv2Lt <- mapply(rep,svCom,c(2,1,2))
    fit2Lt <- nls(vb2Lt,data=Kimura,start=sv2Lt)
    lr02 <- lrtest(fit2Lt,fitGen)
    expect_equal(lr02$Df[2],fm1$results$df[fm1$results$tests=="Ho vs H2"])
    expect_equal(round(lr02$Chisq[2],2),
                 round(fm1$results$chisq[fm1$results$tests=="Ho vs H2"],2))
    expect_equal(round(lr02$'Pr(>Chisq)'[2],3),
                 round(fm1$results$p[fm1$results$tests=="Ho vs H2"],3))
    
    ## Is this the same as fishmethods results for Ho vs H1
    vb2Kt <- length~Linf*(1-exp(-K[sex]*(age-t0[sex])))
    sv2Kt <- mapply(rep,svCom,c(1,2,2))
    fit2Kt <- nls(vb2Kt,data=Kimura,start=sv2Kt)
    lr01 <- lrtest(fit2Kt,fitGen)
    expect_equal(lr01$Df[2],fm1$results$df[fm1$results$tests=="Ho vs H1"])
    expect_equal(round(lr01$Chisq[2],2),
                 round(fm1$results$chisq[fm1$results$tests=="Ho vs H1"],2))
    expect_equal(round(lr01$'Pr(>Chisq)'[2],3),
                 round(fm1$results$p[fm1$results$tests=="Ho vs H1"],3))
    
    ## Do parameter estimates match those in Kimura (Table 3)
    # general model
    expect_equivalent(round(coef(fitGen)[1:2],2),c(61.23,55.98))
    expect_equivalent(round(coef(fitGen)[3:6],3),c(0.296,0.386,-0.057,0.171))
    # Linf equivalent model (H3)
    expect_equivalent(round(coef(fit2Kt)[1],2),c(59.40))
    expect_equivalent(round(coef(fit2Kt)[2:5],3),c(0.337,0.297,0.087,-0.111))
    # K equivalent model (H2) Linf slightly off in 2nd decimal for 2nd value
    expect_equivalent(round(coef(fit2Lt)[1:2],1),c(60.1,57.4))
    expect_equivalent(round(coef(fit2Lt)[3:5],3),c(0.330,0.095,-0.021))
    # t0 equivalent model (H1)
    expect_equivalent(round(coef(fit2LK)[1:2],2),c(60.77,56.45))
    expect_equivalent(round(coef(fit2LK)[3:5],3),c(0.313,0.361,0.057))
    # common model (H4)
    expect_equivalent(round(coef(fitCom),2),c(59.29,0.32,0.01))
  }
})

