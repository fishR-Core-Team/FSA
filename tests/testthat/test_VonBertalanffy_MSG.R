context("Von Bertalanffy MESSAGES")
# also see test_growthFuns

test_that("vbStarts() messages",{
  ## Get some data for the following attempts
  if (require(fishmethods)) {
    data(Kimura,package="fishmethods")
    ## Asked for a dynamicPlot, which now does not exist
    expect_warning(vbStarts(length~age,data=Kimura,dynamicPlot=TRUE),
                   "functionality has been moved to")
    ## wrong types
    expect_error(vbStarts(length~age,data=Kimura,param="Derek"),"should be one of")
    expect_error(vbStarts(length~age,data=Kimura,type="Derek"),"should be one of")
    expect_error(vbStarts(length~age,data=Kimura,param="Francis",methEV="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,param="Schnute",methEV="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,param="typical",meth0="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,param="original",meth0="Derek"),
                 "should be one of")
    expect_error(vbStarts(length~age,data=Kimura,methLinf="Derek"),"should be one of")
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
    expect_error(vbStarts(length~age,data=Kimura,param="Francis",ages2use=c(2,5,10)),
                 "have only two ages")
    expect_error(vbStarts(length~age,data=Kimura,param="Schnute",ages2use=2),
                 "have only two ages")
    expect_error(vbStarts(length~age,data=Kimura,param="Schnute",ages2use=c(2,5,10)),
                 "have only two ages")
    ## ages2use in wrong order
    expect_warning(vbStarts(length~age,data=Kimura,param="Francis",ages2use=c(10,2)),
                   "order reversed to continue")
    expect_warning(vbStarts(length~age,data=Kimura,param="Schnute",ages2use=c(10,2)),
                   "order reversed to continue")
    ## problems with fixed argument
    expect_error(vbStarts(length~age,data=Kimura,fixed=c(Linf=3)),"must be a list")
    expect_error(vbStarts(length~age,data=Kimura,fixed=list(Linf=3,7)),"must be named")
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
