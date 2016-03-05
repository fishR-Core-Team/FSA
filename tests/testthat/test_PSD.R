context("PSD functions")

# ############################################################
# Messaging
# ############################################################

test_that("psdVal() errors and warnings",{
  ## bad species name
  expect_error(psdVal("Derek"),"The Gabelhouse lengths do not exist")
  ## bad units
  expect_error(psdVal("Bluegill",units="inches"),"should be one of")
  ## too many species name
  expect_error(psdVal(c("Bluegill","Yellow Perch")),"can have only one name")
  ## addLens and addNames don't match up
  expect_error(psdVal("Bluegill",addLens=7,addNames=c("Derek","Ogle")),"have different lengths")
  expect_error(psdVal("Bluegill",addLens=c(7,9),addNames="Derek"),"have different lengths")
  ## and addLens is also a Gabelhouse length
  expect_warning(psdVal("Bluegill",addLens=150),"At least one Gabelhouse length that was in")
})

test_that("psdVal() returns",{
  ## check values for yellow perch
  tmp <- psdVal("Yellow Perch",incl.zero=FALSE)
  expect_equivalent(tmp,c(130,200,250,300,380))
  expect_equal(names(tmp),c("stock","quality","preferred","memorable","trophy" ))
  tmp <- psdVal("Yellow Perch",incl.zero=FALSE,units="in")
  expect_equivalent(tmp,c(5,8,10,12,15))
  expect_equal(names(tmp),c("stock","quality","preferred","memorable","trophy" ))
  tmp <- psdVal("Yellow Perch",units="in")
  expect_equivalent(tmp,c(0,5,8,10,12,15))
  expect_equal(names(tmp),c("substock","stock","quality","preferred","memorable","trophy" ))
  tmp <- psdVal("Yellow Perch",units="in",addLens=c(7,9))
  expect_equivalent(tmp,c(0,5,7,8,9,10,12,15))
  expect_equal(names(tmp),c("substock","stock","7","quality","9","preferred","memorable","trophy" ))
  tmp <- psdVal("Yellow Perch",units="in",addLens=c(7,9),addNames=c("minSlot","maxSlot"))
  expect_equivalent(tmp,c(0,5,7,8,9,10,12,15))
  expect_equal(names(tmp),c("substock","stock","minSlot","quality","maxSlot","preferred","memorable","trophy"))
  tmp <- psdVal("Yellow Perch",units="in",addLens=c(minSlot=7,maxSlot=9),addNames=c("minSlot","maxSlot"))
  expect_equivalent(tmp,c(0,5,7,8,9,10,12,15))
  expect_equal(names(tmp),c("substock","stock","minSlot","quality","maxSlot","preferred","memorable","trophy"))
  tmp <- psdVal("yellow perch")
  expect_equivalent(tmp,c(0,130,200,250,300,380))
  expect_equal(names(tmp),c("substock","stock","quality","preferred","memorable","trophy" ))
})


test_that("psdCI() errors and warnings",{
  ## problems with proportions table
  # not obviously a proportions table
#  expect_warning(psdCI(c(1,0,0,0),c(0.5,0.3,0.2,10),11))
#  expect_warning(psdCI(c(1,0,0,0),c(5,3,2,10),20))
  # looks like proportions, but doesn't sum to 1
  expect_error(psdCI(c(1,0,0,0),c(0.5,0.3,0.1,0.08),20))
  expect_error(psdCI(c(1,0,0,0),c(0.5,0.3,0.2,0.08),20))
  # small n for table using multinomial distribution
  expect_warning(psdCI(c(0,0,0,1),c(0.5,0.3,0.1,0.1),20,method="multinomial"))
  
  ## problems with indicator vector
  ipsd <- c(0.130,0.491,0.253,0.123)
  n <- 445
  # all zeroes
  expect_error(psdCI(c(0,0,0,0),ipsd,n),"cannot be all zeroes")
  # all ones
  expect_error(psdCI(c(1,1,1,1),ipsd,n),"cannot be all ones")
  # wrong length of indvec
  expect_error(psdCI(c(1,0,0),ipsd,n),"must be the same")
  expect_error(psdCI(c(1,0,0,0,0),ipsd,n),"must be the same")
})

test_that("psdCalc() errors and warnings",{
  ## simulate data set
  df <- data.frame(tl=round(c(rnorm(100,mean=125,sd=15),
                              rnorm(50,mean=200,sd=25),
                              rnorm(20,mean=300,sd=40)),0),
                      species=rep("Yellow Perch",170))
  ## get Gabelhouse lengths for Yellow Perch
  ghl <- psdVal("Yellow perch")
  ## restrict data.frame to sub-stock-length fish
  tmp <- subset(df,tl<ghl["stock"])
  expect_error(psdCalc(~tl,data=tmp,species="Yellow perch"),"no stock-length fish in the sample")
  ## restrict data.frame to no fish
  tmp <- subset(df,tl<ghl["substock"])
  expect_error(psdCalc(~tl,data=tmp,species="Yellow perch"),"does not contain any rows")
  
  ## no species name given
  expect_error(psdCalc(~tl,data=tmp),"Must include a species name in")
  
  ## bad formulae
  expect_error(psdCalc(tl,data=df,species="Yellow perch"),"not found")
  expect_error(psdCalc(tl~species,data=df,species="Yellow perch"),"Function only works with formulas with 1 variable")
  expect_error(psdCalc(~tl+species,data=df,species="Yellow perch"),"Function only works with formulas with 1 variable")
  expect_error(psdCalc(~species,data=df,species="Yellow perch"),"must be numeric")
})

test_that("psdPlot() errors and warnings",{
  ## simulate data set
  df <- data.frame(tl=round(c(rnorm(100,mean=125,sd=15),
                              rnorm(50,mean=200,sd=25),
                              rnorm(20,mean=300,sd=40)),0),
                   species=rep("Yellow Perch",170))
  ## get Gabelhouse lengths for Yellow Perch
  ghl <- psdVal("Yellow perch")

  ## set minimum length higher than stock length
  expect_error(psdPlot(~tl,data=df,species="Yellow perch",xlim=c(ghl["stock"]+10,300)),"Minimum chosen length value in")
  
  ## restrict data.frame to no fish
  tmp <- subset(df,tl<ghl["substock"])
  expect_error(psdPlot(~tl,data=tmp,species="Yellow perch"),"does not contain any rows")
  
  ## bad formulae
  expect_error(psdPlot(tl,data=df,species="Yellow perch"),"not found")
  expect_error(psdPlot(tl~species,data=df,species="Yellow perch"),"Function only works with formulas with 1 variable.")
  expect_error(psdPlot(~tl+species,data=df,species="Yellow perch"),"Function only works with formulas with 1 variable.")
  expect_error(psdPlot(~species,data=df,species="Yellow perch"),"must be numeric")
})

test_that("psdAdd() errors and warnings",{
  ## simulate data set
  set.seed(345234534)
  dbt <- data.frame(species=factor(rep(c("Bluefin Tuna"),30)),
                    tl=round(rnorm(30,1900,300),0))
  dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
  dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),
                    tl=round(rnorm(30,130,50),0))
  dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
  dlb <- data.frame(species=factor(rep(c("Largemouth Bass"),30)),
                    tl=round(rnorm(30,350,60),0))
  dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
  df <- rbind(dbt,dbg,dlb)
  
  ## bad units
  expect_error(psdAdd(tl~species,df,units="inches"),"should be one of")
  expect_error(psdAdd(df$tl,df$species,units="inches"),"should be one of")
  
  ## bad formulae
  expect_error(psdAdd(~tl,df),"one variable")
  expect_error(psdAdd(~species,df),"one variable")
  expect_error(psdAdd(tl~wt+species,df),"one variable")
  
  ## bad variable types
  expect_error(psdAdd(species~tl,df),"not numeric")
  expect_error(psdAdd(tl~wt,df),"only one factor")
  expect_error(psdAdd(df$species,df$tl),"numeric")
  expect_error(psdAdd(df$tl,df$wt),"factor")
  
  ## bad addSpec/addLens combination
  expect_warning(psdAdd(tl~species,df,addSpec="Derek",verbose=FALSE),"is not NULL")
  
  ## One species had all missing lengths
  df[df$species=="Bluegill","tl"] <- NA
  df <- df[df$species!="Bluefin Tuna",]
  expect_message(psdAdd(tl~species,df),"were missing for")
})


# ############################################################
# Analytical Results
# ############################################################
context("Verification of psdXXX results")

test_that("Does psdCI results match Brenden et al. (2008) results",{
  ## proportions table from Brenden et al. (2008)
  ipsd <- c(0.130,0.491,0.253,0.123)
  n <- 445
  
  ## results from Brendent et al. (2008) ... Table 2
  bpsd <- cbind(c(13,49,25,12,87,38),
                c(9,42,20,8,82,31),
                c(17,56,31,17,91,44))
  colnames(bpsd) <- c("Estimate","95% LCI","95% UCI")
  rownames(bpsd) <- c("PSD S-Q","PSD Q-P","PSD P-M","PSD M-T","PSD","PSD-P")
  
  ## psdCI calculations
  imat <- matrix(c(1,0,0,0,
                   0,1,0,0,
                   0,0,1,0,
                   0,0,0,1,
                   0,1,1,1,
                   0,0,1,1),nrow=6,byrow=TRUE)
  rownames(imat) <- rownames(bpsd)
  mcis <- t(apply(imat,MARGIN=1,FUN=psdCI,ptbl=ipsd,n=n,method="multinomial"))
  colnames(mcis) <- c("Estimate","95% LCI","95% UCI")
  diff <- mcis-bpsd
  # Brenden's results were rounded, thus all should be within 0.5
  expect_true(all(abs(diff)<=0.5))
})

test_that("Does psdCI results match Brenden et al. (2008) results",{
  ## Setup sample with known PSD values
  brks <- psdVal("Yellow Perch")
  freq <- c(110,75,50,35,20,10)
  df <- data.frame(tl=rep(brks,freq)+sample(1:45,300,replace=TRUE),
                   species=rep("Yellow Perch",300))
  ## Get known PSD values
  psdXYs <- prop.table(freq[-1])*100
  psdXs <- rcumsum(psdXYs)[-1]
  psdXYs <- psdXYs[-length(psdXYs)]
  ## Get psdCalc results
  suppressWarnings(resXY <- psdCalc(~tl,data=df,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=df,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  ## Are lengths what you would expect
  expect_equal(nrow(resXY),4)
  expect_equal(nrow(resX),4)
  ## Are values the same
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  diffs <- round(resX[,"Estimate"]-psdXs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  
  ## Do things still work if all sub-stock fish are removed
  tmp <- Subset(df,tl>=brks["stock"])
  suppressWarnings(resXY <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  expect_equal(nrow(resXY),4)
  expect_equal(nrow(resX),4)
  ## Are values the same
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  diffs <- round(resX[,"Estimate"]-psdXs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))

  ## Do things still work if all sub-stock and stock fish are removed  
  psdXYs <- prop.table(freq[-c(1:2)])*100
  psdXs <- rcumsum(psdXYs)[-1]
  psdXYs <- psdXYs[-length(psdXYs)]
  
  tmp <- Subset(df,tl>=brks["quality"])
  suppressWarnings(resXY <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  expect_equal(nrow(resXY),3)  # no S-Q row
  expect_equal(nrow(resX),4)   # all should be there
  ## Are values the same
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  diffs <- round(resX[-1,"Estimate"]-psdXs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))

  ## Do things still work if all trophy fish are removed  
  psdXYs <- prop.table(freq[-c(1,length(freq))])*100
  psdXs <- rcumsum(psdXYs)[-1]
  
  tmp <- Subset(df,tl<brks["trophy"])
  suppressWarnings(resXY <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  expect_equal(nrow(resXY),4)  # no T- row
  expect_equal(nrow(resX),3)   # no T row
  ## Are values the same
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  diffs <- round(resX[,"Estimate"]-psdXs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))
})

test_that("Does manual calculation after psdAdd() equal psdCalc() results?",{
  ## simulate data set
  set.seed(345234534)
  dbt <- data.frame(species=factor(rep(c("Bluefin Tuna"),30)),tl=round(rnorm(30,1900,300),0))
  dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
  dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
  dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
  dlb <- data.frame(species=factor(rep(c("Largemouth Bass"),30)),tl=round(rnorm(30,350,60),0))
  dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
  df <- rbind(dbt,dbg,dlb)
  
  ## get psdCalc results for LMB and BG .. ultimately compare to psdDataPrep results
  suppressWarnings(psdBG <- psdCalc(~tl,data=Subset(df,species=="Bluegill"),species="Bluegill",digits=getOption("digits")))
  suppressWarnings(psdLMB <- psdCalc(~tl,data=Subset(df,species=="Largemouth Bass"),species="Largemouth Bass",digits=getOption("digits")))
  
  ## apply psdAdd
  suppressMessages(df$PSDcat <- psdAdd(tl~species,df))
  # remove substock and other fish
  df <- Subset(df,species %in% c("Bluegill","Largemouth Bass"))
  df <- Subset(df,PSDcat!="substock")
  res <- prop.table(xtabs(~species+PSDcat,data=df),margin=1)*100
  ## do PSD X-Y results match for two species
  ## Are values the same
  diffs <- round(res["Bluegill",1:3]-psdBG[3:5,"Estimate"],7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  diffs <- round(res["Largemouth Bass",1:3]-psdLMB[3:5,"Estimate"],7)
  expect_equivalent(diffs,rep(0,length(diffs)))
})


# ------------------------------------------------------------
# read in external CSV file
# ------------------------------------------------------------
ftmp <- system.file("extdata", "PSDWR_testdata.csv", package="FSA")
df <- read.csv(ftmp)
dfbg <- Subset(df,species=="Bluegill")
dflmb <- Subset(df,species=="Largemouth Bass")

test_that("Does psdAdd() create correct Gabelhouse categories?",{
  suppressMessages(df$gcatn <- psdAdd(tl~species,data=df))
  expect_equivalent(df$gcatn,df$GCATN)
})

test_that("Does psdCalc() compute correct PSD values?",{
  suppressWarnings(bgres <- psdCalc(~tl,data=dfbg,species="Bluegill"))
  expect_equivalent(bgres[,"Estimate"],c(80,60,40,20,20,20,20,20))
  suppressWarnings(lmbres <- psdCalc(~tl,data=dflmb,species="Largemouth Bass"))
  expect_equivalent(lmbres[,"Estimate"],c(60,30,10,40,30,20,10))
})
