context("PSD functions VALIDATE")
source("EXS_PSD.R")

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
  df3 <- data.frame(tl=rep(brks,freq)+sample(1:45,300,replace=TRUE),
                    species=rep("Yellow Perch",300))
  ## Get known PSD values
  psdXYs <- prop.table(freq[-1])*100
  psdXs <- rcumsum(psdXYs)[-1]
  psdXYs <- psdXYs[-length(psdXYs)]
  ## Get psdCalc results
  suppressWarnings(resXY <- psdCalc(~tl,data=df3,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=df3,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  ## Are lengths what you would expect
  expect_equal(nrow(resXY),4)
  expect_equal(nrow(resX),4)
  ## Are values the same
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  diffs <- round(resX[,"Estimate"]-psdXs,7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  
  ## Do things still work if all sub-stock fish are removed
  tmp <- Subset(df3,tl>=brks["stock"])
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
  
  tmp <- Subset(df3,tl>=brks["quality"])
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
  
  tmp <- Subset(df3,tl<brks["trophy"])
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
  ## get psdCalc results for LMB and BG .. ultimately compare to psdDataPrep results
  suppressWarnings(psdBG <- psdCalc(~tl,data=Subset(df,species=="Bluegill"),species="Bluegill",digits=getOption("digits")))
  suppressWarnings(psdLMB <- psdCalc(~tl,data=Subset(df,species=="Largemouth Bass"),species="Largemouth Bass",digits=getOption("digits")))
  
  ## apply psdAdd
  suppressMessages(df$PSDcat <- psdAdd(tl~species,df))
  # remove substock and other fish
  tmp <- Subset(df,species %in% c("Bluegill","Largemouth Bass"))
  tmp <- Subset(tmp,PSDcat!="substock")
  res <- prop.table(xtabs(~species+PSDcat,data=tmp),margin=1)*100
  ## do PSD X-Y results match for two species
  ## Are values the same
  diffs <- round(res["Bluegill",1:3]-psdBG[3:5,"Estimate"],7)
  expect_equivalent(diffs,rep(0,length(diffs)))
  diffs <- round(res["Largemouth Bass",1:3]-psdLMB[3:5,"Estimate"],7)
  expect_equivalent(diffs,rep(0,length(diffs)))
})

test_that("Does psdCalc() compute correct PSD values?",{
  suppressWarnings(bgres <- psdCalc(~tl,data=df2bg,species="Bluegill"))
  expect_equivalent(bgres[,"Estimate"],c(80,60,40,20,20,20,20,20))
  suppressWarnings(lmbres <- psdCalc(~tl,data=df2lmb,species="Largemouth Bass"))
  expect_equivalent(lmbres[,"Estimate"],c(60,30,10,40,30,20,10))
})
