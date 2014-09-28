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
  expect_that(all(abs(diff)<=0.5),is_true())
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
  ## Get psdCalc results
  suppressWarnings(resXY <- psdCalc(~tl,data=df,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=df,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  ## Are lengths what you would expect
  expect_that(nrow(resXY),equals(5))
  expect_that(nrow(resX),equals(4))
  ## Are values the same
  expect_that(all(round(resXY[,"Estimate"]-psdXYs,7)==0),is_true())
  expect_that(all(round(resX[,"Estimate"]-psdXs,7)==0),is_true())
  
  ## Do things still work if all sub-stock fish are removed
  tmp <- Subset(df,tl>=brks["stock"])
  suppressWarnings(resXY <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  expect_that(nrow(resXY),equals(5))
  expect_that(nrow(resX),equals(4))
  expect_that(all(round(resXY[,"Estimate"]-psdXYs,7)==0),is_true())
  expect_that(all(round(resX[,"Estimate"]-psdXs,7)==0),is_true())
  
  ## Do things still work if all sub-stock and stock fish are removed  
  psdXYs <- prop.table(freq[-c(1:2)])*100
  psdXs <- rcumsum(psdXYs)[-1]
  
  tmp <- Subset(df,tl>=brks["quality"])
  suppressWarnings(resXY <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  expect_that(nrow(resXY),equals(4))  # no S-Q row
  expect_that(nrow(resX),equals(4))   # all should be there
  expect_that(all(round(resXY[,"Estimate"]-psdXYs,7)==0),is_true())
  expect_that(all(round(resX[-1,"Estimate"]-psdXs,7)==0),is_true()) # psdXs does not have PSD-Q

  ## Do things still work if all trophy fish are removed  
  psdXYs <- prop.table(freq[-c(1,length(freq))])*100
  psdXs <- rcumsum(psdXYs)[-1]
  
  tmp <- Subset(df,tl<brks["trophy"])
  suppressWarnings(resXY <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="incremental",digits=getOption("digits")))
  suppressWarnings(resX <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="traditional",digits=getOption("digits")))
  expect_that(nrow(resXY),equals(4))  # no T- row
  expect_that(nrow(resX),equals(3))   # no T row
  expect_that(all(round(resXY[,"Estimate"]-psdXYs,7)==0),is_true())
  expect_that(all(round(resX[,"Estimate"]-psdXs,7)==0),is_true())
})

test_that("Does manual calculation after psdAdd() equal psdCalc() results",{
  ## simulate data set
  set.seed(898343789)
  dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
  dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
  dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
  dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
  dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
  dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
  df <- rbind(dbg,dlb,dbt)
  df$species1 <- recodeSpecies(df,~species,oldn=c("LMB"),newn=c("Largemouth Bass"))

  ## get psdCalc results for LMB and BG .. ultimately compare to psdDataPrep results
  suppressWarnings(psdBG <- psdCalc(~tl,data=Subset(df,species1=="Bluegill"),species="Bluegill",digits=getOption("digits")))
  suppressWarnings(psdLMB <- psdCalc(~tl,data=Subset(df,species1=="Largemouth Bass"),species="Largemouth Bass",digits=getOption("digits")))
  
  ## apply psdAdd
  df$PSDcat <- psdAdd(df,tl~species1)
  # remove substock and other fish
  df <- Subset(df,species1 %in% c("Bluegill","Largemouth Bass"))
  df <- Subset(df,PSDcat!="zero")
  res <- prop.table(xtabs(~species1+PSDcat,data=df),margin=1)*100
  ## do PSD X-Y results match for two species
  expect_that(all(round(res["Bluegill",1:3]-psdBG[3:5,"Estimate"],7)==0),is_true())
  expect_that(all(round(res["Largemouth Bass",1:3]-psdLMB[3:5,"Estimate"],7)==0),is_true())  
})