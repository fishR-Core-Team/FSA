context("psdXXX function Messages")

test_that("psdVal() errors and warnings",{
  ## bad species name
  expect_that(psdVal("Derek"),throws_error())
  ## bad units
  expect_that(psdVal("Bluegill",units="inches"),throws_error())
  ## too many species name
  expect_that(psdVal(c("Bluegill","Yellow Perch")),throws_error())
  ## addLens and addNames don't match up
  expect_that(psdVal("Bluegill",addLens=7,addNames=c("Derek","Ogle")),throws_error())
  expect_that(psdVal("Bluegill",addLens=c(7,9),addNames="Derek"),throws_error())
})


test_that("psdCI() errors and warnings",{
  ## problems with proportions table
  # not obviously a proportions table
#  expect_that(psdCI(c(1,0,0,0),c(0.5,0.3,0.2,10),11),gives_warning())
#  expect_that(psdCI(c(1,0,0,0),c(5,3,2,10),20),gives_warning())
  # looks like proportions, but doesn't sum to 1
  expect_that(psdCI(c(1,0,0,0),c(0.5,0.3,0.1,0.08),20),throws_error())
  expect_that(psdCI(c(1,0,0,0),c(0.5,0.3,0.2,0.08),20),throws_error())
  # small n for table using multinomial distribution
  expect_that(psdCI(c(0,0,0,1),c(0.5,0.3,0.1,0.1),20,method="multinomial"),gives_warning())
  
  ## problems with indicator vector
  ipsd <- c(0.130,0.491,0.253,0.123)
  n <- 445
  # all zeroes
  expect_that(psdCI(c(0,0,0,0),ipsd,n),throws_error())
  # all ones
  expect_that(psdCI(c(1,1,1,1),ipsd,n),throws_error())
  # wrong length of indvec
  expect_that(psdCI(c(1,0,0),ipsd,n),throws_error())
  expect_that(psdCI(c(1,0,0,0,0),ipsd,n),throws_error())
})

test_that("psdCalc() errors and warnings",{
  ## simulate data set
  df <- data.frame(tl=round(c(rnorm(100,mean=125,sd=15),
                              rnorm(50,mean=200,sd=25),
                              rnorm(20,mean=300,sd=40)),0),
                      species=rep("Yellow Perch",170))
  ## get Gabelhous lengths for Yellow Perch
  ghl <- psdVal("Yellow perch")
  ## restrict data.frame to sub-stock-length fish
  tmp <- subset(df,tl<ghl["stock"])
  expect_that(psdCalc(~tl,data=tmp,species="Yellow perch"),throws_error())
  ## restrict data.frame to no fish
  tmp <- subset(df,tl<ghl["zero"])
  expect_that(psdCalc(~tl,data=tmp,species="Yellow perch"),throws_error())
  
  ## no species name given
  expect_that(psdCalc(~tl,data=tmp),throws_error())
  
  ## bad formulae
  expect_that(psdCalc(tl,data=df,species="Yellow perch"),throws_error())
  expect_that(psdCalc(tl~species,data=df,species="Yellow perch"),throws_error())
  expect_that(psdCalc(~tl+species,data=df,species="Yellow perch"),throws_error())
  expect_that(psdCalc(~species,data=df,species="Yellow perch"),throws_error())
})



test_that("psdPlot() errors and warnings",{
  ## simulate data set
  df <- data.frame(tl=round(c(rnorm(100,mean=125,sd=15),
                              rnorm(50,mean=200,sd=25),
                              rnorm(20,mean=300,sd=40)),0),
                   species=rep("Yellow Perch",170))
  ## get Gabelhous lengths for Yellow Perch
  ghl <- psdVal("Yellow perch")

  ## set minimum length higher than stock length
  expect_that(psdPlot(~tl,data=df,species="Yellow perch",xlim=c(ghl["stock"]+10,300)),throws_error())
  
  ## restrict data.frame to no fish
  tmp <- subset(df,tl<ghl["zero"])
  expect_that(psdPlot(~tl,data=tmp,species="Yellow perch"),throws_error())
  
  ## bad formulae
  expect_that(psdPlot(tl,data=df,species="Yellow perch"),throws_error())
  expect_that(psdPlot(tl~species,data=df,species="Yellow perch"),throws_error())
  expect_that(psdPlot(~tl+species,data=df,species="Yellow perch"),throws_error())
  expect_that(psdPlot(~species,data=df,species="Yellow perch"),throws_error())
})

test_that("psdAdd() errors and warnings",{
  ## simulate data set
  set.seed(345234534)
  dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
  dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
  dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
  dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
  dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
  dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
  df <- rbind(dbt,dbg,dlb)
  df$species <- recodeF(df$species,"LMB","Largemouth Bass")
  
  ## bad units
  expect_error(psdAdd(tl~species,df,units="inches"),"units")
  expect_error(psdAdd(df$tl,df$species,units="inches"),"units")
  
  ## bad formulae
  expect_error(psdAdd(~tl,df),"one variable")
  expect_error(psdAdd(~species,df),"one variable")
  expect_error(psdAdd(tl~wt+species,df),"one variable")
  
  ## bad variable types
  expect_error(psdAdd(species~tl,df),"not numeric")
  expect_error(psdAdd(tl~wt,df),"only one factor")
  expect_error(psdAdd(df$species,df$tl),"numeric")
  expect_error(psdAdd(df$tl,df$wt),"factor")
})

