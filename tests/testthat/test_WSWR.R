context("wsXXX and wrXXX function Tests")

# ############################################################
# ============================================================
# Messaging
# ============================================================
# ############################################################

test_that("wsVal() errors and warnings",{
  ## bad species name
  expect_that(wsVal("Derek"),throws_error())
  ## too many species name
  expect_that(wsVal(c("Bluegill","Yellow Perch")),throws_error())
  ## bad units
  # typed wrong
  expect_that(wsVal("Bluegill",units="inches"),throws_error())
  # don't exist for the species
  expect_that(wsVal("Ruffe",units="English"),throws_error())
  ## reference value does not exist
  expect_that(wsVal("Bluegill",ref=50),throws_error())
})

test_that("wrAdd() errors and warnings",{
  ## simulate data set
  set.seed(345234534)
  dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
  dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
  dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
  dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
  dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
  dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
  df <- rbind(dbt,dbg,dlb)
  df$rnd <- runif(nrow(df))
  df$junk <- sample(c("Derek","Hugh","Ogle"),nrow(df),replace=TRUE)
  df$species <- recodeF(df$species,"LMB","Largemouth Bass")
  
  ## bad units
  expect_error(wrAdd(wt~tl+species,df,units="inches"),"units")
  
  ## bad formulae
  expect_error(wrAdd(~tl,df),"one variable")
  expect_error(wrAdd(~tl+species,df),"one variable")
  expect_error(wrAdd(~tl+species+wt,df),"left-hand-side")
  expect_error(wrAdd(wt~tl,df),"one variable")
  expect_error(wrAdd(wt~species,df),"one variable")
  expect_error(wrAdd(wt~tl+rnd,df),"only one numeric")
  expect_error(wrAdd(wt~species+junk,df),"only one numeric")
  expect_error(wrAdd(wt~tl+species+junk,df),"one variable")
  expect_error(wrAdd(wt+tl~species,df),"more than one variable")
  expect_error(wrAdd(wt~tl+rnd+species,df),"one variable")
  
  ## bad vector types
  expect_error(wrAdd(species~wt+tl,df),"not numeric")
  expect_error(wrAdd(df$species,df$wt,df$tl),"numeric")
  expect_error(wrAdd(df$wt,df$species,df$tl),"numeric")
  expect_error(wrAdd(df$wt,df$tl,df$rnd),"factor")
})


# ############################################################
# ============================================================
# Analytical Results
# ============================================================
# ############################################################
# ------------------------------------------------------------
# read in external CSV file
# ------------------------------------------------------------
ftmp <- system.file("extdata", "PSDWR_testdata.csv", package="FSA")
df <- read.csv(ftmp)

test_that("wrAdd() matches values computed in Excel.",{
  df$wr <- wrAdd(wt~tl+species,data=df)
  expect_equivalent(df$wr,df$WR)
})