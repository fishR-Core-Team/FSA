context("wsXXX and wrXXX function Messages")

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
  dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
  dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
  dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
  dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
  dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
  dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
  df <- rbind(dbg,dlb,dbt)
  df$rnd <- runif(nrow(df))
  df$junk <- sample(c("Derek","Hugh","Ogle"),nrow(df),replace=TRUE)
  df$species <- recodeSpecies(df,~species,oldn=c("LMB"),newn=c("Largemouth Bass"))
  
  ## bad formulae
  expect_that(wrAdd(df,~tl),throws_error())
  expect_that(wrAdd(df,~tl+species),throws_error())
  expect_that(wrAdd(df,~tl+species+wt),throws_error())
  expect_that(wrAdd(df,wt~tl),throws_error())
  expect_that(wrAdd(df,wt~species),throws_error())
  expect_that(wrAdd(df,wt~tl+rnd),throws_error())
  expect_that(wrAdd(df,wt~species+junk),throws_error())
  expect_that(wrAdd(df,wt~tl+species+junk),throws_error())
  expect_that(wrAdd(df,wt+tl~species),throws_error())
  expect_that(wrAdd(df,wt~tl+rnd+species),throws_error())
  expect_that(wrAdd(df,species~wt+junk),throws_error())
})
