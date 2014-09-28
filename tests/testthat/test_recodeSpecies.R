context("recodeSpecies function Messages")

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

test_that("recodeSpecies() errors and warnings",{
  ## bad formulae
  expect_that(recodeSpecies(df,~tl),throws_error())
  expect_that(recodeSpecies(df,~tl+species),throws_error())
  
  ## different numbers of names
  expect_that(recodeSpecies(df,~species,oldnames=c("LMB")),gives_warning())
  expect_that(recodeSpecies(df,~species,newnames=c("Largemouth Bass")),gives_warning())
  expect_that(recodeSpecies(df,~species,oldnames=c("LMB"),newnames=c("Bluegill","Largemouth Bass")),throws_error())
  expect_that(recodeSpecies(df,~species,oldnames=c("BG","LMB"),newnames=c("Largemouth Bass")),throws_error())
  
  ## nothing to do
  expect_that(recodeSpecies(df,~species,doCapFirst="none"),gives_warning())  
})

test_that("recodeSpecies() Capitalizations are correct",{
  
  expect_that(levels(recodeSpecies(df,~species,doCapFirst="all")),
              equals(c("Bluefin Tuna","Bluegill","Lmb")))
  expect_that(levels(recodeSpecies(df,~species,doCapFirst="first")),
              equals(c("Bluefin tuna","Bluegill","Lmb")))
#  expect_that(levels(recodeSpecies(df,~species,oldn="LMB",
#                                   newn="Largemouth Bass",
#                                   doCapFirst="none")),
#              equals(c("bluefin tuna","Bluegill","Largemouth Bass")))
  expect_that(levels(recodeSpecies(df,~species,oldn="LMB",
                                   newn="Largemouth Bass",
                                   doCapFirst="all")),
              equals(c("Bluefin Tuna","Bluegill","Largemouth Bass")))
  expect_that(levels(recodeSpecies(df,~species,oldn="LMB",
                                   newn="Largemouth Bass",
                                   doCapFirst="first")),
              equals(c("Bluefin tuna","Bluegill","Largemouth bass")))
})
