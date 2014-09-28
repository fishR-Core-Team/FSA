context("recodeF() and capFirsst() function Messages")

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

test_that("recodeF() errors and warnings",{
  ## bad vector type
  expect_error(recodeF(df$tl),"must be a factor")
  ## bad formulae
  expect_error(recodeF(~tl,df),"must be a factor")
  expect_error(recodeF(~tl+species,df),"only one variable")
  expect_error(recodeF(tl~species,df),"only one variable")
  ## bad ocodes and ncodes types/classes
  expect_error(recodeF(df$species,c(1,2),c("Bluegill","Largemouth Bass")),"must be a character")
  expect_error(recodeF(df$species,c("BG","LMB"),c(1,2)),"must be a character")
  ## different numbers of codes
  expect_error(recodeF(df$species,c("LMB")),"missing")
  expect_error(recodeF(df$species,n=c("LMB")),"missing")
  expect_error(recodeF(df$species,c("LMB"),c("Bluegill","Largemouth Bass")),"must be equal")
  expect_error(recodeF(df$species,c("BG","LMB"),c("Largemouth Bass")),"must be equal")
})

test_that("recodeF() return type",{
  expect_is(recodeF(df$species,c("LMB"),c("Largemouth Bass")),"factor")
  expect_is(recodeF(df$junk,c("Derek"),c("Doctor")),"character")
})


test_that("capFirst() capitalizations are correct",{
  expect_equivalent(levels(factor(capFirst(df$species))),c("Bluefin Tuna","Bluegill","Lmb"))
  expect_equivalent(levels(factor(capFirst(df$species,which="first"))),c("Bluefin tuna","Bluegill","Lmb"))
                    
  df$species1 <- recodeF(df$species,c("LMB"),c("Largemouth Bass"))
  expect_equivalent(levels(factor(capFirst(df$species1))),c("Bluefin Tuna","Bluegill","Largemouth Bass"))
  expect_equivalent(levels(factor(capFirst(df$species1,which="first"))),c("Bluefin tuna","Bluegill","Largemouth bass"))
})
