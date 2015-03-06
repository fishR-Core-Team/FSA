context("capFirst() function Messages")

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

test_that("capFirst() capitalizations are correct",{
  expect_equivalent(levels(factor(capFirst(df$species))),c("Bluefin Tuna","Bluegill","Lmb"))
  expect_equivalent(levels(factor(capFirst(df$species,which="first"))),c("Bluefin tuna","Bluegill","Lmb"))
})

test_that("capFirst() returned classes are correct",{
  vec <- c("Derek Ogle","derek ogle","Derek ogle","derek Ogle","DEREK OGLE")
  vec1 <- capFirst(vec)
  expect_equivalent(class(vec),class(vec1))
  expect_equivalent(class(vec1),"character")
  fvec <- factor(vec)
  fvec1 <- capFirst(fvec)
  expect_equivalent(class(fvec),class(fvec1))
  expect_equivalent(class(fvec1),"factor")
})
