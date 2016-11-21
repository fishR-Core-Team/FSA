context("wsXXX and wrXXX functions MESSAGES")

test_that("wsVal() messages",{
  ## bad species name
  expect_error(wsVal("Derek"),"A Ws equation may not exist given your choices")
  ## too many species name
  expect_error(wsVal(c("Bluegill","Yellow Perch")),"must contain only one name")
  ## bad units
  # typed wrong
  expect_error(wsVal("Bluegill",units="inches"),"should be one of")
  # don't exist for the species
  expect_error(wsVal("Ruffe",units="English"),"A Ws equation may not exist given your choices")
  ## reference value does not exist
  expect_error(wsVal("Bluegill",ref=50),"A Ws equation may not exist given your choices")
})

test_that("wrAdd() messages",{
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
  df$rnd <- runif(nrow(df))
  df$junk <- sample(c("Derek","Hugh","Ogle"),nrow(df),replace=TRUE)
  
  ## bad units
  expect_error(wrAdd(wt~tl+species,df,units="inches"),"should be one of")
  
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
