## Test Messages ----
test_that("wsVal() messages",{
  ## bad species name
  expect_error(wsVal("Derek"),
               "A Ws equation may not exist given your choices")
  ## too many species name
  expect_error(wsVal(c("Bluegill","Yellow Perch")),
               "must contain only one name")
  ## bad units
  # typed wrong
  expect_error(wsVal("Bluegill",units="inches"),
               "should be one of")
  # don't exist for the species
  expect_error(wsVal("Ruffe",units="English"),
               "A Ws equation may not exist given your choices")
  ## reference value does not exist
  expect_error(wsVal("Bluegill",ref=50),
               "A Ws equation may not exist given your choices")
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


## Test Output Types ----
test_that("wsVal() results",{
  ## Do Bluegill results match
  bg1 <- wsVal("Bluegill")
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="metric",]
  bg2 <- bg2[,-which(names(bg2) %in% c("max.len","quad","comment"))]
  expect_equivalent(bg1,bg2)
  bg1 <- wsVal("Bluegill",units="English")
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="English",]
  bg2 <- bg2[,-which(names(bg2) %in% c("max.len","quad","comment"))]
  expect_equivalent(bg1,bg2)
  bg1 <- wsVal("Bluegill",units="English",simplify=TRUE)
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="English",]
  bg2 <- bg2[,which(names(bg2) %in% c("species","min.len","int","slope"))]
  expect_equivalent(bg1,bg2)
  ## Do Ruffe results match
  ruf1 <- wsVal("Ruffe")
  ruf2 <- WSlit[WSlit$species=="Ruffe" & WSlit$units=="metric" & WSlit$ref=="75",]
  expect_equivalent(ruf1,ruf2)
  ruf1 <- wsVal("Ruffe",simplify=TRUE)
  ruf2 <- WSlit[WSlit$species=="Ruffe" & WSlit$units=="metric" & WSlit$ref=="75",]
  ruf2 <- ruf2[,which(names(ruf2) %in% c("species","min.len","max.len",
                                         "int","slope","quad"))]
  expect_equivalent(ruf1,ruf2)
  ##
  expect_message(capture.output(wsVal("List")),"must be one of following")
  expect_output(suppressMessages(wsVal("List")))
})


## Validate Results ----
test_that("wrAdd() matches values computed in Excel.",{
  # Read in external CSV file
  ftmp <- system.file("extdata","PSDWR_testdata.csv",package="FSA")
  df <- read.csv(ftmp)

  df$wr <- wrAdd(wt~tl+species,data=df)
  expect_equivalent(df$wr,df$WR)
})
