## Test Messages ----
test_that("wsVal() messages",{
  #===== bad species name
  expect_error(wsVal("Derek"),
               "There is no Ws equation in 'WSlit' for \"Derek\"")
  expect_error(wsVal("Largemouth bass"),
               "There is no Ws equation in 'WSlit' for \"Largemouth bass\"")
  expect_error(wsVal("bluegill"),
               "There is no Ws equation in 'WSlit' for \"bluegill\"")
  #===== too many species name
  expect_error(wsVal(c("Bluegill","Yellow Perch")),
               "must contain only one name")
  #===== need groups
  expect_error(wsVal("Walleye"),
               "\"Walleye\" has Ws equations for these sub-groups:")
  expect_error(wsVal("Walleye",group="Derek"),
               "There is no \"Derek\" group for \"Walleye\"")
  expect_warning(wsVal("Bluegill",group="Derek"),
               "There are no groups for \"Bluegill\"; thus,")
  
  #===== bad units
  #----- typed wrong
  expect_error(wsVal("Bluegill",units="inches"),"should be one of")
  #----- don't exist for the species
  expect_error(wsVal("Ruffe",units="English"),
               "There is no Ws equation in 'English' units for \"Ruffe\"")
  #===== reference value does not exist
  expect_error(wsVal("Bluegill",ref=30),
               "A 'ref' of 30 is non-standard and does not exist")
  expect_error(wsVal("Bluegill",ref=50),
               "There is no Ws equation for 'ref=50' for \"Bluegill\"")
  #===== bad choices for method
  expect_error(wsVal("Bluegill",method="Derek"),
               "There is no Ws equation for \"Bluegill\" derived from")
  expect_error(wsVal("Brook Trout",group="overall",method="EmP"),
               "There is no Ws equation for \"Brook Trout\" derived from")
  expect_error(wsVal("Blue Sucker",method="RLP"),
               "There is no Ws equation for \"Blue Sucker\" derived from")
  expect_error(wsVal("Arctic Grayling"),
               "Ws equations exist for both the RLP and EmP")
  expect_error(wsVal("Arctic Grayling",method="Derek"),
               "There is no Ws equation for \"Arctic Grayling\" derived")
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
  ## Do Bluegill results match ... example with no group or quad
  bg1 <- wsVal("Bluegill")
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="metric",]
  bg2 <- bg2[,!names(bg2) %in% c("group","max.len","quad","comment")]
  expect_equal(bg1,bg2,ignore_attr=TRUE)
  bg1 <- wsVal("Bluegill",units="English")
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="English",]
  bg2 <- bg2[,!names(bg2) %in% c("group","max.len","quad","comment")]
  expect_equal(bg1,bg2,ignore_attr=TRUE)
  bg1 <- wsVal("Bluegill",units="English",simplify=TRUE)
  bg2 <- WSlit[WSlit$species=="Bluegill" & WSlit$units=="English",]
  bg2 <- bg2[,names(bg2) %in% c("species","min.len","int","slope")]
  expect_equal(bg1,bg2,ignore_attr=TRUE)
  ## Do Ruffe results match ... example with quad
  ruf1 <- wsVal("Ruffe")
  ruf2 <- WSlit[WSlit$species=="Ruffe" & WSlit$units=="metric" & WSlit$ref=="75",]
  ruf2 <- ruf2[!names(ruf2) %in% c("group","comment")]
  expect_equal(ruf1,ruf2,ignore_attr=TRUE)
  ruf1 <- wsVal("Ruffe",simplify=TRUE)
  ruf2 <- WSlit[WSlit$species=="Ruffe" & WSlit$units=="metric" & WSlit$ref=="75",]
  ruf2 <- ruf2[,names(ruf2) %in% c("species","min.len","max.len","int","slope","quad")]
  expect_equal(ruf1,ruf2,ignore_attr=TRUE)
  ## Do Walleye results match ... example with a sub-group
  wae1 <- wsVal("Walleye",group="overall")
  wae2 <- WSlit[WSlit$species=="Walleye" & WSlit$group=="overall" & WSlit$units=="metric",]
  wae2 <- wae2[,!names(wae2) %in% c("max.len","quad","comment")]
  expect_equal(wae1,wae2,ignore_attr=TRUE)
  wae1 <- wsVal("Walleye",group="overall",units="English")
  wae2 <- WSlit[WSlit$species=="Walleye" & WSlit$group=="overall" & WSlit$units=="English",]
  wae2 <- wae2[,!names(wae2) %in% c("max.len","quad","comment")]
  expect_equal(wae1,wae2,ignore_attr=TRUE)
  wae1 <- wsVal("Walleye",group="overall",simplify=TRUE)
  wae2 <- WSlit[WSlit$species=="Walleye" & WSlit$group=="overall" & WSlit$units=="metric",]
  wae2 <- wae2[,names(wae2) %in% c("species","min.len","int","slope")]
  expect_equal(wae1,wae2,ignore_attr=TRUE)
  
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
  expect_equal(df$wr,df$WR)
})
