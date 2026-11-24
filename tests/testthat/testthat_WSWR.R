## Test Messages ----
test_that("wsVal() messages",{
  #===== bad species name
  expect_error(wsVal("Derek"),
               "There is no Ws equation in 'WSlit' for \"Derek\"")
  expect_error(wsVal("Largemouth bass"),
               "There is no Ws equation in 'WSlit' for \"Largemouth bass\"")
  expect_error(wsVal("bluegill"),
               "There is no Ws equation in 'WSlit' for \"bluegill\"")
  #===== suggests reasonable alternatives
  withr::local_options(list(width=150))
  err <- expect_error(wsVal("Bro Trut"),"Did you mean")
  expect_match(conditionMessage(err),"Brook Trout")
  expect_match(conditionMessage(err),"Brown Trout")
  
  err <- expect_error(wsVal("Sau"),"Did you mean")
  expect_match(conditionMessage(err),"Sauger")
  expect_match(conditionMessage(err),"Saugeye")
  
  err <- expect_error(wsVal("Yell"),"Did you mean")
  expect_match(conditionMessage(err),"Yellow Bass")
  expect_match(conditionMessage(err),"Yellow Bullhead")
  expect_match(conditionMessage(err),"Yellow Perch")
  
  ## no suggestions if there would be too many suggestions
  err <- expect_error(wsVal("S"),
                      "There is no Ws equation in 'WSlit'")
  expect_false(grepl("Did you mean",conditionMessage(err)))
  
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
  #===== need groups, but trying group in parentheses with species
  expect_error(wsVal("Walleye (junk)"),
               "There is no Ws equation in \'WSlit\' for \"Walleye ")
  expect_warning(wsVal("Walleye (overall)",group="junk"),
                 "There are no groups for \"Walleye ")
  expect_no_error(wsVal("Walleye (overall)"))
  
  #===== bad units
  #----- typed wrong
  expect_error(wsVal("Bluegill",units="inches"),"should be one of")
  #----- don't exist for the species
  expect_error(wsVal("Ruffe",units="English"),
               "There is no Ws equation in 'English' units for \"Ruffe\"")
  #===== reference value does not exist
  expect_error(wsVal("Bluegill",ref=30),
               "There is no Ws equation for \"Bluegill\" with a reference")
  expect_error(wsVal("Bluegill",ref=50),
               "There is no Ws equation for \"Bluegill\" with a reference")
  expect_error(wsVal("Ruffe",ref=30),
               "There is no Ws equation for \"Ruffe\" with a reference")
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
  #===== Get data
  #----- Species with no "issues" + random numeric variable
  df1 <- subset(PSDWRtest,
                species %in% c("Yellow Perch","Largemouth Bass","Iowa Darter"))
  df1$rndn <- runif(nrow(df1))

  #----- Species with some "issues"
  df2 <- subset(PSDWRtest,
                species %in% c("Bluegill Sunfish","Ruffe","Walleye","Iowa Darter"))
  
   
  #===== bad units
  expect_error(wrAdd(wt~len+species,df1,units="inches"),"should be one of")
  
  #===== bad formulae
  expect_error(wrAdd(~len,df1),"must have one variable on the left-hand-side")
  expect_error(wrAdd(~len+species,df1),"must have one variable on the left-hand-side")
  expect_error(wrAdd(~len+species+wt,df1),"must have a left-hand-side")
  expect_error(wrAdd(wt~len,df1),"must have one variable on the left-hand-side")
  expect_error(wrAdd(wt~species,df1),"must have one variable on the left-hand-side")
  expect_error(wrAdd(wt~len+rndn,df1),"must have one and only one numeric variable")
  expect_error(wrAdd(wt~species+location,df1),"must have one and only one numeric variable")
  expect_error(wrAdd(wt~len+species+location,df1),"must have one variable on the left-hand-side")
  expect_error(wrAdd(wt+len~species,df1),"does not work with more than one variable on the")
  expect_error(wrAdd(wt~len+rndn+species,df1),"must have one variable on the left-hand-side")
  
  #===== bad vector types
  expect_error(wrAdd(species~wt+len,df1),"Variable on left-hand-side of \'wt\' is not numeric")
  expect_error(wrAdd(df1$species,df1$wt,df1$len),"\'wt\' must be numeric")
  expect_error(wrAdd(df1$wt,df1$species,df1$len),"\'len\' must be numeric")
  expect_error(wrAdd(df1$wt,df1$len,df1$rndn),"\'spec\' must be character or factor")
  
  #===== need to use WsOpts
  expect_error(wrAdd(wt~len+species,df2),
               "More than one Ws equation exists for \"Ruffe\"") %>%
    expect_output("Ruffe")
  expect_error(wrAdd(wt~len+species,df2,
                     WsOpts=list(Bluegill=list(group="overall"))),
               "More than one Ws equation exists for \"Ruffe\"") %>%
    expect_output("Ruffe")
  expect_error(wrAdd(wt~len+species,df2,
                     WsOpts=list(Ruffe=list(ref=50))),
               "More than one Ws equation exists for \"Walleye\"") %>%
    expect_output("Walleye")
  expect_error(wrAdd(wt~len+species,df2,
                     WsOpts=list(Ruffe=list(ref=50),
                                 Walleye=list(ref=50))),
               "Use of 'ref=50' for \"Walleye\" did not return")
  expect_error(wrAdd(wt~len+species,df2,
                     WsOpts=list(Ruffe=list(junk=50))),
               "'junk' in 'WsOpts=' must be one of")
  expect_no_error(wrAdd(wt~len+species,data=df2,
                        WsOpts=list(Walleye=list(group="overall"),
                                    Ruffe=list(ref=75))))
  
  #===== bad thesaurus
  expect_error(wrAdd(wt~len+species,df2,thesaurus=c("Bluegill"),
                     WsOpts=list(Walleye=list(group="overall"),
                                 Ruffe=list(ref=75))),
               "Values in \'thesaurus\' must be named")
  expect_error(wrAdd(wt~len+species,df2,thesaurus=c("Bluegill"=7),
                     WsOpts=list(Walleye=list(group="overall"),
                                 Ruffe=list(ref=75))),
               "Values in \'thesaurus\' must be strings of species names")
  expect_error(wrAdd(wt~len+species,df2,thesaurus=factor(c("Bluegill"="Bluegill Sunfish")),
                     WsOpts=list(Walleye=list(group="overall"),
                                 Ruffe=list(ref=75))),
               "\'thesaurus\' must be either a vector or list")
  tmp <- wrAdd(wt~len+species,df2,thesaurus=c("bluegill"="Bluegill Sunfish"),
               WsOpts=list(Walleye=list(group="overall"),
                           Ruffe=list(ref=75))) %>%
    expect_message("The following species names were in \'thesaurus\' but do not") 
})


## Test Output Types ----
test_that("wsVal() results",{
  #===== Do Bluegill results match ... example with no group or quad
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
  #===== Do Ruffe results match ... example with quad
  ruf1 <- wsVal("Ruffe",ref=75)
  ruf2 <- WSlit[WSlit$species=="Ruffe" & WSlit$units=="metric" & WSlit$ref=="75",]
  ruf2 <- ruf2[!names(ruf2) %in% c("group","comment")]
  expect_equal(ruf1,ruf2,ignore_attr=TRUE)
  ruf1 <- wsVal("Ruffe",ref=75,simplify=TRUE)
  ruf2 <- WSlit[WSlit$species=="Ruffe" & WSlit$units=="metric" & WSlit$ref=="75",]
  ruf2 <- ruf2[,names(ruf2) %in% c("species","min.len","max.len","int","slope","quad")]
  expect_equal(ruf1,ruf2,ignore_attr=TRUE)
  #===== Do Walleye results match ... example with a sub-group
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
  #===== Do Walleye results match ... example with a sub-group in the species name
  wae1 <- wsVal("Walleye (overall)")
  wae2 <- WSlit[WSlit$species=="Walleye (overall)" & WSlit$units=="metric",]
  wae2 <- wae2[,!names(wae2) %in% c("group","max.len","quad","comment")]
  expect_equal(wae1,wae2,ignore_attr=TRUE)
  wae1 <- wsVal("Walleye (overall)",units="English")
  wae2 <- WSlit[WSlit$species=="Walleye (overall)" & WSlit$units=="English",]
  wae2 <- wae2[,!names(wae2) %in% c("group","max.len","quad","comment")]
  expect_equal(wae1,wae2,ignore_attr=TRUE)
  wae1 <- wsVal("Walleye (overall)",simplify=TRUE)
  wae2 <- WSlit[WSlit$species=="Walleye (overall)" & WSlit$units=="metric",]
  wae2 <- wae2[,names(wae2) %in% c("species","min.len","int","slope")]
  expect_equal(wae1,wae2,ignore_attr=TRUE)
  
  #===== Check list output
  expect_message(capture.output(wsVal("List")),"must be one of following")
  expect_output(suppressMessages(wsVal("List")))
})


## Validate Results ----
test_that("wrAdd() matches values computed 'by hand' in Excel.",{
  # Read in external CSV file
  ftmp <- system.file("extdata","PSDWR_data4testthat.csv",package="FSA")
  tmp <- read.csv(ftmp)
  # Create a thesaurus for a couple of species
  thes <- c("Bluegill"="Bluegill Sunfish",
            "Lake Trout"="Lean Lake Trout")
  # Add wr variable as calculated in FSA
  tmp$wr2 <- wrAdd(wt~len+species2,data=tmp,
                   thesaurus=thes,
                   WsOpts=list(Ruffe=list(ref=75)))
  # Compare  
  expect_equal(tmp$wr,tmp$wr2)
})
