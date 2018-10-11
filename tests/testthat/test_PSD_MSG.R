context("PSD functions MESSAGES")
source("EXS_PSD.R")

test_that("psdVal() messages",{
  ## bad species name
  expect_error(psdVal("Derek"),"The Gabelhouse lengths do not exist")
  ## bad units
  expect_error(psdVal("Bluegill",units="inches"),"should be one of")
  ## too many species name
  expect_error(psdVal(c("Bluegill","Yellow Perch")),"can have only one name")
  ## addLens and addNames don't match up
  expect_error(psdVal("Bluegill",addLens=7,addNames=c("Derek","Ogle")),
               "have different lengths")
  expect_error(psdVal("Bluegill",addLens=c(7,9),addNames="Derek"),"have different lengths")
  ## and addLens is also a Gabelhouse length
  expect_warning(psdVal("Bluegill",addLens=150),
                 "At least one Gabelhouse length that was in")
})

test_that("psdCI() messages",{
  ## problems with proportions table
  # not obviously a proportions table
#  expect_warning(psdCI(c(1,0,0,0),c(0.5,0.3,0.2,10),11))
#  expect_warning(psdCI(c(1,0,0,0),c(5,3,2,10),20))
  # looks like proportions, but doesn't sum to 1
  expect_error(psdCI(c(1,0,0,0),c(0.5,0.3,0.1,0.08),20),"does not sum to 1")
  expect_error(psdCI(c(1,0,0,0),c(0.5,0.3,0.2,0.08),20),"does not sum to 1")
  # small n for table using multinomial distribution
  expect_warning(psdCI(c(0,0,0,1),c(0.5,0.3,0.1,0.1),20,method="multinomial"),"CI coverage may be lower than 95%")
  
  ## problems with indicator vector
  ipsd <- c(0.130,0.491,0.253,0.123)
  n <- 445
  # all zeros
  expect_error(psdCI(c(0,0,0,0),ipsd,n),"cannot be all zeros")
  # all ones
  expect_error(psdCI(c(1,1,1,1),ipsd,n),"cannot be all ones")
  # wrong length of indvec
  expect_error(psdCI(c(1,0,0),ipsd,n),"must be the same")
  expect_error(psdCI(c(1,0,0,0,0),ipsd,n),"must be the same")
  
  ## ptbl not proportions
  ipsd <- c(5,4,3,3)
  n <- 300
  expect_warning(psdCI(c(1,0,0,0),ipsd,n),"not a table of proportions")
})

test_that("psdCalc() messages",{
  ## get Gabelhouse lengths for Yellow Perch
  ghl <- psdVal("Yellow perch")
  ## restrict data.frame to no fish
  tmp <- subset(df,tl<ghl["substock"])
  expect_error(psdCalc(~tl,data=tmp,species="Yellow perch"),
               "does not contain any rows")
  ## restrict data.frame to sub-stock-length fish
  tmp <- subset(df,tl<ghl["stock"])
  expect_error(psdCalc(~tl,data=tmp,species="Yellow perch"),
               "no stock-length fish in the sample")
  ## restrict data.frame to no >=quality fish
  tmp <- subset(df,tl<ghl["quality"])
  expect_warning(psdCalc(~tl,data=tmp,species="Yellow perch"),
                 "No 'quality' or larger fish in sample")
  
  ## no species name given
  expect_error(psdCalc(~tl,data=tmp),"Must include a species name in")
  
  ## bad formulae
  expect_error(psdCalc(tl,data=df,species="Yellow perch"),"not found")
  expect_error(psdCalc(tl~species,data=df,species="Yellow perch"),
               "Function only works with formulas with 1 variable")
  expect_error(psdCalc(~tl+species,data=df,species="Yellow perch"),
               "Function only works with formulas with 1 variable")
  expect_error(psdCalc(~species,data=df,species="Yellow perch"),"must be numeric")
})

test_that("psdPlot() messages",{
  ## get Gabelhouse lengths for Yellow Perch
  ghl <- psdVal("Yellow perch")

  ## restrict data.frame to no fish
  tmp <- subset(df,tl<ghl["substock"])
  expect_error(psdPlot(~tl,data=tmp,species="Yellow perch"),
               "does not contain any rows")
  ## restrict data.frame to no >=quality fish
  tmp <- subset(df,tl<ghl["quality"])
  ## set minimum length higher than stock length
  expect_error(psdPlot(~tl,data=df,species="Yellow perch",
                       xlim=c(ghl["stock"]+10,300)),
               "Minimum length value in")
  
  ## bad formulae
  expect_error(psdPlot(tl,data=df,species="Yellow perch"),"not found")
  expect_error(psdPlot(tl~species,data=df,species="Yellow perch"),
               "Function only works with formulas with 1 variable.")
  expect_error(psdPlot(~tl+species,data=df,species="Yellow perch"),
               "Function only works with formulas with 1 variable.")
  expect_error(psdPlot(~species,data=df,species="Yellow perch"),"must be numeric")
})

test_that("psdAdd() messages",{
  ## bad units
  expect_error(psdAdd(tl~species,df,units="inches"),"should be one of")
  expect_error(psdAdd(df$tl,df$species,units="inches"),"should be one of")
  
  ## bad formulae
  expect_error(psdAdd(~tl,df),"one variable")
  expect_error(psdAdd(~species,df),"one variable")
  expect_error(psdAdd(tl~wt+species,df),"one variable")
  
  ## bad variable types
  expect_error(psdAdd(species~tl,df),"not numeric")
  expect_error(psdAdd(tl~wt,df),"only one factor")
  expect_error(psdAdd(df$species,df$tl),"numeric")
  expect_error(psdAdd(df$tl,df$wt),"factor")
  
  ## bad addSpec/addLens combination
  expect_warning(psdAdd(tl~species,df,addSpec="Derek",verbose=FALSE),"is not NULL")
  
  ## One species had all missing lengths
  tmp <- df
  tmp[df$species=="Bluegill","tl"] <- NA
  tmp <- tmp[tmp$species!="Bluefin Tuna",]
  expect_message(psdAdd(tl~species,tmp),"were missing for")
})

test_that("tictactoe() errors and warnings",{
  ## objective values do not contain 2 values
  expect_error(tictactoe(predobj=70),"must contain two numbers")
  expect_error(tictactoe(predobj=71:75),"must contain two numbers")
  expect_error(tictactoe(predobj=NULL),"must be numeric")
  expect_error(tictactoe(predobj=NA),"must be numeric")
  expect_error(tictactoe(predobj=c(-5,70)),"must be between 0 and 100")
  expect_error(tictactoe(predobj=c(70,105)),"must be between 0 and 100")
  expect_error(tictactoe(predobj=c(-5,105)),"must be between 0 and 100")
  expect_error(tictactoe(predobj=c("A","B")),"must be numeric")
  expect_error(tictactoe(preyobj=70),"must contain two numbers")
  expect_error(tictactoe(preyobj=71:75),"must contain two numbers")
  expect_error(tictactoe(preyobj=NULL),"must be numeric")
  expect_error(tictactoe(preyobj=NA),"must be numeric")
  expect_error(tictactoe(preyobj=c(-5,70)),"must be between 0 and 100")
  expect_error(tictactoe(preyobj=c(70,105)),"must be between 0 and 100")
  expect_error(tictactoe(preyobj=c(-5,105)),"must be between 0 and 100")
  expect_error(tictactoe(preyobj=c("A","B")),"must be numeric")
})
