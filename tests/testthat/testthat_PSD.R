## Preparation
#===== Read in external CSV file for validation (near end)
ftmp <- system.file("extdata", "PSDWR_data4testthat.csv", package="FSA")
df2 <- read.csv(ftmp)
df2$psd <- factor(df2$psd,levels=c("substock","stock","quality","preferred",
                                   "memorable","trophy"))
thes <- c("Bluegill"="Bluegill Sunfish","Lake Trout"="Lean Lake Trout")

test_that("psdVal() suggests equally close species names",{
  withr::local_options(list(width=150))
  err <- expect_error(psdVal("Bro Trut"),"Did you mean")
  expect_match(conditionMessage(err),"Brook Trout")
  expect_match(conditionMessage(err),"Brown Trout")
})

## Test Messages ----
test_that("psdVal() messages",{
  #===== bad species name
  expect_error(psdVal("Derek"),
               "There are no Gablehouse lengths in 'PSDlit'")
  expect_error(psdVal("bluegill"),
               "There are no Gablehouse lengths in 'PSDlit'")

  #===== suggests reasonable alternatives
  withr::local_options(list(width=150))
  err <- expect_error(psdVal("Bro Trut"),"Did you mean")
  expect_match(conditionMessage(err),"Brook Trout")
  expect_match(conditionMessage(err),"Brown Trout")
  
  err <- expect_error(psdVal("Sau"),"Did you mean")
  expect_match(conditionMessage(err),"Sauger")
  expect_match(conditionMessage(err),"Saugeye")
  
  err <- expect_error(psdVal("Yell"),"Did you mean")
  expect_match(conditionMessage(err),"Yellow Bass")
  expect_match(conditionMessage(err),"Yellow Bullhead")
  expect_match(conditionMessage(err),"Yellow Perch")
  
  ## no suggestions if there would be too many suggestions
  err <- expect_error(psdVal("S"),
                      "There are no Gablehouse lengths in 'PSDlit'")
  expect_false(grepl("Did you mean",conditionMessage(err)))
  
  #===== too many species name
  expect_error(psdVal(c("Bluegill","Yellow Perch")),
               "can have only one name")
  
  #===== needed and bad group=
  expect_error(psdVal("Brown Trout"),
               "\"Brown Trout\" has Gabelhouse categories for these")
  expect_error(psdVal("Brown Trout",group="Derek"),
               "There is no \"Derek\" group for \"Brown Trout\"")
  expect_error(psdVal("Brown Trout (Lotic)"),
               "There are no Gablehouse lengths in \'PSDlit\'")
  
  #===== bad units
  expect_error(psdVal("Bluegill",units="inches"),
               "should be one of")
  
  #===== addLens and addNames don't match up
  expect_error(psdVal("Bluegill",addLens=7,addNames=c("Derek","Ogle")),
               "\'addLens\' and \'addNames\' have different lengths")
  expect_error(psdVal("Bluegill",addLens=c(7,9),addNames="Derek"),
               "\'addLens\' and \'addNames\' have different lengths")
  
  #===== and addLens is also a Gabelhouse length
  expect_warning(psdVal("Bluegill",addLens=150),
                 "The following Gabelhouse length categories were removed")
})

test_that("psdAdd() messages",{
  #===== Get some data for multiple species
  df <- subset(PSDWRtest,
               species %in% c("Yellow Perch","Iowa Darter",
                              "Bluegill Sunfish","Brown Trout"),
               select=c("species","len","wt"))
  
  #===== bad variable types
  expect_error(psdAdd(species~len,df),
               "Variable on left-hand-side of \'len\' is not numeric")
  expect_error(psdAdd(len~wt,df),
               "\'len\' must have one and only one factor")
  expect_error(psdAdd(df$species,df$len),
               "\'len\' must be numeric")
  expect_error(psdAdd(df$len,df$wt),
               "\'species\' must be character or factor")
  
  #===== bad formulae
  expect_error(psdAdd(~len,df),
               "\'len\' must have one variable on the left-hand-side")
  expect_error(psdAdd(~species,df),
               "\'len\' must have one variable on the left-hand-side")
  expect_error(psdAdd(len~wt+species,df),
               "\'len\' must have one variable on the left-hand-side")
  
  #===== Sub-group and thesaurus issues (and messages about no lengths)
  psdAdd(len~species,data=df) %>%
    expect_message("Species in the data with no Gabelhouse") %>%
    expect_error("\"Brown Trout\" has Gabelhouse categories for these")
  tmp <- psdAdd(len~species,data=df,group=list("Brown Trout"="lotic")) %>%
    expect_message("Species in the data with no Gabelhouse")
  tmp <- psdAdd(len~species,data=df,group=list("Brown Trout"="lotic"),
                thesaurus=c("bluegill"="Bluegill Sunfish")) %>%
    expect_message("The following species names were in \'thesaurus\'") %>%
    expect_message("Species in the data with no Gabelhouse")
  psdAdd(len~species,data=df,group=list("Brown Trout"="lotic"),
         thesaurus=c("Bluegill Sunfish")) %>%
    expect_error("Values in \'thesaurus\' must be named")
  psdAdd(len~species,data=df,group=list("Brown Trout"="lotic"),
         thesaurus=c("Bluegill"=7)) %>%
    expect_error("Values in \'thesaurus\' must be strings")
  psdAdd(len~species,data=df,group=list("Brown Trout"="lotic"),
         thesaurus=c("Bluegill"=factor("Bluegill Sunfish"))) %>%
    expect_error("\'thesaurus\' must be either a vector or list")
  tmp <- psdAdd(len~species,data=df,group=list("Brown Trout"="lotic"),
                thesaurus=c("Bluegill"="Bluegill Sunfish")) %>%
    expect_message("Species in the data with no Gabelhouse") %>%
    expect_no_error()
  
  #===== bad units
  expect_error(psdAdd(len~species,df,units="inches"),
               "should be one of")
  expect_error(psdAdd(df$len,df$species,units="inches"),
               "should be one of")
  
  #===== One species had all missing lengths
  tmp <- subset(PSDWRtest,species=="Yellow Perch",select=c("species","len"))
  tmp$len <- NA_real_
  expect_message(psdAdd(len~species,tmp),
                 "All values in \'len\' were missing for Yellow Perch")
})

test_that("psdCalc() messages",{
  #===== Get some data for one species
  df <- subset(PSDWRtest,species=="Yellow Perch",select=c("species","len"))
  #===== get Gabelhouse lengths for Yellow Perch
  ghl <- psdVal("Yellow Perch")
  
  #===== species name does not exist in PSDlit
  expect_error(psdCalc(~len,data=df,species="Slimy Sculpin"),
               "There are no Gablehouse lengths in 'PSDlit'")
  expect_error(psdCalc(~len,data=df,species="Yellow perch"),
               "There are no Gablehouse lengths in 'PSDlit'")

  #===== needed and bad group=
  expect_error(psdCalc(~len,data=df,species="Brown Trout"),
               "\"Brown Trout\" has Gabelhouse categories for these")
  expect_error(psdCalc(~len,data=df,species="Brown Trout",group="Derek"),
               "There is no \"Derek\" group for \"Brown Trout\"")
  expect_error(psdCalc(~len,data=df,species="Brown Trout (Lotic)"),
               "There are no Gablehouse lengths in 'PSDlit'")
  psdCalc(~len,data=df,species="Brown Trout",group="lotic") %>%
    expect_warning("some CI coverage may be lower") %>%
    expect_no_failure()
  
  #===== data.frame issues
  #-----  restrict data.frame to no fish
  tmp <- subset(df,len<ghl["substock"])
  expect_error(psdCalc(~tl,data=tmp,species="Yellow Perch"),
               "\'data\' does not contain any rows")
  #-----  restrict data.frame to sub-stock-length fish
  tmp <- subset(df,len<ghl["stock"])
  expect_error(psdCalc(~len,data=tmp,species="Yellow Perch"),
               "There are no stock-length fish in the sample")
  #-----  restrict data.frame to no >=quality fish
  tmp <- subset(df,len<ghl["quality"])
  expect_warning(psdCalc(~len,data=tmp,species="Yellow Perch"),
                 "No fish in larger than 'stock' categories")
  
  #===== bad formulae
  expect_error(psdCalc(len,data=df,species="Yellow Perch"),
               "object \'len\' not found")
  expect_error(psdCalc(len~species,data=df,species="Yellow Perch"),
               "Function only works with formulas with 1 variable")
  expect_error(psdCalc(~len+species,data=df,species="Yellow Perch"),
               "Function only works with formulas with 1 variable")
  expect_error(psdCalc(~species,data=df,species="Yellow Perch"),
               "Variable in \'formula\' must be numeric")
  
  #===== testing confidence intervals
  expect_error(psdCalc(~len,data=df,species="Yellow Perch",conf.level=0),
               "\'conf.level\' must be between 0 and 1")
  expect_error(psdCalc(~len,data=df,species="Yellow Perch",conf.level=1),
               "\'conf.level\' must be between 0 and 1")
  expect_error(psdCalc(~len,data=df,species="Yellow Perch",conf.level="R"),
               "\'conf.level\' must be numeric")

  #===== Testing when species name not given
  #-----  no species name given or addLens given
  expect_error(psdCalc(~len,data=df),
               "Must include name in 'species' or lengths in 'addLens'")
  #-----  addLens must have at least two values
  expect_error(psdCalc(~len,data=df,addLens=c("stock"=100)),
               "'addLens' must contain at least two length categories")
  #-----  addLens must have at least a stock and quality length
  expect_error(psdCalc(~len,data=df,addLens=c("preferred"=100,"quality"=200)),
               "First category name must be 'stock'")
  expect_error(psdCalc(~len,data=df,addLens=c("Stock"=100,"quality"=200)),
               "First category name must be 'stock'")
  expect_error(psdCalc(~len,data=df,addLens=c(100,200),addNames=c("Stock","quality")),
               "First category name must be 'stock'")
  #-----  No names given
  expect_error(psdCalc(~len,data=df,addLens=c(100,200,250)),
               "Category names must be defined in 'addLens'")
  #-----  lengths of addLens and addNames do not match
  expect_error(psdCalc(~len,data=df,addLens=c(100,200),addNames=c("name1")),
               "'addLens' and 'addNames' cannot be different lengths")
  expect_error(psdCalc(~len,data=df,addLens=c(100,200),
                       addNames=c("name1","name2","name3")),
               "'addLens' and 'addNames' cannot be different lengths")
})

test_that("psdCI() messages",{
  #===== problems with proportions table
  #-----  not obviously a proportions table
  psdCI(c(1,0,0,0),c(0.5,0.3,0.2,10),11) %>%
    expect_warning("\'ptbl\' not a table of proportions") %>%
    expect_error("\'x\' must be whole numbers")
  expect_warning(psdCI(c(1,0,0,0),c(5,3,2,10),20),
                 "\'ptbl\' not a table of proportions")
  expect_warning(psdCI(c(1,0,0,0),c(5,4,3,3),300),
                 "\'ptbl\' not a table of proportions")
  #-----  looks like proportions, but doesn't sum to 1
  expect_error(psdCI(c(1,0,0,0),c(0.5,0.3,0.1,0.08),20),
               "\'ptbl\' does not sum to 1")
  expect_error(psdCI(c(1,0,0,0),c(0.5,0.3,0.2,0.08),20),
               "\'ptbl\' does not sum to 1")
  #-----  small n for table using multinomial distribution
  expect_warning(psdCI(c(0,0,0,1),c(0.5,0.3,0.1,0.1),20,method="multinomial"),
                 "CI coverage may be lower than")
  
  #===== problems with indicator vector
  ipsd <- c(0.130,0.491,0.253,0.123)
  n <- 445
  
  #-----  all zeros
  expect_error(psdCI(c(0,0,0,0),ipsd,n),
               "\'indvec\' cannot be all zeros")
  #-----  all ones
  expect_error(psdCI(c(1,1,1,1),ipsd,n),
               "\'indvec\' cannot be all ones")
  #-----  wrong length of indvec
  expect_error(psdCI(c(1,0,0),ipsd,n),
               "Length of \'ptbl\' and \'indvec\' must be the same")
  expect_error(psdCI(c(1,0,0,0,0),ipsd,n),
               "Length of \'ptbl\' and \'indvec\' must be the same")
  
  #===== bad args in conf.level
  expect_error(psdCI(c(0,0,1,1),ipsd,n=n,conf.level=0),
               "\'conf.level\' must be between 0 and 1")
  expect_error(psdCI(c(0,0,1,1),ipsd,n=n,conf.level=1),
               "\'conf.level\' must be between 0 and 1")
  expect_error(psdCI(c(0,0,1,1),ipsd,n=n,conf.level="R"),
               "\'conf.level\' must be numeric")
})

test_that("psdPlot() messages",{
  #===== Get some data for one species
  df <- subset(PSDWRtest,species=="Yellow Perch",select=c("species","len"))
  #===== get Gabelhouse lengths for Yellow Perch
  ghl <- psdVal("Yellow Perch")

  #===== data.frame issue ... restrict data.frame to no fish
  tmp <- subset(df,len<ghl["substock"])
  expect_error(psdPlot(~len,data=tmp,species="Yellow Perch"),
               "\'data\' does not contain any rows")

  #===== set minimum length higher than stock length
  expect_error(psdPlot(~len,data=df,species="Yellow Perch",
                       xlim=c(ghl["stock"]+10,300)),
               "Minimum length value in \'xlim\' is greater than")
  
  #===== bad formulae
  expect_error(psdPlot(len,data=df,species="Yellow Perch"),
               "object \'len\' not found")
  expect_error(psdPlot(len~species,data=df,species="Yellow Perch"),
               "Function only works with formulas with 1 variable.")
  expect_error(psdPlot(~len+species,data=df,species="Yellow Perch"),
               "Function only works with formulas with 1 variable.")
  expect_error(psdPlot(~species,data=df,species="Yellow Perch"),
               "Variable in \'formula\' must be numeric")
})

test_that("tictactoe() messages",{
  #===== objective values do not contain 2 values
  expect_error(tictactoe(predobj=70),
               "Predator target objective must contain two numbers")
  expect_error(tictactoe(predobj=71:75),
               "Predator target objective must contain two numbers")
  expect_error(tictactoe(preyobj=70),
               "Prey target objective must contain two numbers")
  expect_error(tictactoe(preyobj=71:75),
               "Prey target objective must contain two numbers")
  
  #===== Objective values contain wrong type of values
  expect_error(tictactoe(predobj=NULL),
               "Predator target objectives must be numeric")
  expect_error(tictactoe(predobj=NA),
               "Predator target objectives must be numeric")
  expect_error(tictactoe(predobj=c("A","B")),
               "Predator target objectives must be numeric")
  expect_error(tictactoe(predobj=c(-5,70)),
               "Predator target objectives must be between 0 and 100")
  expect_error(tictactoe(predobj=c(70,105)),
               "Predator target objectives must be between 0 and 100")
  expect_error(tictactoe(predobj=c(-5,105)),
               "Predator target objectives must be between 0 and 100")
  expect_error(tictactoe(preyobj=NULL),
               "Prey target objectives must be numeric")
  expect_error(tictactoe(preyobj=NA),
               "Prey target objectives must be numeric")
  expect_error(tictactoe(preyobj=c("A","B")),
               "Prey target objectives must be numeric")
  expect_error(tictactoe(preyobj=c(-5,70)),
               "Prey target objectives must be between 0 and 100")
  expect_error(tictactoe(preyobj=c(70,105)),
               "Prey target objectives must be between 0 and 100")
  expect_error(tictactoe(preyobj=c(-5,105)),
               "Prey target objectives must be between 0 and 100")
})


## Test Output Types ----
test_that("psdVal() returns",{
  ## check values for yellow Perch
  tmp <- psdVal("Yellow Perch",incl.zero=FALSE)
  expect_equal(tmp,c(130,200,250,300,380),ignore_attr=TRUE)
  expect_equal(names(tmp),c("stock","quality","preferred","memorable","trophy"))
  tmp <- psdVal("Yellow Perch",incl.zero=FALSE,units="in")
  expect_equal(tmp,c(5,8,10,12,15),ignore_attr=TRUE)
  expect_equal(names(tmp),c("stock","quality","preferred","memorable","trophy"))
  tmp <- psdVal("Yellow Perch",units="in")
  expect_equal(tmp,c(0,5,8,10,12,15),ignore_attr=TRUE)
  expect_equal(names(tmp),c("substock","stock","quality","preferred",
                            "memorable","trophy" ))
  tmp <- psdVal("Yellow Perch",units="in",addLens=c(7,9))
  expect_equal(tmp,c(0,5,7,8,9,10,12,15),ignore_attr=TRUE)
  expect_equal(names(tmp),c("substock","stock","7","quality","9",
                            "preferred","memorable","trophy" ))
  tmp <- psdVal("Yellow Perch",units="in",addLens=c(7,9),
                addNames=c("minSlot","maxSlot"))
  expect_equal(tmp,c(0,5,7,8,9,10,12,15),ignore_attr=TRUE)
  expect_equal(names(tmp),c("substock","stock","minSlot","quality","maxSlot",
                            "preferred","memorable","trophy"))
  tmp <- psdVal("Yellow Perch",units="in",addLens=c(minSlot=7,maxSlot=9),
                addNames=c("minSlot","maxSlot"))
  expect_equal(tmp,c(0,5,7,8,9,10,12,15),ignore_attr=TRUE)
  expect_equal(names(tmp),c("substock","stock","minSlot","quality","maxSlot",
                            "preferred","memorable","trophy"))
  tmp <- psdVal("Yellow Perch")
  expect_equal(tmp,c(0,130,200,250,300,380),ignore_attr=TRUE)
  expect_equal(names(tmp),c("substock","stock","quality","preferred",
                            "memorable","trophy"))
  tmp <- psdVal("Yellow Perch",showJustSource=TRUE)
  expect_equal(class(tmp),"data.frame")
  expect_equal(ncol(tmp),2)
  expect_equal(nrow(tmp),1)
})

test_that("psdAdd() returns",{
  tmp <- subset(PSDWRtest,species %in% c("Yellow Perch","Bluegill Sunfish",
                                         "Largemouth Bass","Lean Lake Trout"),
                select=c("species","len","wt"))
  tmp$PSD <- suppressMessages(psdAdd(len~species,data=tmp,thesaurus=thes))
  expect_equal(ncol(tmp),4)
  expect_true(is.factor(tmp$PSD))
  tmp$PSD <- suppressMessages(psdAdd(len~species,data=tmp,thesaurus=thes,use.names=FALSE))
  expect_equal(ncol(tmp),4)
  expect_true(is.numeric(tmp$PSD))
})

test_that("psdCalc() returns",{
  #===== Get some data for one species
  df <- subset(PSDWRtest,species=="Yellow Perch",select=c("species","len"))

  #===== All values
  tmp <- suppressWarnings(psdCalc(~len,data=df,species="Yellow Perch"))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),7)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-P","PSD-M","PSD S-Q",
                               "PSD Q-P","PSD P-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))

  #===== Traditional values
  tmp <- suppressWarnings(psdCalc(~len,data=df,species="Yellow Perch",
                                  what="traditional"))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-P","PSD-M"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))

  #===== Incremental values
  tmp <- suppressWarnings(psdCalc(~len,data=df,species="Yellow Perch",
                                  what="incremental"))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),4)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD S-Q","PSD Q-P","PSD P-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  #===== All values, but don't drop 0s
  tmp <- suppressWarnings(psdCalc(~len,data=df,species="Yellow Perch",
                                  drop0Est=FALSE))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),8)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-P","PSD-M","PSD-T","PSD S-Q",
                               "PSD Q-P","PSD P-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  #===== All values, with some additional lengths
  tmp <- suppressWarnings(psdCalc(~len,data=df,species="Yellow Perch",
                                  addLens=225,addNames="Derek"))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),9)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-Derek","PSD-P","PSD-M",
                               "PSD S-Q","PSD Q-Derek","PSD Derek-P",
                               "PSD P-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  #===== All values, with some additional lengths but no names
  tmp <- suppressWarnings(psdCalc(~len,data=df,species="Yellow Perch",
                                  addLens=c(225,245),drop0Est=FALSE))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),12)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-225","PSD-245","PSD-P","PSD-M",
                               "PSD-T","PSD S-Q","PSD Q-225","PSD 225-245",
                               "PSD 245-P","PSD P-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  #===== Just the additional values
  tmp <- suppressWarnings(psdCalc(~len,data=df,species="Yellow Perch",
                                  addLens=c(225,245),drop0Est=FALSE,
                                  justAdds=TRUE))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),5)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-225","PSD-245","PSD Q-225",
                               "PSD 225-245","PSD 245-P"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  #===== All values, but df only has values greater than stock values
  df1 <- droplevels(subset(df,len>=130))
  tmp <- suppressWarnings(psdCalc(~len,data=df1,species="Yellow Perch"))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),7)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-P","PSD-M",
                               "PSD S-Q","PSD Q-P","PSD P-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  #===== All values, but df only has values greater than quality values
  df1 <- droplevels(subset(df,len>=200))
  tmp <- suppressWarnings(psdCalc(~len,data=df1,species="Yellow Perch"))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),6)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-P","PSD-M",
                               "PSD Q-P","PSD P-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  #===== All values, but df only has values greater than memorable value
  df1 <- droplevels(subset(df,len>=300))
  tmp <- suppressWarnings(psdCalc(~len,data=df1,species="Yellow Perch"))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),4)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-P","PSD-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  #===== Pretend like no species is given
  tmp <- suppressWarnings(psdCalc(~len,data=df,
                                  addLens=c("stock"=130,"quality"=200,"preferred"=250,
                                            "memorable"=300,"trophy"=380)))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),7)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-Q","PSD-P","PSD-M",
                               "PSD S-Q","PSD Q-P","PSD P-M","PSD M-T"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  tmp <- suppressWarnings(psdCalc(~len,data=df,
                                  addLens=c("stock"=130,"name1"=200,"name2"=250)))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),4)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-name1","PSD-name2","PSD S-name1","PSD name1-name2"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))

  tmp <- suppressWarnings(psdCalc(~len,data=df,
                                  addLens=c("stock"=130,"name1"=200)))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),2)
  expect_equal(ncol(tmp),3)
  expect_equal(rownames(tmp),c("PSD-name1","PSD S-name1"))
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
})

test_that("psdCI() returns",{
  #===== Make an incremental proportions table
  n <- 997
  ipsd <- c(130,491,253,123)/n
  
  #===== single binomial
  tmp <- psdCI(c(0,0,1,1),ipsd,n=n)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),3)
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))

  tmp <- psdCI(c(1,0,0,0),ipsd,n=n,label="PSD S-Q")
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),3)
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  expect_equal(rownames(tmp),"PSD S-Q")
  
  #===== single multinomial
  tmp <- psdCI(c(0,0,1,1),ipsd,n=n,method="multinomial")
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),3)
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  
  tmp <- psdCI(c(1,0,0,0),ipsd,n=n,method="multinomial",label="PSD S-Q")
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),3)
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  expect_equal(rownames(tmp),"PSD S-Q")
  
  #===== Adjustment for not proportions
  ipsd <- c(130,491,253,123)
  n <- sum(ipsd)
  ipsd2 <- ipsd/n
  
  tmp <- suppressWarnings(psdCI(c(0,0,1,1),ipsd,n=n))
  tmp2 <- psdCI(c(0,0,1,1),ipsd2,n=n)
  expect_equal(tmp,tmp2)
  
  tmp <- suppressWarnings(psdCI(c(0,0,1,1),ipsd,n=n,method="multinomial"))
  tmp2 <- psdCI(c(0,0,1,1),ipsd2,n=n,method="multinomial")
  expect_equal(tmp,tmp2)
  
  ## Adjustment for a PSD of 0 or 1
  ipsd <- c(1,0,0,0)
  n <- 455
  tmp <- psdCI(c(1,1,0,0),ipsd,n=n)
  expect_equal(tmp,matrix(c(100,NA,NA),nrow=1),ignore_attr=TRUE)
  
  ipsd <- c(0,0,0,1)
  n <- 455
  tmp <- psdCI(c(1,1,0,0),ipsd,n=n)
  expect_equal(tmp,matrix(c(0,NA,NA),nrow=1),ignore_attr=TRUE)
})  


## Validate Results ----
test_that("Does psdAdd() create correct Gabelhouse categories?",{
  #!!!!! df2 has hand-calculated results for psd variable
  suppressMessages(df2$psd2 <- psdAdd(len~species2,data=df2,
                                      thesaurus=thes))
  expect_equal(df2$psd,df2$psd2)
})

test_that("Does psdAdd() properly handle NA in species?",{
  #!!!!! Makes sure NAs are in proper position and by extension correct number
  #===== Just NAs for species only one other species
  testdf <- data.frame(TL=c(400,90,250,130,50),
                       Spp=c("White Crappie",NA,"White Crappie",
                             "White Crappie","White Crappie"))
  suppressMessages(gcat <- psdAdd(TL~Spp,data=testdf,drop.levels=TRUE))
  expect_equal(which(is.na(testdf$TL) | is.na(testdf$Spp)),
                  which(is.na(gcat)))
  
  #===== Just NAs for species only multiple other species
  testdf <- data.frame(TL=c(400,90,250,130,50),
                       Spp=c("White Crappie",NA,"White Crappie",
                             "White Crappie","Black Crappie"))
  suppressMessages(gcat <- psdAdd(TL~Spp,data=testdf,drop.levels=TRUE))
  expect_equal(which(is.na(testdf$TL) | is.na(testdf$Spp)),
                    which(is.na(gcat)))

  #===== Just NAs for species, but with a species w/o Gabelhouse lengths
  testdf <- data.frame(TL=c(400,90,250,NA,50),
                       Spp=c("White Crappie",NA,"badSpp",
                             "White Crappie","Black Crappie"))
  suppressMessages(gcat <- psdAdd(TL~Spp,data=testdf,drop.levels=TRUE))
  expect_equal(which(is.na(testdf$TL) | is.na(testdf$Spp) | testdf$Spp=="badSpp"),
                    which(is.na(gcat)))
  
  #===== NAs for length and species
  testdf <- data.frame(TL=c(400,90,250,NA,50),
                       Spp=c("White Crappie",NA,"White Crappie",
                             "White Crappie","Black Crappie"))
  suppressMessages(gcat <- psdAdd(TL~Spp,data=testdf,drop.levels=TRUE))
  expect_equal(which(is.na(testdf$TL) | is.na(testdf$Spp)),
                    which(is.na(gcat)))
})

test_that("Does psdCalc() compute correct PSD values?",{
  #===== Bluegill and Bass examples
  suppressWarnings(res <- psdCalc(~len,data=subset(df2,species=="Bluegill Sunfish"),
                                  species="Bluegill"))
  expect_equal(res[,"Estimate"],c(55,10,45,45,10),ignore_attr=TRUE)
  suppressWarnings(res <- psdCalc(~len,data=subset(df2,species=="Largemouth Bass"),
                                  species="Largemouth Bass"))
  expect_equal(res[,"Estimate"],c(43,4,57,39,4),ignore_attr=TRUE)
  
  #===== pretend like no species is given (but using bluegill results)
  suppressWarnings(res <- psdCalc(~len,data=subset(df2,species=="Bluegill Sunfish"),
                                  species="Bluegill",
                                  addLens=c("stock"=80,"quality"=150,
                                            "preferred"=200,"memorable"=250,
                                            "trophy"=300)))
  expect_equal(res[,"Estimate"],c(55,10,45,45,10),ignore_attr=TRUE)
})

#test_that("Does psdCalc() work with a tibble?",{
#  #!!!!! Commented out so as not to have to include tibble in package depends
#  tmp1 <- subset(df2,species=="Bluegill Sunfish")
#  tmp2 <- tibble::as_tibble(tmp1)
#  suppressWarnings(res1 <- psdCalc(~len,data=tmp1,species="Bluegill"))
#  suppressWarnings(res2 <- psdCalc(~len,data=tmp2,species="Bluegill"))
#  expect_equal(res,res2)
#})

test_that("Does psdCalc() results match Brenden et al. (2008) results",{
  #===== Setup sample with known PSD values
  brks <- psdVal("Yellow Perch")
  freq <- c(110,75,50,35,20,10)
  tmp <- data.frame(tl=rep(brks,freq)+sample(1:45,300,replace=TRUE),
                    species=factor(rep("Yellow Perch",300)))
  
  #===== Get known PSD values
  psdXYs <- prop.table(freq[-1])*100
  psdXs <- rcumsum(psdXYs)[-1]
  psdXYs <- psdXYs[-length(psdXYs)]

  #----- Get psdCalc results
  suppressWarnings(
    resXY <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="incremental",
                     digits=getOption("digits")))
  suppressWarnings(
    resX <- psdCalc(~tl,data=tmp,species="Yellow Perch",what="traditional",
                    digits=getOption("digits")))

  #----- Are lengths what you would expect
  expect_equal(nrow(resXY),4)
  expect_equal(nrow(resX),4)
  #----- Are values the same
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
  diffs <- round(resX[,"Estimate"]-psdXs,7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
  
  #===== Do things still work if all sub-stock fish are removed
  tmp1 <- droplevels(subset(tmp,tl>=brks["stock"]))
  suppressWarnings(
    resXY <- psdCalc(~tl,data=tmp1,species="Yellow Perch",what="incremental",
                     digits=getOption("digits")))
  suppressWarnings(
    resX <- psdCalc(~tl,data=tmp1,species="Yellow Perch",what="traditional",
                    digits=getOption("digits")))
  expect_equal(nrow(resXY),4)
  expect_equal(nrow(resX),4)
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
  diffs <- round(resX[,"Estimate"]-psdXs,7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
  
  #===== Do things still work if all sub-stock and stock fish are removed  
  psdXYs <- prop.table(freq[-c(1:2)])*100
  psdXs <- rcumsum(psdXYs)[-1]
  psdXYs <- psdXYs[-length(psdXYs)]
  
  tmp1 <- droplevels(subset(tmp,tl>=brks["quality"]))
  suppressWarnings(
    resXY <- psdCalc(~tl,data=tmp1,species="Yellow Perch",what="incremental",
                     digits=getOption("digits")))
  suppressWarnings(
    resX <- psdCalc(~tl,data=tmp1,species="Yellow Perch",what="traditional",
                    digits=getOption("digits")))
  expect_equal(nrow(resXY),3)  # no S-Q row
  expect_equal(nrow(resX),4)   # all should be there
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
  diffs <- round(resX[-1,"Estimate"]-psdXs,7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
  
  #===== Do things still work if all trophy fish are removed  
  psdXYs <- prop.table(freq[-c(1,length(freq))])*100
  psdXs <- rcumsum(psdXYs)[-1]
  
  tmp1 <- droplevels(subset(tmp,tl<brks["trophy"]))
  suppressWarnings(
    resXY <- psdCalc(~tl,data=tmp1,species="Yellow Perch",what="incremental",
                     digits=getOption("digits")))
  suppressWarnings(
    resX <- psdCalc(~tl,data=tmp1,species="Yellow Perch",what="traditional",
                    digits=getOption("digits")))
  expect_equal(nrow(resXY),4)  # no T- row
  expect_equal(nrow(resX),3)   # no T row
  diffs <- round(resXY[,"Estimate"]-psdXYs,7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
  diffs <- round(resX[,"Estimate"]-psdXs,7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
})

test_that("Does manual calculation after psdAdd() equal psdCalc() results?",{
  #===== get psdCalc results for LMB and BG .. ultimately compare to df2 results
  suppressWarnings(
    psdBG <- psdCalc(~len,data=droplevels(subset(df2,species=="Bluegill Sunfish")),
                     species="Bluegill",digits=getOption("digits")))
  suppressWarnings(
    psdLMB <- psdCalc(~len,data=droplevels(subset(df2,species=="Largemouth Bass")),
                      species="Largemouth Bass",digits=getOption("digits")))
  
  #===== apply psdAdd to df2
  suppressMessages(df2$PSDcat <- psdAdd(len~species2,df2,thesaurus=thes))
  #-----  isolate two species, remove substock and other fish
  tmp <- droplevels(subset(df2,species %in% c("Bluegill Sunfish","Largemouth Bass")))
  tmp <- droplevels(subset(tmp,PSDcat!="substock"))
  #-----  calculate PSDs as prop.table
  res <- prop.table(xtabs(~species+PSDcat,data=tmp),margin=1)*100
  
  #===== do PSD X-Y results match for two species
  diffs <- round(res["Bluegill Sunfish",1:3]-psdBG[3:5,"Estimate"],7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
  diffs <- round(res["Largemouth Bass",1:3]-psdLMB[3:5,"Estimate"],7)
  expect_equal(diffs,rep(0,length(diffs)),ignore_attr=TRUE)
})

test_that("Does psdCI results match Brenden et al. (2008) results",{
  #===== proportions table from Brenden et al. (2008)
  ipsd <- c(0.130,0.491,0.253,0.123)
  n <- 445
  
  #===== results from Brenden et al. (2008) ... Table 2
  bpsd <- cbind(c(13,49,25,12,87,38),
                c(9,42,20,8,82,31),
                c(17,56,31,17,91,44))
  colnames(bpsd) <- c("Estimate","95% LCI","95% UCI")
  rownames(bpsd) <- c("PSD S-Q","PSD Q-P","PSD P-M","PSD M-T","PSD","PSD-P")
  
  #===== psdCI calculations
  imat <- matrix(c(1,0,0,0,
                   0,1,0,0,
                   0,0,1,0,
                   0,0,0,1,
                   0,1,1,1,
                   0,0,1,1),nrow=6,byrow=TRUE)
  rownames(imat) <- rownames(bpsd)
  mcis <- t(apply(imat,MARGIN=1,FUN=psdCI,ptbl=ipsd,n=n,method="multinomial"))
  colnames(mcis) <- c("Estimate","95% LCI","95% UCI")
  
  #===== Brenden's results were rounded, thus all should be within 0.5
  diff <- mcis-bpsd
  expect_true(all(abs(diff)<=0.5))
})
