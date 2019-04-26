## Create small data for testing
d <- data.frame(id=1:5,sex=c("m","m","f","m","f"),
                first=c(1,1,1,0,0),
                second=c(0,0,1,1,1),
                third=c(1,0,0,0,1))

## A small example of 'event' format
ex1 <- data.frame(fish=c(17,18,21,17,21,18,19,20),
                  yr=c(1987,1987,1987,1988,1988,1989,1989,1990))
# convert to different formats
ex1.E2I <- capHistConvert(ex1,id="fish",in.type="event")
ex1.E2F <- capHistConvert(ex1,id="fish",in.type="event",
                          out.type="frequency")
ex1.E2M <- capHistConvert(ex1,id="fish",in.type="event",
                          out.type="MARK")
ex1.E2R <- capHistConvert(ex1,id="fish",in.type="event",
                          out.type="RMark")

## convert each of these to other formats
## Individual to ...
ex1.I2E <- capHistConvert(ex1.E2I,in.type="individual",
                          out.type="event",id="fish")
ex1.I2F <- capHistConvert(ex1.E2I,in.type="individual",
                          out.type="frequency",id="fish")
ex1.I2M <- capHistConvert(ex1.E2I,in.type="individual",
                          out.type="MARK",id="fish")
ex1.I2R <- capHistConvert(ex1.E2I,in.type="individual",
                          out.type="RMark",id="fish")
## Frequency to ...
ex1.F2E <- capHistConvert(ex1.E2F,in.type="frequency",
                          out.type="event",freq="freq")
ex1.F2I <- capHistConvert(ex1.E2F,in.type="frequency",
                          out.type="individual",freq="freq")
ex1.F2I2 <- capHistConvert(ex1.E2F,in.type="frequency",
                           out.type="individual",freq="freq",include.id=TRUE)
ex1.F2M <- capHistConvert(ex1.E2F,in.type="frequency",
                          out.type="MARK",freq="freq")
ex1.F2R <- capHistConvert(ex1.E2F,in.type="frequency",
                          out.type="RMark",freq="freq")
ex1.F2R2 <- capHistConvert(ex1.E2F,in.type="frequency",
                           out.type="RMark",freq="freq",include.id=TRUE)
## MARK to ...
ex1.M2E <- capHistConvert(ex1.E2M,in.type="MARK",
                          out.type="event",freq="freq")
ex1.M2F <- capHistConvert(ex1.E2M,in.type="MARK",
                          out.type="frequency",freq="freq")
ex1.M2I <- capHistConvert(ex1.E2M,in.type="MARK",
                          out.type="individual",freq="freq")
ex1.M2I2 <- capHistConvert(ex1.E2M,in.type="MARK",
                           out.type="individual",freq="freq",include.id=TRUE)
ex1.M2R <- capHistConvert(ex1.E2M,in.type="MARK",
                          out.type="RMark",freq="freq")
ex1.M2R2 <- capHistConvert(ex1.E2M,in.type="MARK",
                           out.type="RMark",freq="freq",include.id=TRUE)
## RMARK to ...
ex1.R2E <- capHistConvert(ex1.E2R,in.type="RMark",
                          out.type="event",id="fish")
ex1.R2F <- capHistConvert(ex1.E2R,in.type="RMark",
                          out.type="frequency",id="fish")
ex1.R2I <- capHistConvert(ex1.E2R,in.type="RMark",
                          out.type="individual",id="fish")
ex1.R2M <- capHistConvert(ex1.E2R,in.type="RMark",
                          out.type="MARK",id="fish")


## Test Messages ----
test_that("capHistSum() messages",{
  expect_error(capHistSum(d,cols2use=1,cols2ignore=4),"Cannot use both")
  expect_error(suppressWarnings(capHistSum(d,cols2ignore=1:5),
                                "undefined columns selected"))
})  

test_that("capHistConvert() messages",{
  ## wrong types in in.type or out.type
  expect_error(capHistConvert(ex1,id="fish",in.type="derek"),
               "should be one of")
  expect_error(capHistConvert(ex1,id="fish",out.type="derek"),
               "should be one of")
  expect_error(capHistConvert(ex1,id="fish",in.type="event",out.type="event"),
               "cannot be the same")
  ## bad var.lbls.pre
  # start with a number
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",
                                var.lbls.pre="7event"),
                 "cannot begin with a number")
  # too many (must be just one)
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",
                                var.lbls.pre=LETTERS[1:2]),
                 "contains more than one prefix")
  ## bad var.lbls ... too few (too many is adjusted without warning)
  expect_warning(capHistConvert(ex1.E2R,id="fish",in.type="RMark",
                                var.lbls=LETTERS[1:2]),
                 "Too few labels")
  
  ## Bad (or semi-bad) arguments for specific conversions
  # no "id" variable when converting from 'event' to 'individual' format
  expect_error(capHistConvert(ex1,in.type="event"),
               "No variable with unique fish")
  # no "freq" var when from 'frequency', 'RMark' or 'MARK' to 'individual' formats
  expect_warning(capHistConvert(ex1.E2F,in.type="frequency"),
                 "No 'freq' given")
  # should give a warning as well
  expect_error(suppressWarnings(capHistConvert(ex1.E2R,in.type="RMark")),
               "invalid 'times' argument")
  expect_error(capHistConvert(ex1.E2R,in.type="RMark",id="fish",freq="fish"),
               "Only one of 'id' or 'freq' can be used")
  # should give a warning as well
  expect_error(suppressWarnings(capHistConvert(ex1.E2M,in.type="MARK")),
               "invalid 'times' argument")
  # cols2XXX problems
  expect_error(capHistConvert(ex1,cols2use=1,cols2ignore=4),
               "Cannot use both")
  expect_error(suppressWarnings(capHistConvert(ex1,cols2ignore=1:5)),
               "Some 'cols2ignore' do not exist")
})

## Test Output Types ----
test_that("capHistSum() results",{
  ## Simple two-sample test data
  ch <- capHistSum(d,cols2use=3:4)
  expect_is(ch,"CapHist")
  expect_equal(mode(ch),"list")
  expect_equal(names(ch),c("caphist","sum"))
  expect_is(ch$caphist,"table")
  expect_is(ch$sum,"data.frame")
  
  ## Simple three-sample test data
  ch <- capHistSum(d,cols2use=3:5)
  expect_is(ch,"CapHist")
  expect_equal(mode(ch),"list")
  expect_equal(names(ch),c("caphist","sum","methodB.top",
                           "methodB.bot","m.array"))
  expect_is(ch$caphist,"table")
  expect_is(ch$sum,"data.frame")
  expect_is(ch$methodB.top,"matrix")
  expect_equal(nrow(ch$methodB.top),3)
  expect_equal(ncol(ch$methodB.top),3)
  expect_equal(colnames(ch$methodB.top),paste0("i=",seq_len(3)))
  expect_equal(rownames(ch$methodB.top),paste0("j=",seq_len(3)))
  expect_is(ch$methodB.bot,"matrix")
  expect_equal(nrow(ch$methodB.bot),4)
  expect_equal(ncol(ch$methodB.bot),3)
  expect_equal(colnames(ch$methodB.bot),paste0("i=",seq_len(3)))
  expect_equal(rownames(ch$methodB.bot),c("m","u","n","R"))
  expect_is(ch$m.array,"matrix")
  expect_equal(nrow(ch$m.array),3)
  expect_equal(ncol(ch$m.array),4)
  expect_equal(rownames(ch$m.array),paste0("i=",seq_len(3)))
  expect_equal(colnames(ch$m.array),c("ni","c2","c3","not recapt"))
})  

test_that("capHistConvert() results",{
  ## Individual format created from ...
  # event format
  expect_is(ex1.E2I,"data.frame")
  expect_equal(nrow(ex1.E2I),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.E2I),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.E2I[2:5]),nrow(ex1))
  expect_equal(names(ex1.E2I),c("fish",unique(ex1$yr)))
  expect_is(ex1.E2I$fish,"character")
  # frequency format (without id)
  expect_is(ex1.F2I,"data.frame")
  expect_equal(nrow(ex1.F2I),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.F2I),length(unique(ex1$yr)))
  expect_equal(sum(ex1.F2I),nrow(ex1))
  expect_equal(names(ex1.F2I),as.character(unique(ex1$yr)))
  # frequency format (with id)
  expect_is(ex1.F2I2,"data.frame")
  expect_equal(nrow(ex1.F2I2),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.F2I2),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.F2I2[2:5]),nrow(ex1))
  expect_equal(names(ex1.F2I2),c("id",unique(ex1$yr)))
  # MARK format
  expect_is(ex1.M2I,"data.frame")
  expect_equal(nrow(ex1.M2I),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.M2I),length(unique(ex1$yr)))
  expect_equal(sum(ex1.M2I),nrow(ex1))
  expect_equal(names(ex1.M2I),paste0("event",seq_along(unique(ex1$yr))))
  # MARK format (with id)
  expect_is(ex1.M2I2,"data.frame")
  expect_equal(nrow(ex1.M2I2),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.M2I2),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.M2I2[,2:5]),nrow(ex1))
  expect_equal(names(ex1.M2I2),c("id",paste0("event",
                                             seq_along(unique(ex1$yr)))))
  # RMARK format
  expect_is(ex1.R2I,"data.frame")
  expect_equal(nrow(ex1.R2I),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.R2I),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.R2I[,2:5]),nrow(ex1))
  expect_equal(names(ex1.R2I),c("fish",paste0("event",
                                              seq_along(unique(ex1$yr)))))
  
  ## Frequency created from
  # event format
  expect_is(ex1.E2F,"data.frame")
  expect_equal(ncol(ex1.E2F),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.E2F$freq),length(unique(ex1$fish)))
  expect_equal(names(ex1.E2F),c(unique(ex1$yr),"freq"))
  # Individual format
  expect_is(ex1.I2F,"data.frame")
  expect_equal(ncol(ex1.I2F),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.I2F$freq),length(unique(ex1$fish)))
  expect_equal(names(ex1.I2F),c(unique(ex1$yr),"freq"))
  # Mark format
  expect_is(ex1.M2F,"data.frame")
  expect_equal(ncol(ex1.M2F),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.M2F$freq),length(unique(ex1$fish)))
  expect_equal(names(ex1.M2F),c(paste0("event",seq_along(unique(ex1$yr))),
                                "freq"))
  # RMark format
  expect_is(ex1.R2F,"data.frame")
  expect_equal(ncol(ex1.R2F),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.R2F$freq),length(unique(ex1$fish)))
  expect_equal(names(ex1.R2F),c(paste0("event",seq_along(unique(ex1$yr))),
                                "freq"))
  
  ## MARK created from
  # event format
  expect_is(ex1.E2M,"data.frame")
  expect_equal(ncol(ex1.E2M),2)
  expect_equal(names(ex1.E2M),c("ch","freq"))
  # frequency format
  expect_is(ex1.F2M,"data.frame")
  expect_equal(ncol(ex1.F2M),2)
  expect_equal(names(ex1.F2M),c("ch","freq"))
  # Individual format
  expect_is(ex1.I2M,"data.frame")
  expect_equal(ncol(ex1.I2M),2)
  expect_equal(names(ex1.I2M),c("ch","freq"))
  # RMARK format
  expect_is(ex1.R2M,"data.frame")
  expect_equal(ncol(ex1.R2M),2)
  expect_equal(names(ex1.R2M),c("ch","freq"))
  
  ## RMARK created from
  # event format
  expect_is(ex1.E2R,"data.frame")
  expect_equal(nrow(ex1.E2R),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.E2R),2)
  expect_equal(names(ex1.E2R),c("fish","ch"))
  expect_is(ex1.E2R$fish,"character")
  expect_equal(ex1.E2R$fish,as.character(sort(unique(ex1$fish))))
  # frequency format
  expect_is(ex1.F2R,"data.frame")
  expect_equal(ncol(ex1.F2R),1)
  expect_equal(nrow(ex1.F2R),length(unique(ex1$fish)))
  expect_equal(names(ex1.F2R),"ch")
  # frequency format (with id)
  expect_is(ex1.F2R2,"data.frame")
  expect_equal(ncol(ex1.F2R2),2)
  expect_equal(nrow(ex1.F2R2),length(unique(ex1$fish)))
  expect_equal(names(ex1.F2R2),c("id","ch"))
  # Individual format
  expect_is(ex1.I2R,"data.frame")
  expect_equal(ncol(ex1.I2R),2)
  expect_equal(nrow(ex1.I2R),length(unique(ex1$fish)))
  expect_equal(names(ex1.I2R),c("fish","ch"))
  expect_is(ex1.I2R$fish,"character")
  expect_equal(ex1.I2R$fish,as.character(sort(unique(ex1$fish))))
  # MARK format
  expect_is(ex1.M2R,"data.frame")
  expect_equal(ncol(ex1.M2R),1)
  expect_equal(nrow(ex1.M2R),length(unique(ex1$fish)))
  expect_equal(names(ex1.M2R),"ch")
  # RMARK format (with id)
  expect_is(ex1.M2R2,"data.frame")
  expect_equal(nrow(ex1.M2R2),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.M2R2),2)
  expect_equal(names(ex1.M2R2),c("id","ch"))
})


## Validate Results ----
test_that("capHistSum() results",{
  ## Simple two-sample test data
  ch <- capHistSum(d,cols2use=3:4)
  expect_equivalent(ch$caphist,as.table(c(2,2,1)))
  expect_equivalent(names(ch$caphist),c("01","10","11"))
  expect_equal(ch$sum,data.frame(M=3,n=3,m=1))
  
  ## Simple three-sample test data
  ch <- capHistSum(d,cols2use=3:5)
  expect_equivalent(ch$caphist,as.table(c(1,1,1,1,1)))
  expect_equivalent(names(ch$caphist),c("010","011","100","101","110"))
  tmp <- data.frame(n=c(3,3,2),m=c(0,1,2),R=c(3,3,0),M=c(0,3,5),
                    u=c(3,3,0),v=c(1,3,2),f=c(2,3,0))
  expect_equal(ch$sum,tmp)
  expect_identical(ch$methodB.top[upper.tri(ch$methodB.top)],c(1,1,1))
  expect_identical(ch$methodB.top[!upper.tri(ch$methodB.top)],
                   as.numeric(rep(NA,6)))
  expect_equal(ch$sum,tmp)
  tmp <- matrix(c(0,3,3,3,1,2,3,3,2,0,2,0),nrow=4)
  expect_equivalent(ch$methodB.bot,tmp)
  tmp <- matrix(c(3,3,2,1,NA,NA,1,1,NA,1,2,2),nrow=3)
  expect_equivalent(ch$m.array,tmp)
})  

