context("capHistXXX() OUTPUT")
source("EXS_capHist.R")

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
  expect_equal(names(ch),c("caphist","sum","methodB.top","methodB.bot","m.array"))
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
  expect_equal(names(ex1.M2I2),c("id",paste0("event",seq_along(unique(ex1$yr)))))
  # RMARK format
  expect_is(ex1.R2I,"data.frame")
  expect_equal(nrow(ex1.R2I),length(unique(ex1$fish)))
  expect_equal(ncol(ex1.R2I),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.R2I[,2:5]),nrow(ex1))
  expect_equal(names(ex1.R2I),c("fish",paste0("event",seq_along(unique(ex1$yr)))))
  
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
  expect_equal(names(ex1.M2F),c(paste0("event",seq_along(unique(ex1$yr))),"freq"))
  # RMark format
  expect_is(ex1.R2F,"data.frame")
  expect_equal(ncol(ex1.R2F),length(unique(ex1$yr))+1)
  expect_equal(sum(ex1.R2F$freq),length(unique(ex1$fish)))
  expect_equal(names(ex1.R2F),c(paste0("event",seq_along(unique(ex1$yr))),"freq"))
  
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
  