context("Von Bertalanffy Messages")

test_that("vbFuns errors and warnings",{
  ## wrong types
  expect_that(vbFuns(type="Derek"),throws_error())
})
  
test_that("vbStarts errors and warnings",{
  ## Get some data for the following attempts
  library(fishmethods)
  data(Kimura)
  ## wrong types
  expect_that(vbStarts(length~age,data=Kimura,type="Derek"),throws_error())
  expect_that(vbStarts(length~age,data=Kimura,type="Francis",methEV="Derek"),throws_error())
  expect_that(vbStarts(length~age,data=Kimura,type="Schnute",methEV="Derek"),throws_error())
  expect_that(vbStarts(length~age,data=Kimura,type="typical",meth0="Derek"),throws_error())
  expect_that(vbStarts(length~age,data=Kimura,type="original",meth0="Derek"),throws_error())
  ## Two variables on LHS
  expect_that(vbStarts(length+age~age,data=Kimura,type="typical"),throws_error())
  ## Two variables on RHS
  expect_that(vbStarts(length~age+sex,data=Kimura,type="typical"),throws_error())
  ## LHS is a factor
  expect_that(vbStarts(sex~age,data=Kimura,type="typical"),throws_error())
  ## RHS is a factor
  expect_that(vbStarts(length~sex,data=Kimura,type="typical"),throws_error())
  ## not two ages2use given
  expect_that(vbStarts(length~age,data=Kimura,type="Francis",ages2use=2),throws_error())
  expect_that(vbStarts(length~age,data=Kimura,type="Francis",ages2use=c(2,5,10)),throws_error())
  expect_that(vbStarts(length~age,data=Kimura,type="Schnute",ages2use=2),throws_error())
  expect_that(vbStarts(length~age,data=Kimura,type="Schnute",ages2use=c(2,5,10)),throws_error())
  ## ages2use in wrong order
  expect_that(vbStarts(length~age,data=Kimura,type="Francis",ages2use=c(10,2)),gives_warning())
  expect_that(vbStarts(length~age,data=Kimura,type="Schnute",ages2use=c(10,2)),gives_warning())
  ## gives warning about a poor estimate for K and Linf
  library(FSAdata)
  data(SpottedSucker1)
  sv <- list(Linf=max(SpottedSucker1$tl),K=0.3,t0=0)
  expect_that(vbStarts(tl~age,data=SpottedSucker1,type="typical"),gives_warning())
})

test_that("growthModelSim errors and warnings",{
  ## Get some data for the following attempts
  library(fishmethods)
  data(Kimura)
  ## Two variables on LHS
  expect_that(growthModelSim(length+age~age,data=Kimura),throws_error())
  ## Two variables on RHS
  expect_that(growthModelSim(length~age+sex,data=Kimura),throws_error())
  ## LHS is a factor
  expect_that(growthModelSim(sex~age,data=Kimura),throws_error())
  ## RHS is a factor
  expect_that(growthModelSim(length~sex,data=Kimura),throws_error())
})

test_that("walfordPlot and chapmanPlot errors and warnings",{
  ## Get some data for the following attempts
  library(fishmethods)
  data(Kimura)
  ## Two variables on LHS
  expect_that(walfordPlot(length+age~age,data=Kimura),throws_error())
  expect_that(chapmanPlot(length+age~age,data=Kimura),throws_error())
  ## Two variables on RHS
  expect_that(walfordPlot(length~age+sex,data=Kimura),throws_error())
  expect_that(chapmanPlot(length~age+sex,data=Kimura),throws_error())
  ## LHS is a factor
  expect_that(walfordPlot(sex~age,data=Kimura),throws_error())
  expect_that(chapmanPlot(sex~age,data=Kimura),throws_error())
  ## RHS is a factor
  expect_that(walfordPlot(length~sex,data=Kimura),throws_error())
  expect_that(chapmanPlot(length~sex,data=Kimura),throws_error())
})
