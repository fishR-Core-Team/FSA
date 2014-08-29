context("removal Messages")

test_that("removal errors and warnings",{
  ## wrong type
  expect_that(removal(c(346,184,49),method="Derek"),throws_error())
  ## wrong type for CS.se
  expect_that(removal(c(346,184,49),method="CarleStrub",CS.se="Derek"),throws_error())
  ## alpha and beta are not positive
  expect_that(removal(c(346,184,49),method="CarleStrub",alpha=1,beta=0),throws_error())
  expect_that(removal(c(346,184,49),method="CarleStrub",alpha=-1,beta=1),throws_error())
  expect_that(removal(c(346,184,49),method="CarleStrub",alpha=-1,beta=0),throws_error())
  ## Catch not in a vector
  expect_that(removal(matrix(c(346,184,49,12),nrow=2)),throws_error())
  ## only one catch
  expect_that(removal(346),throws_error())
  ## Try using Moran or Schnute method with not three catches
  expect_that(removal(c(346,184),method="Moran"),throws_error())
  expect_that(removal(c(346,184),method="Schnute"),throws_error())
  ## Try using 3-pass method with not three catches
  expect_that(removal(c(346,184),method="Seber3"),throws_error())
  expect_that(removal(c(346,184,49,12),method="Seber3"),throws_error())
  ## Try using 2-pass method with not >2 catches
  expect_that(removal(c(346,184,49),method="Seber2"),throws_error())
  expect_that(removal(c(346,184,49),method="RobsonRegier2"),throws_error())
  ## Errors in 2- and 3-pass methods if last catch is greater than first catch
  expect_that(removal(c(184,346),method="Seber2"),gives_warning())
  expect_that(removal(c(184,346),method="RobsonRegier2"),gives_warning())
  expect_that(removal(c(49,184,346),method="Seber3"),gives_warning())
  ## wrong parm in summary and confint
  tmp <- removal(c(346,184,49))
  expect_that(summary(tmp,parm="Derek"),throws_error())
  expect_that(confint(tmp,parm="Derek"),throws_error())
  ## Bad data leads to failure of Zippin (from Carle-Strub (1978) example 2)
  expect_that(removal(c(5,7,8),method="Zippin"),gives_warning())
  ## Chose "p1" summary for other than Schnute method
  tmp <- removal(c(45,11,18,8),method="Zippin")
  expect_that(summary(tmp,parm="p1"),throws_error())
  expect_that(summary(tmp,parm=c("p","p1")),gives_warning())
  ## Chose only "p" CI for Moran or Schnute method
  tmp <- removal(c(45,11,18,8),method="Schnute")
  expect_that(confint(tmp,parm="p"),throws_error())
  ## Chose bad value for Tmult
  expect_that(removal(c(45,11,18,8),method="Moran",Tmult=0.9),throws_error())  
  expect_that(removal(c(45,11,18,8),method="Moran",Tmult=1.2),gives_warning())  
  ## NAs in catch vector
  expect_that(removal(c(45,11,NA,8)),gives_warning()) 
})  
