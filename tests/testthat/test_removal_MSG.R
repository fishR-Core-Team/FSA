context("removal() MESSAGES")

test_that("removal() messages",{
  ## wrong type
  expect_error(removal(c(346,184,49),method="Derek"),"should be one of")
  ## wrong type for CS.se
  expect_error(removal(c(346,184,49),method="CarleStrub",CS.se="Derek"),"should be one of")
  ## alpha and beta are not positive
  expect_error(removal(c(346,184,49),method="CarleStrub",alpha=1,beta=0),"must be positive")
  expect_error(removal(c(346,184,49),method="CarleStrub",alpha=-1,beta=1),"must be positive")
  expect_error(removal(c(346,184,49),method="CarleStrub",alpha=-1,beta=0),"must be positive")
  ## Catch not in a vector
  expect_error(removal(matrix(c(346,184,49,12),nrow=2)),"must be a vector")
  expect_error(removal(data.frame(c(346,184),c(49,12))),"must be a vector")
  ## only one catch
  expect_error(removal(346),"with one catch value")
  ## Try using Moran or Schnute method with not three catches
  expect_error(removal(c(346,184),method="Moran"),"at least three samples")
  expect_error(removal(c(346,184),method="Schnute"),"at least three samples")
  ## Try using 3-pass method with not three catches
  expect_error(removal(c(346,184),method="Seber3"),"with three samples")
  expect_error(removal(c(346,184,49,12),method="Seber3"),"with three samples")
  ## Try using 2-pass method with not >2 catches
  expect_error(removal(c(346,184,49),method="Seber2"),"with two samples")
  expect_error(removal(c(346,184,49),method="RobsonRegier2"),"with two samples")
  ## Errors in 2- and 3-pass methods if last catch is greater than first catch
  expect_warning(removal(c(184,346),method="Seber2"),"results in model failure")
  expect_warning(removal(c(184,346),method="RobsonRegier2"),"results in model failure")
  expect_warning(removal(c(49,184,346),method="Seber3"),"results in model failure")
  ## wrong parm in summary and confint
  tmp <- removal(c(346,184,49))
  expect_error(summary(tmp,parm="Derek"),"should be one of")
  expect_error(confint(tmp,parm="Derek"),"should be one of")
  expect_error(removal(c(346,184,49),conf.level=0),"must be between 0 and 1")
  expect_error(removal(c(346,184,49),conf.level=1),"must be between 0 and 1")
  
  ## Bad data leads to failure of Zippin (from Carle-Strub (1978) example 2)
  expect_warning(removal(c(5,7,8),method="Zippin"),"Zippin model failure")
  ## Chose "p1" summary for other than Schnute method
  tmp <- removal(c(45,11,18,8),method="Zippin")
  expect_error(summary(tmp,parm="p1"),"parameter not relevant")
  expect_warning(summary(tmp,parm=c("p","p1")),"parameter not relevant")
  ## Chose only "p" CI for Moran or Schnute method
  tmp <- removal(c(45,11,18,8),method="Schnute")
  expect_error(confint(tmp,parm="p"),"cannot be computed with Schnute")
  ## Chose bad value for Tmult
  expect_error(removal(c(45,11,18,8),method="Moran",Tmult=0.9),"greater than 1")  
  expect_warning(removal(c(45,11,18,8),method="Moran",Tmult=1.2),"try increasing")  
  ## NAs in catch vector
  expect_warning(removal(c(45,11,NA,8)),"'NA's removed from") 
})  

test_that("removal() verbose= messages",{
  expect_message(summary(removal(c(38,26,12)),verbose=TRUE),"Carle & Strub")
  expect_message(summary(removal(c(38,26,12),method="Moran"),verbose=TRUE),"Moran")
  expect_message(summary(removal(c(38,26,12),method="Zippin"),verbose=TRUE),"Zippin")
  expect_message(summary(removal(c(38,26,12),method="Schnute"),verbose=TRUE),"Schnute")
  expect_message(summary(removal(c(38,26,12),method="Seber3"),verbose=TRUE),"Seber")
  expect_message(summary(removal(c(38,26),method="Seber2"),verbose=TRUE),"Seber")
  expect_message(summary(removal(c(38,26),method="RobsonRegier2"),verbose=TRUE),"Robson & Regier")
})   
