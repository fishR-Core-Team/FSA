context("metaM() MESSAGES")

test_that("Mmethods() messages",{
  expect_error(Mmethods("Ogle"),"should be one of")
})

test_that("metaM() messages",{
  ## bad method
  expect_error(metaM("Ogle"),"should be one of")
  
  ## missing parameters
  # default tmax method
  expect_error(metaM("tmax"),"must be given to")
  # Pauly L method
  expect_error(metaM("PaulyL",Linf=200,K=0.3),"must be given to")
  expect_error(metaM("PaulyL",Linf=200,T=10),"must be given to")
  expect_error(metaM("PaulyL",K=0.3,T=10),"must be given to")
  expect_error(metaM("PaulyL",K=0.3,a=3),"unused argument")
  # Hoenig method
  expect_error(metaM("HoenigO"),"must be given to")
  expect_error(metaM("HoenigO",Linf=200),"must be given to")
  
  ## Bad parameter values
  expect_warning(metaM("PaulyL",Linf=200,K=0.3,T=-3),"seems unreasonable")
  expect_warning(metaM("PaulyL",Linf=200,K=0.3,T=33),"seems unreasonable")
  expect_error(metaM("PaulyL",Linf=200,K=-0.3,T=13),"'K' must be positive")
  expect_error(metaM("PaulyL",Linf=0,K=0.3,T=13),"'Linf' must be positive")
  expect_warning(metaM("PaulyL",Linf=2000,K=0.3,T=13),"'Linf' value seems unreasonable")
  expect_error(metaM("HoenigO",tmax=0),"'tmax' must be positive")
  expect_warning(metaM("HoenigO",tmax=110),"'tmax' value seems unreasonable")
  expect_error(metaM("K1",K=0),"'K' must be positive")
  expect_warning(metaM("K1",K=5),"'K' value seems unreasonable")
})
