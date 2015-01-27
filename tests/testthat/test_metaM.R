context("metaM() function Tests")

test_that("metaM() error and warning messages",{
  ## bad method
  expect_error(metaM("Ogle"))
  
  ## bad Hgroup for Hoenig methods
  expect_error(metaM("HoenigO",Hgroup="Ogle",tmax=8))
  expect_error(metaM("HoenigO2",Hgroup="Ogle",tmax=8))
  
  ## missing parameters
  # default tmax method
  expect_error(metaM())
  # Pauly L method
  expect_error(metaM("PaulyL",Linf=200,K=0.3))
  expect_error(metaM("PaulyL",Linf=200,T=10))
  expect_error(metaM("PaulyL",K=0.3,T=10))
  expect_error(metaM("PaulyL",K=0.3,a=3))
  # Hoenig method
  expect_error(metaM("HoenigO"))
  expect_error(metaM("HoenigO",Linf=200))
  
  ## Bad parameter values
  expect_warning(metaM("PaulyL",Linf=200,K=0.3,T=-3))
  expect_warning(metaM("PaulyL",Linf=200,K=0.3,T=33))
  expect_error(metaM("PaulyL",Linf=200,K=-0.3,T=13))
  expect_error(metaM("PaulyL",Linf=0,K=0.3,T=13))
  expect_warning(metaM("PaulyL",Linf=2000,K=0.3,T=13))
  expect_error(metaM("HoenigO",tmax=0))
  expect_warning(metaM("HoenigO",tmax=110))
  expect_error(metaM("K1",K=0))
  expect_warning(metaM("K1",K=5))
})

test_that("metaM() matches Chesapeke Bay Anchovy results from Kenchington (2014)",{
  ## Life History Parameters from Kenchington (2014) Table 2
  lh <- c(Tmax=3,Linf=12.93,Winf=20.79,K=0.23,t0=-1.23,a=0.005477,
           b=3.22,lm=4,tm=0.83,T=17,ne=863,tc=0,l1=3,l2=10)
  
  ## Pauly's Length equation (Kenchington's Pauly's I by Length)
  tmp1 <- metaM("PaulyL",Linf=lh[["Linf"]],K=lh[["K"]],T=lh[["T"]])
  expect_equivalent(round(tmp1$M,2),0.69)
  
  ## Pauly's Weight equation (Kenchington's Pauly's I by Weight)
  tmp1 <- metaM("PaulyW",Winf=lh[["Winf"]],K=lh[["K"]],T=lh[["T"]])
  expect_equivalent(round(tmp1$M,2),0.66)
  
  ## Hoenig's combined equation (Kenchington's Hoenig's combined)
  tmp1 <- metaM("HoenigO",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),1.43)
  
  ## Hoenig's fish equation (Kenchington's Hoenig's)
  tmp1 <- metaM("HoenigO",Hgroup="fish",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),1.42)

  ## Hoenig's combined equation (Kenchington's Hoenig's combined GM)
  tmp1 <- metaM("HoenigO2",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),1.69)
  
  ## Hoenig's fish equation (Kenchington's Hoenig's GM)
  tmp1 <- metaM("HoenigO2",Hgroup="fish",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),1.83)
  
  ## JensenK1 equation (Jensen second in Kenchington)
  ##   only difference is how R rounds a 5
  tmp1 <- metaM("JensenK1",K=lh[["K"]])
  expect_equivalent(round(tmp1$M+0.001,2),0.35)
  
  ## Gislason equation (Gislason first in Kenchington)
  tmp1 <- metaM("Gislason",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l1"]])
  expect_equivalent(round(tmp1$M,2),2.71)
  tmp1 <- metaM("Gislason",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l2"]])
  expect_equivalent(round(tmp1$M,2),0.39)
  
  ## AlversonCarney equation
  tmp1 <- metaM("Alverson",K=lh[["K"]],tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),2.30)
  
  ## Charnov equation (Gislason second in Kenchington)
  tmp1 <- metaM("Charnov",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l1"]])
  expect_equivalent(round(tmp1$M,2),2.06)
  tmp1 <- metaM("Charnov",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l2"]])
  expect_equivalent(round(tmp1$M,2),0.34)
  
  ## ZhangMegrey equation
  tmp1 <- metaM("Zhang",ZMgroup="demersal",K=lh[["K"]],tmax=lh[["Tmax"]],t0=lh[["t0"]],b=lh[["b"]])
  expect_equivalent(round(tmp1$M,2),1.17)
  
  ## RikhterEfanov1 equation (Kenchington's second)
  tmp1 <- metaM("RikhterEfanov1",t50=lh[["tm"]])
  expect_equivalent(round(tmp1$M,2),1.58)
  
  ## RikhterEfanov2 equation (Kenchington's first)
  tmp1 <- metaM("RikhterEfanov2",K=lh[["K"]],t0=lh[["t0"]],b=lh[["b"]],t50=lh[["tm"]])
  expect_equivalent(round(tmp1$M,2),1.22)
})

test_that("metaM() matches Norwegian Fjord Lanternfish results from Kenchington (2014)",{
  ## Life History Parameters from Kenchington (2014) Table 2
  lh <- c(Tmax=8,Linf=8.306,Winf=8.68,K=0.204,t0=-0.64,a=0.00873,
          b=3.26,lm=4.75,tm=2,T=8,ne=309,tc=4,l1=2,l2=8)
  
  ## Pauly's Length equation (Kenchington's Pauly's I by Length)
  tmp1 <- metaM("PaulyL",Linf=lh[["Linf"]],K=lh[["K"]],T=lh[["T"]])
  expect_equivalent(round(tmp1$M,2),0.51)
  
  ## Pauly's Weight equation (Kenchington's Pauly's I by Weight)
  tmp1 <- metaM("PaulyW",Winf=lh[["Winf"]],K=lh[["K"]],T=lh[["T"]])
  expect_equivalent(round(tmp1$M,2),0.46)
  
  ## Hoenig's combined equation (Kenchington's Hoenig's combined)
  tmp1 <- metaM("HoenigO",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),0.55)
  
  ## Hoenig's fish equation (Kenchington's Hoenig's)
  tmp1 <- metaM("HoenigO",Hgroup="fish",tmax=lh[["Tmax"]])
#  expect_equivalent(round(tmp1$M,2),0.61)
  
  ## Hoenig's combined equation (Kenchington's Hoenig's combined GM)
  tmp1 <- metaM("HoenigO2",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),0.58)
  
  ## Hoenig's fish equation (Kenchington's Hoenig's GM)
  tmp1 <- metaM("HoenigO2",Hgroup="fish",tmax=lh[["Tmax"]])
#  expect_equivalent(round(tmp1$M,2),0.64)
  
  ## JensenK1 equation (Jensen second in Kenchington)
  tmp1 <- metaM("JensenK1",K=lh[["K"]])
  expect_equivalent(round(tmp1$M,2),0.31)
  
  ## Gislason equation (Gislason first in Kenchington)
  tmp1 <- metaM("Gislason",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l1"]])
  expect_equivalent(round(tmp1$M,2),2.44)
  tmp1 <- metaM("Gislason",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l2"]])
  expect_equivalent(round(tmp1$M,2),0.26)
  
  ## AlversonCarney equation
  tmp1 <- metaM("Alverson",K=lh[["K"]],tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),0.71)
  
  ## Charnov equation (Gislason second in Kenchington)
  tmp1 <- metaM("Charnov",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l1"]])
  expect_equivalent(round(tmp1$M,2),1.73)
  tmp1 <- metaM("Charnov",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l2"]])
  expect_equivalent(round(tmp1$M,2),0.22)
  
  ## ZhangMegrey equation
  tmp1 <- metaM("Zhang",ZMgroup="demersal",K=lh[["K"]],tmax=lh[["Tmax"]],t0=lh[["t0"]],b=lh[["b"]])
  expect_equivalent(round(tmp1$M,2),0.77)
  
  ## RikhterEfanov1 equation (Kenchington's second)
  tmp1 <- metaM("RikhterEfanov1",t50=lh[["tm"]])
  expect_equivalent(round(tmp1$M,2),0.77)
  
  ## RikhterEfanov2 equation (Kenchington's first)
  tmp1 <- metaM("RikhterEfanov2",K=lh[["K"]],t0=lh[["t0"]],b=lh[["b"]],t50=lh[["tm"]])
  expect_equivalent(round(tmp1$M,2),0.93)
})


test_that("metaM() matches Rio Formosa Seahorse results from Kenchington (2014)",{
  ## Life History Parameters from Kenchington (2014) Table 2
  lh <- c(Tmax=5.5,Linf=19.76,Winf=17.3,K=0.571,t0=-0.91,a=0.000984,
          b=3.276,lm=10.94,tm=0.49,T=19,ne=234,tc=0.79,l1=10,l2=19)
  
  ## Pauly's Length equation (Kenchington's Pauly's I by Length)
  tmp1 <- metaM("PaulyL",Linf=lh[["Linf"]],K=lh[["K"]],T=lh[["T"]])
  expect_equivalent(round(tmp1$M,2),1.16)
  
  ## Pauly's Weight equation (Kenchington's Pauly's I by Weight)
  tmp1 <- metaM("PaulyW",Winf=lh[["Winf"]],K=lh[["K"]],T=lh[["T"]])
  expect_equivalent(round(tmp1$M,2),1.30)
  
  ## Hoenig's combined equation (Kenchington's Hoenig's combined)
  tmp1 <- metaM("HoenigO",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),0.79)
  
  ## Hoenig's fish equation (Kenchington's Hoenig's)
  tmp1 <- metaM("HoenigO",Hgroup="fish",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),0.77)
  
  ## Hoenig's combined equation (Kenchington's Hoenig's combined GM)
  tmp1 <- metaM("HoenigO2",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),0.88)
  
  ## Hoenig's fish equation (Kenchington's Hoenig's GM)
  tmp1 <- metaM("HoenigO2",Hgroup="fish",tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),0.87)
  
  ## JensenK1 equation (Jensen second in Kenchington)
  tmp1 <- metaM("JensenK1",K=lh[["K"]])
  expect_equivalent(round(tmp1$M,2),0.86)
  
  ## Gislason equation (Gislason first in Kenchington)
  tmp1 <- metaM("Gislason",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l1"]])
  expect_equivalent(round(tmp1$M,2),1.78)
  tmp1 <- metaM("Gislason",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l2"]])
  expect_equivalent(round(tmp1$M,2),0.63)
  
  ## AlversonCarney equation
  tmp1 <- metaM("Alverson",K=lh[["K"]],tmax=lh[["Tmax"]])
  expect_equivalent(round(tmp1$M,2),0.75)
  
  ## Charnov equation (Gislason second in Kenchington)
  tmp1 <- metaM("Charnov",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l1"]])
  expect_equivalent(round(tmp1$M,2),1.59)
  tmp1 <- metaM("Charnov",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l2"]])
  expect_equivalent(round(tmp1$M,2),0.61)
  
  ## ZhangMegrey equation
  tmp1 <- metaM("Zhang",ZMgroup="pelagic",K=lh[["K"]],tmax=lh[["Tmax"]],t0=lh[["t0"]],b=lh[["b"]])
  expect_equivalent(round(tmp1$M,2),0.33)
  
  ## RikhterEfanov1 equation (Kenchington's second)
  tmp1 <- metaM("RikhterEfanov1",t50=lh[["tm"]])
  expect_equivalent(round(tmp1$M,2),2.39)
  
  ## RikhterEfanov2 equation (Kenchington's first)
  tmp1 <- metaM("RikhterEfanov2",K=lh[["K"]],t0=lh[["t0"]],b=lh[["b"]],t50=lh[["tm"]])
  expect_equivalent(round(tmp1$M,2),1.53)
})

test_that("metaM() matches M.empirical() from fishmethods for Rio Formosa Seahorse",{
  ## Life History Parameters from Kenchington (2014) Table 2
  lh <- c(Tmax=5.5,Linf=19.76,Winf=17.3,K=0.571,t0=-0.91,a=0.000984,
          b=3.276,lm=10.94,tm=0.49,T=19,ne=234,tc=0.79,l1=10,l2=19)
  
  if (require(fishmethods)) {
    ## Pauly's Length equation
    tmp1 <- metaM("PaulyL",Linf=lh[["Linf"]],K=lh[["K"]],T=lh[["T"]])
    tmp2 <- M.empirical(Linf=lh[["Linf"]],Kl=lh[["K"]],T=lh[["T"]],method=1)
    expect_equivalent(round(tmp1$M,3),round(tmp2[,"M"],3))
    
    ## Pauly's Weight equation
    tmp1 <- metaM("PaulyW",Winf=lh[["Winf"]],K=lh[["K"]],T=lh[["T"]])
    tmp2 <- M.empirical(Winf=lh[["Winf"]],Kw=lh[["K"]],T=lh[["T"]],method=2)
    expect_equivalent(round(tmp1$M,3),round(tmp2[,"M"],3))
    
    ## Hoenig's combined equation
    tmp1 <- metaM("HoenigO",tmax=lh[["Tmax"]])
    tmp2 <- M.empirical(tmax=lh[["Tmax"]],method=3)
    expect_equivalent(round(tmp1$M,3),round(tmp2[1,"M"],3))
    ## Hoenig's fish equation
    tmp1 <- metaM("HoenigO",Hgroup="fish",tmax=lh[["Tmax"]])
    expect_equivalent(round(tmp1$M,3),round(tmp2[2,"M"],3))    

    ## Alverson-Carney equation
    tmp1 <- metaM("Alverson",K=lh[["K"]],tmax=lh[["Tmax"]])
    tmp2 <- M.empirical(Kl=lh[["K"]],tmax=lh[["Tmax"]],method=4)
    expect_equivalent(round(tmp1$M,3),round(tmp2[,"M"],3))
    
    ## Gislason equation
    tmp1 <- metaM("Gislason",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l1"]])
    tmp2 <- M.empirical(Kl=lh[["K"]],Linf=lh[["Linf"]],Bl=lh[["l1"]],method=9)
    expect_equivalent(round(tmp1$M,3),round(tmp2[,"M"],3))
    tmp1 <- metaM("Gislason",K=lh[["K"]],Linf=lh[["Linf"]],L=lh[["l2"]])
    tmp2 <- M.empirical(Kl=lh[["K"]],Linf=lh[["Linf"]],Bl=lh[["l2"]],method=9)
    expect_equivalent(round(tmp1$M,3),round(tmp2[,"M"],3))  
  }
})