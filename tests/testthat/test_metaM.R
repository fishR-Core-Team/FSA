context("metaM() function Tests")

test_that("Mmethods() error and warning messages",{
  expect_error(Mmethods("Ogle"))
})

test_that("metaM() error and warning messages",{
  ## bad method
  expect_error(metaM("Ogle"))
  
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

test_that("metaM() matches results from Kenchington (2014)",{  
  ## Chesapeke Bay Anchovy
  ##   matches except for the way R rounds 5s in JensenK1 method
  meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","HoenigO2","HoenigO2F","JensenK1","Gislason","AlversonCarney","Charnov","ZhangMegreyD","RikhterEfanov1","RikhterEfanov2")
  tmp <- apply(matrix(meths),1,metaM,tmax=3,Linf=12.93,Winf=20.79,K=0.23,
               t0=-1.23,a=0.005477,b=3.22,t50=0.83,T=17,ne=863,L=3)
  tmp <- data.frame(meths,M=round(tmp,2),expM=c(0.69,0.66,1.43,1.42,1.69,1.83,0.35,2.71,2.30,2.06,1.17,1.58,1.22))
  expect_equivalent(tmp$M[-7],tmp$expM[-7])
  
  ## Norwegian Fjord Lanternfish
  ##   matches except for the HoenigOF and HoenigO2F, which Kenchington acknowledged as erroneous
  meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","HoenigO2","HoenigO2F","JensenK1","Gislason","AlversonCarney","Charnov","ZhangMegreyD","RikhterEfanov1","RikhterEfanov2")
  tmp <- apply(matrix(meths),1,metaM,tmax=8,Linf=8.306,Winf=8.68,K=0.204,
               t0=-0.64,a=0.00873,b=3.26,t50=2,T=8,L=2)
  tmp <- data.frame(meths,M=round(tmp,2),expM=c(0.51,0.46,0.55,0.61,0.58,0.64,0.31,2.44,0.71,1.73,0.77,0.77,0.93))
  expect_equivalent(tmp$M[-c(4,6)],tmp$expM[-c(4,6)])
  
  ## Rio Formosa Seahorse
  ## ALL MATCHES
  meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","HoenigO2","HoenigO2F","JensenK1","Gislason","AlversonCarney","Charnov","ZhangMegreyP","RikhterEfanov1","RikhterEfanov2")
  tmp <- apply(matrix(meths),1,metaM,tmax=5.5,Linf=19.76,Winf=17.3,K=0.571,
               t0=-0.91,a=0.000984,b=3.276,t50=0.49,T=19,L=10)
  tmp <- data.frame(meths,M=round(tmp,2),expM=c(1.16,1.30,0.79,0.77,0.88,0.87,0.86,1.78,0.75,1.59,0.33,2.39,1.53))
  expect_equivalent(tmp$M,tmp$expM)
})

test_that("metaM() matches M.empirical() from fishmethods for Rio Formosa Seahorse",{
  ## ALL MATCHES
  if (require(fishmethods)) {
    meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","AlversonCarney","Gislason")
    Mest1 <- apply(matrix(meths),1,metaM,tmax=5.5,Linf=19.76,Winf=17.3,K=0.571,
                   t0=-0.91,a=0.000984,b=3.276,t50=0.49,T=19,L=10)
    Mest2 <- M.empirical(Linf=19.76,Winf=17.3,Kl=0.571,Kw=0.571,T=19,Bl=10,tmax=5.5,method=c(1:4,9))
    tmp <- data.frame(meths,Mest1=round(Mest1,3),Mest2)
    expect_equivalent(tmp$Mest1,tmp$M)
  }
})