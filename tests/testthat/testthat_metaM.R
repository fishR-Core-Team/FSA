## Test Messages ----
test_that("Mmethods() messages",{
  expect_error(Mmethods("Ogle"),
               "should be one of")
})

test_that("metaM() messages",{
  ## bad method
  expect_error(metaM("Ogle"),
               "should be one of")
  
  ## missing parameters
  # default tmax method
  expect_error(metaM("tmax"),
               "must be given to")
  # Pauly L method
  expect_error(metaM("PaulyL",Linf=200,K=0.3),
               "must be given to")
  expect_error(metaM("PaulyL",Linf=200,T=10),
               "must be given to")
  expect_error(metaM("PaulyL",K=0.3,T=10),
               "must be given to")
  expect_error(metaM("PaulyL",K=0.3,a=3),
               "unused argument")
  # Hoenig method
  expect_error(metaM("HoenigO"),
               "must be given to")
  expect_error(metaM("HoenigO",Linf=200),
               "must be given to")
  
  ## Bad parameter values
  expect_warning(metaM("PaulyL",Linf=200,K=0.3,T=-3),
                 "seems unreasonable")
  expect_warning(metaM("PaulyL",Linf=200,K=0.3,T=33),
                 "seems unreasonable")
  expect_error(metaM("PaulyL",Linf=200,K=-0.3,T=13),
               "'K' must be positive")
  expect_error(metaM("PaulyL",Linf=0,K=0.3,T=13),
               "'Linf' must be positive")
  expect_warning(metaM("PaulyL",Linf=2000,K=0.3,T=13),
                 "'Linf' value seems unreasonable")
  expect_error(metaM("HoenigO",tmax=0),
               "'tmax' must be positive")
  expect_warning(metaM("HoenigO",tmax=110),
                 "'tmax' value seems unreasonable")
  expect_error(metaM("K1",K=0),
               "'K' must be positive")
  expect_warning(metaM("K1",K=5),
                 "'K' value seems unreasonable")
})


## Test Output Types ----
test_that("metaM() output",{  
  meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","HoenigO2","HoenigO2F",
             "JensenK1","Gislason","AlversonCarney","Charnov","ZhangMegreyD",
             "RikhterEfanov1","RikhterEfanov2")
  
  ## Individual methods with justM
  for (i in meths) {
    expect_is(metaM(i,tmax=3,Linf=12.93,Winf=20.79,K=0.23,t0=-1.23,
                    b=3.22,t50=0.83,T=17,L=3),"numeric")
  }
  
  ## Individual methods without justM
  for (i in meths) {
    tmp <- metaM(i,justM=FALSE,tmax=3,Linf=12.93,Winf=20.79,K=0.23,t0=-1.23,
                 b=3.22,t50=0.83,T=17,L=3)
    expect_is(tmp,"metaM")
    expect_equal(mode(tmp),"list")
    expect_equal(tmp[["method"]],i)
    expect_message(print(tmp))
  }
  
  ## Multiple selected methods
  tmp <- metaM(meths[1:2],tmax=3,Linf=12.93,Winf=20.79,K=0.23,t0=-1.23,
               b=3.22,t50=0.83,T=17,L=3)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),2)
  expect_equal(names(tmp),c("method","M"))
  expect_equal(tmp$method,meths[1:2])
  expect_equal(ncol(tmp),2)
  
  tmp <- metaM(meths,tmax=3,Linf=12.93,Winf=20.79,
               K=0.23,t0=-1.23,b=3.22,t50=0.83,T=17,L=3)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),length(meths))
  expect_equal(names(tmp),c("method","M"))
  expect_equal(tmp$method,meths)
  expect_equal(ncol(tmp),2)
  
  expect_message(tmp <- metaM(meths,justM=FALSE,tmax=3,Linf=12.93,Winf=20.79,
                              K=0.23,t0=-1.23,b=3.22,t50=0.83,T=17,L=3))
  
  tmp <- metaM(Mmethods("tmax"),tmax=3,Linf=12.93,Winf=20.79,
               K=0.23,t0=-1.23,b=3.22,t50=0.83,T=17,L=3)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),length(Mmethods("tmax")))
  expect_equal(names(tmp),c("method","M"))
  expect_equal(tmp$method,Mmethods("tmax"))
  expect_equal(ncol(tmp),2)
  tmp <- metaM(Mmethods("K"),tmax=3,Linf=12.93,Winf=20.79,
               K=0.23,t0=-1.23,b=3.22,t50=0.83,T=17,L=3)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),length(Mmethods("K")))
  expect_equal(names(tmp),c("method","M"))
  expect_equal(tmp$method,Mmethods("K"))
  expect_equal(ncol(tmp),2)
  tmp <- metaM(Mmethods("Pauly"),tmax=3,Linf=12.93,Winf=20.79,
               K=0.23,t0=-1.23,b=3.22,t50=0.83,T=17,L=3)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),length(Mmethods("Pauly")))
  expect_equal(names(tmp),c("method","M"))
  expect_equal(tmp$method,Mmethods("Pauly"))
  expect_equal(ncol(tmp),2)
  tmp <- metaM(Mmethods("Hoenig"),tmax=3,Linf=12.93,Winf=20.79,
               K=0.23,t0=-1.23,b=3.22,t50=0.83,T=17,L=3)
  expect_is(tmp,"data.frame")
  expect_equal(nrow(tmp),length(Mmethods("Hoenig")))
  expect_equal(names(tmp),c("method","M"))
  expect_equal(tmp$method,Mmethods("Hoenig"))
  expect_equal(ncol(tmp),2)
})


## Validate Results ----
test_that("metaM() matches results from Kenchington (2014)",{  
  ## Chesapeke Bay Anchovy
  ##   matches except for the way R rounds 5s in JensenK1 method
  meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","HoenigO2","HoenigO2F",
             "JensenK1","Gislason","AlversonCarney","Charnov","ZhangMegreyD",
             "RikhterEfanov1","RikhterEfanov2")
  tmp <- metaM(meths,tmax=3,Linf=12.93,Winf=20.79,K=0.23,t0=-1.23,
               b=3.22,t50=0.83,T=17,L=3)
  tmp <- data.frame(tmp,expM=c(0.69,0.66,1.43,1.42,1.69,1.83,0.35,2.71,
                               2.30,2.06,1.17,1.58,1.22))
  expect_equivalent(round(tmp$M,2)[-7],tmp$expM[-7])
  
  ## Norwegian Fjord Lanternfish
  ##   matches except for the HoenigOF and HoenigO2F, which Kenchington acknowledged as erroneous
  tmp <- metaM(meths,tmax=8,Linf=8.306,Winf=8.68,K=0.204,t0=-0.64,
               b=3.26,t50=2,T=8,L=2)
  tmp <- data.frame(tmp,expM=c(0.51,0.46,0.55,0.61,0.58,0.64,0.31,
                               2.44,0.71,1.73,0.77,0.77,0.93))
  expect_equivalent(round(tmp$M,2)[-c(4,6)],tmp$expM[-c(4,6)])
  
  ## Rio Formosa Seahorse
  ## ALL MATCHES (note use of ZhangMegreyP)
  meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","HoenigO2","HoenigO2F",
             "JensenK1","Gislason","AlversonCarney","Charnov","ZhangMegreyP",
             "RikhterEfanov1","RikhterEfanov2")
  tmp <- metaM(meths,tmax=5.5,Linf=19.76,Winf=17.3,K=0.571,t0=-0.91
               ,b=3.276,t50=0.49,T=19,L=10)
  tmp <- data.frame(tmp,expM=c(1.16,1.30,0.79,0.77,0.88,0.87,0.86,
                               1.78,0.75,1.59,0.33,2.39,1.53))
  expect_equivalent(round(tmp$M,2),tmp$expM)
})

test_that("metaM() matches M.empirical() from fishmethods for Rio Formosa Seahorse",{
  ## ALL MATCHES
  if (require(fishmethods)) {
    meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","AlversonCarney","Gislason")
    tmp <- metaM(meths,tmax=5.5,Linf=19.76,Winf=17.3,K=0.571,t0=-0.91,
                 b=3.276,t50=0.49,T=19,L=10)
    tmp2 <- M.empirical(Linf=19.76,Winf=17.3,Kl=0.571,Kw=0.571,TC=19,
                        Bl=10,tmax=5.5,method=c(1:4,9))
    tmp <- data.frame(tmp,tmp2)
    expect_equivalent(round(tmp$M,3),tmp$M.1)
  }
})

