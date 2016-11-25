context("metaM() OUTPUT")

test_that("metaM() output",{  
  meths <- c("PaulyL","PaulyW","HoenigO","HoenigOF","HoenigO2","HoenigO2F","JensenK1","Gislason","AlversonCarney","Charnov","ZhangMegreyD","RikhterEfanov1","RikhterEfanov2")

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
})
