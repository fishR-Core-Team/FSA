# Get example results of nls fit
data(SpotVA1)
vb1 <- vbFuns()
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,start=list(Linf=12,K=0.3,t0=0))

if (require(FSAdata)) {
  data(BSkateGB)
  wtr <- filterD(BSkateGB,season=="winter")
  bh1 <- srFuns()
  trc <- capture.output(try(
    fit2 <- nls(recruits~bh1(spawners,a,b),wtr,
                start=srStarts(recruits~spawners,data=wtr),trace=TRUE)
  ))
}

fit3 <- lm(tl~age,data=SpotVA1)
