context("nlsTracePlot() OUTPUT")
vb1 <- vbFuns()
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,start=list(Linf=12,K=0.3,t0=0))

test_that("nlsTracePlot() test output",{
  # successful fit
  tmp <- nlsTracePlot(fit1,vb1,add=FALSE)
  expect_is(tmp,"matrix")
  expect_equal(mode(tmp),"numeric")
  expect_equal(ncol(tmp),4)
  # unsuccessful fit
  if (require(FSAdata)) {
    data(BSkateGB,package="FSAdata")
    wtr <- filterD(BSkateGB,season=="winter")
    bh1 <- srFuns()
    trc <- capture.output(try(
      expect_error(fit2 <- nls(recruits~bh1(spawners,a,b),wtr,
                               start=srStarts(recruits~spawners,data=wtr),
                               trace=TRUE))
    ))
    tmp <- nlsTracePlot(trc,bh1,add=FALSE)
    expect_is(tmp,"matrix")
    expect_equal(mode(tmp),"numeric")
    expect_equal(ncol(tmp),3)
  }
})
  