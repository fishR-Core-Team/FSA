context("nlsTracePlot() MESSAGES")
data(SpotVA1)
vb1 <- vbFuns()
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,start=list(Linf=12,K=0.3,t0=0))
fit3 <- lm(tl~age,data=SpotVA1)

test_that("nlsTracePlot() test messages",{
  # wrong type
  expect_error(nlsTracePlot(fit3,bh1),"must be from")
  # no fun
  expect_error(nlsTracePlot(fit1),"missing, with no default")
  # bad pallette choice
  expect_error(nlsTracePlot(fit1,vb1,pal="derek"),"should be one of")
  # bad n
  expect_error(nlsTracePlot(fit1,vb1,n=1),"'n' must be greater than 2")
  # bad legend
  expect_error(nlsTracePlot(fit1,vb1,legend="derek"),"should be one of")
})
  