context("Confidence Interval functions VALIDATE")

test_that("binCI() compared to epitools functions",{
  res <- as.data.frame(binCI(7,10,verbose=TRUE))
  resepi <- rbind(epitools::binom.exact(7,10),
                  epitools::binom.wilson(7,10),
                  epitools::binom.approx(7,10))[,-6]
  rownames(resepi) <- c("Exact","Wilson","Asymptotic")
  expect_equivalent(res,resepi)

  res <- as.data.frame(binCI(5:7,10,type="wilson",verbose=TRUE))
  resepi <- epitools::binom.wilson(5:7,10)[,-6]
  expect_equivalent(res,resepi)
})

test_that("poiCI() compared to epitools functions",{
  res <- as.data.frame(poiCI(10,verbose=TRUE))
  resepi <- rbind(epitools::pois.exact(10),
                  epitools::pois.daly(10),
                  epitools::pois.byar(10),
                  epitools::pois.approx(10))[,-c(2,3,6)]
  rownames(resepi) <- c("Exact","Daly","Byar","Asymptotic")
  expect_equivalent(res,resepi)
  
  res <- as.data.frame(poiCI(5:7,type="exact",verbose=TRUE))
  resepi <- epitools::pois.exact(5:7)[,-c(2,3,6)]
  expect_equivalent(res,resepi)
})

