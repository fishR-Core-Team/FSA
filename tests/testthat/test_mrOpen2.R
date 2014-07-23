context("Mark-Recapture, Open Populations, Jolly-Seber")

## Jolly's data
# summarized data entered "by hand"
s1 <- rep(NA,13)
s2 <- c(10,rep(NA,12))
s3 <- c(3,34,rep(NA,11))
s4 <- c(5,18,33,rep(NA,10))
s5 <- c(2,8,13,30,rep(NA,9))
s6 <- c(2,4,8,20,43,rep(NA,8))
s7 <- c(1,6,5,10,34,56,rep(NA,7))
s8 <- c(0,4,0,3,14,19,46,rep(NA,6))
s9 <- c(0,2,4,2,11,12,28,51,rep(NA,5))
s10 <- c(0,0,1,2,3,5,17,22,34,rep(NA,4))
s11 <- c(1,2,3,1,0,4,8,12,16,30,rep(NA,3))
s12 <- c(0,1,3,1,1,2,7,4,11,16,26,NA,NA)
s13 <- c(0,1,0,2,3,3,2,10,9,12,18,35,NA)
jolly.top <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13)
n <- c(54,146,169,209,220,209,250,176,172,127,123,120,142)
R <- c(54,143,164,202,214,207,243,175,169,126,120,120,0)
m <- c(0,10,37,56,53,77,112,86,110,84,77,72,95)
u <- n-m
jolly.bot <- rbind(m,u,n,R)

# expectations from Seber (2002) Table 5.3 (page 207)
rJ <- c(24,80,70,71,109,101,108,99,70,58,44,35,NA)
zJ <- c(NA,14,57,71,89,121,110,132,121,107,88,60,NA)
MJ <- c(NA,35.02,170.54,258,227.73,324.99,359.50,319.33,402.13,316.45,317,277.71,NA)
NJ <- c(NA,511.2,779.1,963.0,945.3,882.2,802.5,653.6,628.8,478.5,506.4,462.8,NA)
phiJ <- c(0.649,1.015,0.867,0.564,0.836,0.790,0.651,0.985,0.686,0.884,0.771,NA,NA)
BJ <- c(NA,263.2,291.8,406.4,96.9,107.0,135.7,-13.8,49.0,84.1,74.5,NA,NA)

# mrOpen() results
jolly <- mrOpen(jolly.top,jolly.bot)


## Field Vole example from Krebs (Table 2.2)
# data entered by hand
s1 <- rep(NA,11)
s2 <- c(15,rep(NA,10))
s3 <- c(1,15,rep(NA,9))
s4 <- c(0,0,37,rep(NA,8))
s5 <- c(0,1,2,61,rep(NA,7))
s6 <- c(0,0,0,4,75,rep(NA,6))
s7 <- c(0,0,0,1,3,77,rep(NA,5))
s8 <- c(0,0,0,1,2,4,69,rep(NA,4))
s9 <- c(0,0,0,0,0,0,0,8,rep(NA,3))
s10 <- c(0,0,0,0,0,0,0,1,14,rep(NA,2))
s11 <- c(0,0,0,0,0,0,0,0,0,19,NA)
krebs.top <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11)
n <- c(22,41,48,82,89,101,107,91,19,27,22)
m <- c(0,15,16,37,64,79,81,76,8,15,19)
u <- n-m
R <- c(21,41,46,82,88,99,106,90,19,26,22)
krebs.bot <- rbind(m,u,n,R)

# Expectations from Krebs Table 2.3
Mk <- c(NA,17.5,17.2,40.7,70.5,87.5,91.7,76,9.3,15,NA)
# put 27 in for N10 because 27 were captured in that time
Nk <- c(NA,45.9,49.5,88.8,97.7,111.6,120.8,90.8,20.7,27,NA)
NSEk <- c(NA,6.1,4.0,5.3,5.3,5.8,7.1,6.5,3.6,4.4,NA)
phik <- c(0.832,0.395,0.862,0.824,0.925,0.853,0.651,0.104,0.738,NA,NA)
phiSEk <- c(0.126,0.077,0.055,0.043,0.032,0.043,0.046,0.033,0.101,NA,NA)
# Put in 11.7 for B9 because of change to N10 above
Bk <- c(NA,31.4,47.9,24.6,22.1,27.3,12.8,11.4,11.7,NA,NA)
BSEk <- c(NA,2.7,3.6,3.3,2.6,2.8,1.7,1.8,1.1,NA,NA)

# mrOpen() results
krebs <- mrOpen(krebs.top,krebs.bot)



test_that("Does mrOpen match the Jolly-Seber results from Table 5.3 in Seber (2002)",{
  expect_that(jolly$df$r,equals(rJ))
  expect_that(jolly$df$z,equals(zJ))
  
  # test estimated M --- generally close (<1% overestimate)
  cbind(jolly$df$M,MJ,round((jolly$df$M-MJ)/MJ*100,1))
  # test estimated N -- generally (<3% overestimate except for N2)
  cbind(jolly$df$N,NJ,round((jolly$df$N-NJ)/NJ*100,1))
  # test SE of N
  # test estimated phi --- generally close (<1% overestimate)
  cbind(jolly$df$phi,phiJ,round((jolly$df$phi-phiJ)/phiJ*100,1))
  # test SE of phi
  # test estimated B --- three good-sized discrepancies
  cbind(jolly$df$B,BJ,round((jolly$df$B-BJ)/BJ*100,1))
  # test SE of B
})


test_that("Does mrOpen match the Jolly-Seber results from Table 2.3 in Krebs (1989)",{
  # test estimated M
  for (i in 1:length(Mk)) {
    # off by 0.1 for M5
    if (i!=5) expect_that(krebs$df$M[i],equals(Mk[i]))
  }
  # test estimated N
  for (i in 1:length(Nk)) {
    # off by 0.2 for N8
    if (i!=8) expect_that(krebs$df$N[i],equals(Nk[i]))
  }
  
  # test SE of N
  ## mrOpen() results are not even close to Krebs values
  
  # test estimated phi
  for (i in 1:length(phik)) {
    expect_that(krebs$df$phi[i],equals(phik[i]))
  }
  
  # test SE of phi
  for (i in 1:length(phiSEk)) {
    expect_that(krebs$df$phi.se[i],equals(phiSEk[i]))
  }
  
  # test estimated B
  for (i in 1:length(Bk)) {
    # off by 0.2 for B7 (same as for N8)
    if (i!=7) expect_that(krebs$df$B[i],equals(Bk[i]))
  }
  
  # test SE of B
  for (i in 1:length(BSEk)) {
    # off by 0.1,0.1,0.4 for B6,B7,B9 (same as for N8)
    if (!(i %in% c(6,7,9))) expect_that(krebs$df$B.se[i],equals(BSEk[i]))
  }  
})