## Create a nonlinear function
fnx <- function(days,B1,B2,B3) {
  if (length(B1) > 1) {
     B2 <- B1[2]
     B3 <- B1[3]
     B1 <- B1[1]
  }
  B1/(1+exp(B2+B3*days))
}
## Fit the nonlinear model
nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,start=list(B1=6,B2=7.2,B3=-1.45))
## Get the bootstrap results saved previously because nlsBoot() and bootCase()
##   will not run within a testing environment
load(system.file("extdata", "nlsBoot1.RData", package="FSA"))
load(system.file("extdata", "bootCase1.RData", package="FSA"))
