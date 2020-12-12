# add log length and weight data to ChinookArg data
ChinookArg$logtl <- log(ChinookArg$tl)
ChinookArg$logwt <- log(ChinookArg$w)
# fit model to assess equality of slopes
lm1 <- lm(logwt~logtl*loc,data=ChinookArg)
anova(lm1)

# set graphing parameters so that the plots will look decent
op <- par(mar=c(3.5,3.5,1,1),mgp=c(1.8,0.4,0),tcl=-0.2)
# show predicted weights (w/ CI) at the default quantile lengths for each year
lwCompPreds(lm1,xlab="Location")
# show predicted weights (w/ CI) at the quartile lengths for each year
lwCompPreds(lm1,xlab="Location",qlens=c(0.25,0.5,0.75))
# show predicted weights (w/ CI) at certain lengths for each year
lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150))
# show predicted weights (w/ just PI) at certain lengths for each year
lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150),interval="prediction")
# show predicted weights (w/ CI and PI) at certain lengths for each year
lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150),interval="both")
# show predicted weights (w/ CI and points at the prediction) at certain lengths for each year
lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150),show.preds=TRUE)
# show predicted weights (w/ CI but don't connect means) at certain lengths for each year
lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150),connect.preds=FALSE,show.preds=TRUE)

# fit model with centered data
mn.logtl <- mean(ChinookArg$logtl,na.rm=TRUE)
ChinookArg$clogtl <- ChinookArg$logtl-mn.logtl
lm2 <- lm(logwt~clogtl*loc,data=ChinookArg)
lwCompPreds(lm2,xlab="Location",center.value=mn.logtl)
lwCompPreds(lm2,xlab="Location",lens=c(60,90,120,150),center.value=mn.logtl)

# fit model with a different base (plot should be the same as the first example)
ChinookArg$logtl <- log10(ChinookArg$tl)
ChinookArg$logwt <- log10(ChinookArg$w)
lm1 <- lm(logwt~logtl*loc,data=ChinookArg)
lwCompPreds(lm1,base=10,xlab="Location")

if (interactive()) {
  # should give error, does not work for only a simple linear regression
  lm2 <- lm(logwt~logtl,data=ChinookArg)
  lwCompPreds(lm2)
  # or a one-way ANOVA
  lm3 <- lm(logwt~loc,data=ChinookArg)
  lwCompPreds(lm3)   
}

## return graphing parameters to original state
par(op)
