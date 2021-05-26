# create year as a factor variable
Mirex$fyear <- factor(Mirex$year)
Mirex$cyear <- as.character(Mirex$year)
Mirex$cspecies <- as.character(Mirex$species)


# One-way ANOVA####
aov1 <- lm(mirex~fyear,data=Mirex)
aov1c <- lm(mirex~cyear,data=Mirex)
aov2 <- lm(mirex~species,data=Mirex)
aov2c <- lm(mirex~cspecies,data=Mirex)
fitPlot(aov1)
fitPlot(aov1c)
fitPlot(aov1,col="red")
fitPlot(aov1,col.ci="red")
fitPlot(aov2)
fitPlot(aov2c)

#Should return CI errors
fitPlot(aov1,conf.level=1)
fitPlot(aov1,conf.level=0)
fitPlot(aov1,conf.level="A")

## Two-way ANOVA####
aov3 <- lm(mirex~species*fyear,data=Mirex)
aov3c <- lm(mirex~species*cyear,data=Mirex)
aov3cc <- lm(mirex~cspecies*cyear,data=Mirex)
fitPlot(aov3)
fitPlot(aov3c)
fitPlot(aov3cc)

# interaction plots and color change
fitPlot(aov3,legend="bottomleft")
fitPlot(aov3,change.order=TRUE)
fitPlot(aov3,col="jet")
# main effects plots
fitPlot(aov3,which="species")
fitPlot(aov3c,which="species")
fitPlot(aov3,which="fyear")
fitPlot(aov3c,which="fyear")

#Should return CI errors
fitPlot(aov3,conf.level=1)
fitPlot(aov3,conf.level=0)
fitPlot(aov3,conf.level="A")


## Simple linear regression (showing color change and confidence and prediction bands)####
slr1 <- lm(mirex~weight,data=Mirex)
fitPlot(slr1,pch=8,col.pt="red")
fitPlot(slr1,col.mdl="blue")
fitPlot(slr1,interval="confidence")
fitPlot(slr1,interval="prediction")
fitPlot(slr1,interval="both")

#Should return CI errors
fitPlot(slr1,conf.level=1)
fitPlot(slr1,conf.level=0)
fitPlot(slr1,conf.level="A")



## Indicator variable regression with one factor (also showing confidence bands)####
ivr1 <- lm(mirex~weight*fyear,data=Mirex)
ivr1c <- lm(mirex~weight*cyear,data=Mirex)
fitPlot(ivr1,legend="topleft")
fitPlot(ivr1c,legend="topleft")
fitPlot(ivr1,legend="topleft",interval="confidence")
fitPlot(ivr1c,legend="topleft",interval="confidence")
fitPlot(ivr1,legend="topleft",col="Dark 2",pch=18,lty=1)
fitPlot(ivr1c,legend="topleft",col="Dark 2",pch=18,lty=1)

#Should return CI errors
fitPlot(ivr1,conf.level=1)
fitPlot(ivr1,conf.level=0)
fitPlot(ivr1,conf.level="A")

## Indicator variable regression with one factor (as first variable)
ivr2 <- lm(mirex~fyear*weight,data=Mirex)
ivr2c <- lm(mirex~cyear*weight,data=Mirex)
fitPlot(ivr2,legend="topleft")
fitPlot(ivr2c,legend="topleft")

## Indicator variable regression with one factor (assuming parallel lines)
ivr3 <- lm(mirex~weight+fyear,data=Mirex)
ivr3c <- lm(mirex~weight+cyear,data=Mirex)
fitPlot(ivr3,legend="topleft")
fitPlot(ivr3c,legend="topleft")
fitPlot(ivr3,legend="topleft",col="Dark 2")
fitPlot(ivr3c,legend="topleft",col="Dark 2")


# reduce number of years for visual simplicity
Mirex2 <- droplevels(subset(Mirex,fyear %in% c(1977,1992)))

## Indicator variable regression with two factors
ivr4 <- lm(mirex~weight*fyear*species,data=Mirex2)
ivr4c <- lm(mirex~weight*cyear*species,data=Mirex2)
ivr4cc <- lm(mirex~weight*cyear*cspecies,data=Mirex2)
fitPlot(ivr4)
fitPlot(ivr4c)
fitPlot(ivr4cc)
fitPlot(ivr4,ylim=c(0,0.8),legend="topleft")

## Indicator variable regression with two factors (but different orders)
ivr5 <- lm(mirex~fyear*weight*species,data=Mirex2)
ivr5c <- lm(mirex~cyear*weight*species,data=Mirex2)
ivr5cc <- lm(mirex~cyear*weight*cspecies,data=Mirex2)
fitPlot(ivr5)
fitPlot(ivr5c)
fitPlot(ivr5cc)
ivr6 <- lm(mirex~fyear*species*weight,data=Mirex2)
fitPlot(ivr6)
ivr7 <- lm(mirex~species*fyear*weight,data=Mirex2)
fitPlot(ivr7)


## Polynomial regression####
poly1 <- lm(mirex~weight+I(weight^2),data=Mirex)
fitPlot(poly1,interval="both")

#Should return CI errors
fitPlot(poly1,conf.level=1)
fitPlot(poly1,conf.level=0)
fitPlot(poly1,conf.level="A")


## Non-linear model example####
lr.sv <- list(B1=6,B2=7.2,B3=-1.5)
nl1 <- nls(cells~B1/(1+exp(B2+B3*days)),start=lr.sv,data=Ecoli)
fitPlot(nl1,Ecoli,cex.main=0.7,lwd=2)
fitPlot(nl1,Ecoli,xlab="Day",ylab="Cellsx10^6/ml",plot.pts=FALSE)


## Logistic regression example####
## NASA space shuttle o-ring failures -- from graphics package
fail <- factor(c(2,2,2,2,1,1,1,1,1,1,2,1,2,1,1,1,1,2,1,1,1,1,1),
levels = 1:2, labels = c("no","yes"))
temperature <- c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)
d <- data.frame(fail,temperature)
glm1 <- glm(fail~temperature,data=d,family="binomial")
fitPlot(glm1)
fitPlot(glm1,breaks=seq(52,82,2))
fitPlot(glm1,yaxis1.ticks=c(0,1),yaxis1.lbls=c(0,1))
# changing the size of the y-axis labels
par(cex.axis=1.5,cex.lab=1.5)
fitPlot(glm1)

