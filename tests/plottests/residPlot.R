# MIREX DATA ... 
# create year factor variable of year, characters of year and species for
#   testing the new read.csv() that does not make factors
Mirex$fyear <- factor(Mirex$year)
Mirex$cyear <- as.character(Mirex$year)
Mirex$cspecies <- as.character(Mirex$species)

# reduce number of years for visual simplicity in IVRs
Mirex2 <- droplevels(subset(Mirex,fyear %in% c(1977,1992)))



## One-way ANOVA
aov1 <- lm(mirex~fyear,data=Mirex)
aov1c <- lm(mirex~cyear,data=Mirex)
aov2 <- lm(mirex~species,data=Mirex)
aov2c <- lm(mirex~cspecies,data=Mirex)
# default (uses boxplots)
residPlot(aov1)
residPlot(aov1c)
residPlot(aov2)
residPlot(aov2c)
# use points rather than boxplot
residPlot(aov1,bp=FALSE)
residPlot(aov1c,bp=FALSE)
residPlot(aov2,bp=FALSE)
residPlot(aov2c,bp=FALSE)


## Two-Way ANOVA
aov3 <- lm(mirex~species*fyear,data=Mirex)
aov3c <- lm(mirex~species*cyear,data=Mirex)
aov3cc <- lm(mirex~cspecies*cyear,data=Mirex)
# default (uses boxplots)
residPlot(aov3)
residPlot(aov3c)
residPlot(aov3cc)
# No boxplots
residPlot(aov3,bp=FALSE)
residPlot(aov3c,bp=FALSE)
residPlot(aov3cc,bp=FALSE)


## Simple linear regression
slr1 <- lm(mirex~weight,data=Mirex)
residPlot(slr1)
residPlot(slr1,loess=TRUE)



## Indicator variable regression with only one factor
ivr1 <- lm(mirex~weight*fyear,data=Mirex)
ivr1c <- lm(mirex~weight*cyear,data=Mirex)
residPlot(ivr1)
residPlot(ivr1c)
residPlot(ivr1,inclHist=FALSE)
residPlot(ivr1c,inclHist=FALSE)
residPlot(ivr1,inclHist=FALSE,pch=19)
residPlot(ivr1c,inclHist=FALSE,pch=19)
residPlot(ivr1,inclHist=FALSE,pch=19,col="black")
residPlot(ivr1c,inclHist=FALSE,pch=19,col="black")
residPlot(ivr1,inclHist=FALSE,legend=FALSE)
residPlot(ivr1c,inclHist=FALSE,legend=FALSE)
residPlot(ivr1,inclHist=FALSE,pch=2,col="red",legend=FALSE)
residPlot(ivr1c,inclHist=FALSE,pch=2,col="red",legend=FALSE)

## Indicator variable regression (assuming same slope)
ivr2 <- lm(mirex~weight+fyear,data=Mirex)
ivr2c <- lm(mirex~weight+cyear,data=Mirex)
residPlot(ivr2)
residPlot(ivr2c)


## Indicator variable regression with two factors
ivr3 <- lm(mirex~weight*fyear*species,data=Mirex2)
ivr3c <- lm(mirex~weight*cyear*species,data=Mirex2)
ivr3cc <- lm(mirex~weight*cyear*cspecies,data=Mirex2)

# defaults
residPlot(ivr3)
residPlot(ivr3c)
residPlot(ivr3cc)
residPlot(ivr3,inclHist=FALSE)
residPlot(ivr3c,inclHist=FALSE)
residPlot(ivr3cc,inclHist=FALSE)
residPlot(ivr3,loess=TRUE,inclHist=FALSE)
residPlot(ivr3c,loess=TRUE,inclHist=FALSE)
residPlot(ivr3cc,loess=TRUE,inclHist=FALSE)
residPlot(ivr3,col="rainbow",inclHist=FALSE)
residPlot(ivr3c,col="rainbow",inclHist=FALSE)
residPlot(ivr3cc,col="rainbow",inclHist=FALSE)
residPlot(ivr3,pch=16,inclHist=FALSE)
residPlot(ivr3c,pch=16,inclHist=FALSE)
residPlot(ivr3cc,pch=16,inclHist=FALSE)
residPlot(ivr3,pch=16,col="black",legend=FALSE,inclHist=FALSE)
residPlot(ivr3c,pch=16,col="black",legend=FALSE,inclHist=FALSE)
residPlot(ivr3cc,pch=16,col="black",legend=FALSE,inclHist=FALSE)
residPlot(ivr3,legend=FALSE,inclHist=FALSE)
residPlot(ivr3c,legend=FALSE,inclHist=FALSE)
residPlot(ivr3cc,legend=FALSE,inclHist=FALSE)
residPlot(ivr3,col.ref="blue",lwd.ref=5,inclHist=FALSE)
residPlot(ivr3c,col.ref="blue",lwd.ref=5,inclHist=FALSE)
residPlot(ivr3cc,col.ref="blue",lwd.ref=5,inclHist=FALSE)
residPlot(ivr3,main="MODEL")
residPlot(ivr3c,main="MODEL")
residPlot(ivr3cc,main="MODEL")
residPlot(ivr3,resid.type="studentized",inclHist=FALSE)
residPlot(ivr3c,resid.type="studentized",inclHist=FALSE)
residPlot(ivr3cc,resid.type="studentized",inclHist=FALSE)
residPlot(ivr3,resid.type="standardized",inclHist=FALSE)
residPlot(ivr3c,resid.type="standardized",inclHist=FALSE)
residPlot(ivr3cc,resid.type="standardized",inclHist=FALSE)

## Indicator variable regression with same two factors but in different order
##   (notice use of colors and symbols)
lm4 <- lm(mirex~weight*species*fyear,data=Mirex2)
lm4c <- lm(mirex~weight*species*cyear,data=Mirex2)
lm4cc <- lm(mirex~weight*cspecies*cyear,data=Mirex2)
residPlot(lm4)
residPlot(lm4c)
residPlot(lm4cc)



## Examples showing outlier detection
x <- c(runif(100))
y <- c(7,runif(99))
lma <- lm(y~x)
residPlot(lma)
residPlot(lma,resid.type="studentized")

# multiple outliers
y <- c(7,runif(98),-5)
lmb <- lm(y~x)
residPlot(lmb)

# check that NAs are handled properly ... label should be 100
y <- c(NA,NA,runif(97),7)
lmc <- lm(y~x)
residPlot(lmc)


## Nonlinear regression
# from first example in nls()
DNase1 <- subset(DNase,Run==1)
fm1DNase1 <- nls(density~SSlogis(log(conc),Asym,xmid,scal),DNase1)
residPlot(fm1DNase1)
residPlot(fm1DNase1,resid.type="standardized")
