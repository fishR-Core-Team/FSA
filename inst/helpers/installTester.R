library(FSA)

## Some data and models for below
df <- data.frame(y=c(10,-10,runif(28)),x=c(runif(30)),
                 f=sample(c("A","B","C"),30,replace=TRUE))
slrout <- lm(y~x,data=df)
ivrout <- lm(y~x*f,data=df)
aov1 <- lm(y~f,data=df)

## test of dunn.test functions
dunnTest(y~f,data=df)

## test of lmtest functions
lrt(slrout,com=ivrout)

## test of plotrix functions
data(WhitefishLC)
ab1 <- ageBias(scaleC~otolithC,data=WhitefishLC,
               ref.lab="Otolith Age",nref.lab="Scale Age")
plot(ab1)
plotAB(ab1)
data(WR79)
WR.age <- subset(WR79, !is.na(age))
WR.age$LCat <- lencat(WR.age$len,w=5)
WR.key <- prop.table(xtabs(~LCat+age,data=WR.age), margin=1)
alkPlot(WR.key,"area")
data(ChinookArg)
lm1 <- lm(w~tl*loc,data=ChinookArg)
lwCompPreds(lm1,xlab="Location")


## Make sure all dependent and imported packages would load
library(plyr)
library(dplyr)
library(car)
library(dunn.test)
library(lmtest)
library(plotrix)
library(sciplot)
