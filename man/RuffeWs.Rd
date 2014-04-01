\docType{data}
\name{RuffeWs}
\alias{RuffeWs}
\title{Raw Length-weight data that can be used to compute the Ws for ruffe.}
\format{A data frame with 20005 observations on the following 13 variables:
\describe{
\item{regrnum}{A unique numeric identifier for each separate regression -- should
match one-to-one with \code{locShort}.}
\item{fishID}{A numeric identifier for each fish that is not unique.}
\item{country}{Country of data set.}
\item{locShort}{Name (and possibly year) of data set.}
\item{watertype}{Waterbody type descriptor.}
\item{year}{Year of collection.}
\item{month}{Month of collection.}
\item{day}{Day of collection.}
\item{tl}{Total length (mm) of fish.}
\item{fl}{Fork length (mm) of fish.}
\item{sl}{Standard length (mm) of fish.}
\item{wt}{Weight (g) of fish.}
\item{use}{Use of data set -- either \code{develop} or \code{validate}.}
}}
\source{
From Ogle, D.H. and I.J. Winfield.  2009.  Ruffe
length-weight relationships with a proposed standard weight
equation.  North American Journal of Fisheries Management
29:850-85.
}
\description{
Raw length-weight data from a variety of populations used
for computing the standard weight (Ws) equation for ruffe
(\emph{Gymnocephalus cernuus}).
}
\section{Topic(s)}{
  \itemize{ \item Relative weight \item Standard weight
  \item Length-weight }
}
\examples{
data(RuffeWs)
## Create log10 TL and Wt
RuffeWs$logtl <- log10(RuffeWs$tl)
RuffeWs$logwt <- log10(RuffeWs$wt)

## Isolate development and validation data sets
rWs.d <- subset(RuffeWs,use=="develop")
rWs.d <- gdata::drop.levels(rWs.d,reorder=FALSE)
str(rWs.d)

rWs.v <- subset(RuffeWs,use=="validate")
rWs.v <- gdata::drop.levels(rWs.v,reorder=FALSE)
str(rWs.v)

## Compute regression results for all data sets in development set
# First, a function to loop through the Wt vs. TL regressions
loopreg <- function(d,alpha=0.05) {
 reg.nums <- sort(unique(d$regrnum))
 n <- minTL <- maxTL <- minWT <- maxWT <- r2 <- loga <- b <- numeric(length(reg.nums))
 reg.lbls <- country <- character(length(reg.nums))
 sigb3 <- logical(length(reg.nums))
 for (i in 1:length(reg.nums)) {
   df <- gdata::drop.levels(subset(d,regrnum==reg.nums[i]),reorder=FALSE)
   reg.lbls[i] <- levels(df$loc)
   minTL[i] <- min(df$tl)
   maxTL[i] <- max(df$tl)
   minWT[i] <- min(df$wt)
   maxWT[i] <- max(df$wt)
   lw.lm <- lm(logwt~logtl,data=df)
   n[i] <- dim(lw.lm$model)[1]
   r2[i] <- summary(lw.lm)$r.squared
   loga[i] <- coef(lw.lm)[1]
   b[i] <- coef(lw.lm)[2]
 }
 data.frame(reg.lbls,n,minTL,maxTL,minWT,maxWT,loga,b,sigb3,r.sq=round(r2,3))
}

# Second, do the looping and view the results
reg.d <- loopreg(rWs.d)
head(reg.d)


## Ruffe Ws equation from Ogle and Winfield
# Summarize TL to set min and max to emp()
summary(rWs.d$tl)
# Find the EmP Ws equation
emp.res.d <- emp(rWs.d,pop="regrnum",len="tl",wt="wt",min=60,max=220,w=10,
 n.cutoff=4,cutoff.tail=TRUE)
# EmP model results
anova(emp.res.d)
summary(emp.res.d)
# Fitted model and residual plots
fitPlot(emp.res.d)
plot(emp.res.d$Ws$residuals~emp.res.d$Ws$fitted.values,pch=19)
abline(h=0,lty=3)
# Sample sizes
emp.res.d$pop.by.len
emp.res.d$ind.by.len
# Fitted model relative to mean weights by length categories
plot(emp.res.d)

# Validate (explore length-related biases)
# Perform validation calculations -- Willis method
emp.v.w <- wsValidate(emp.res.d,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,type="Willis")
# Observed validation results
summary(emp.v.w)
emp.v.w
# See individual regressions -- NOT RUN
\dontrun{
xyplot(Wr~tl|locShort,data=emp.v.w$df,type=c("p","r"),par.strip.text=list(cex=0.5))
}

# Perform validation calculations -- weighted EmpQ method
emp.v.q <- wsValidate(emp.res.d,rWs.v,"regrnum","tl","wt",min=60,max=220,w=10,
 type="EmpQ",weighted=TRUE)
anova(emp.v.q)
fitPlot(emp.v.q)


## Fit the RLP model
rlp.res.d <- rlp(reg.d$loga,reg.d$b,min=60,max=210,w=10)
# RLP model results
summary(rlp.res.d)
# Fitted model and residual plot
fitPlot(rlp.res.d)
#######residPlot(rlp.res.d)
# Show rlp model relative to all regression lines
plot(rlp.res.d)

# Validate (explore length-related biases)
# Perform validation calculations -- Willis method
rlp.v.w <- wsValidate(rlp.res.d,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,type="Willis")
summary(rlp.v.w)
rlp.v.w

# See individual regressions -- NOT RUN
\dontrun{
xyplot(Wr~tl|locShort,data=rlp.v.w$df,type=c("p","r"),par.strip.text=list(cex=0.5))
}

# Perform validation calculations -- weighted EmpQ method
rlp.v.q <- wsValidate(rlp.res.d,rWs.v,"regrnum","tl","wt",min=60,max=210,w=10,
 type="EmpQ",weighted=TRUE)
anova(rlp.v.q)
fitPlot(rlp.v.q)


## "Fit" the Froese model
Froese.res.d <- FroeseWs(reg.d$loga,reg.d$b)
# Observe results
coef(Froese.res.d)
plot(Froese.res.d,min=55,max=200)

# Validate (explore length-related biases)
# Perform validation calculations -- Willis method
Froese.v.w <- wsValidate(Froese.res.d,rWs.v,"regrnum","tl","wt",min=60,max=190,w=10,type="Willis")
summary(Froese.v.w)
Froese.v.w

# See individual regressions -- NOT RUN
\dontrun{
xyplot(Wr~tl|locShort,data=Froese.v.w$df,type=c("p","r"),par.strip.text=list(cex=0.5))
}

# Perform validation calculations -- EmpQ method
Froese.v.q <- wsValidate(Froese.res.d,rWs.v,"regrnum","tl","wt",min=60,max=190,
 w=10,type="EmpQ",use.means=TRUE,weighted=TRUE)
anova(Froese.v.q)
fitPlot(Froese.v.q,ylab="Standarded Mean Mean Weight")
}
\seealso{
\code{\link{WSlit}}, \code{\link{wsVal}},
\code{\link{rlp}}, \code{\link{emp}}, and
\code{\link{wsValidate}}.
}
\keyword{datasets}

