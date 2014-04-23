\name{lwPredsComp}
\alias{lwPredsComp}
\title{Constructs plots of predicted weights at given lengths among different groups.}
\usage{
lwPredsComp(mdl, lens = NULL, quant.lens = c(0, 0.25, 0.5, 0.75, 1),
  interval = c("both", "confidence", "prediction"), center.value = 0,
  xlab = "Groups", ylab = "Predicted Weight", ylim = NULL,
  main.pre = "Length==", lwd = 2, connect.mean = TRUE,
  rows = round(sqrt(num)), cols = ceiling(sqrt(num)))
}
\arguments{
  \item{mdl}{An \code{lm} object (i.e., returned from
  fitting a model with \code{lm}).  This model should have
  log(weight) as the response and log(length) as the
  explanatory covariate and an explanatory factor variable
  that describes the different groups.}

  \item{lens}{A numeric that indicates the lengths at which
  the weights should be predicted.}

  \item{quant.lens}{A numeric that indicates the quantiles
  of length to be used for predicing weight.  This is
  ignored if \code{lens} is non-null.}

  \item{xlab}{A string for labelling the x-axis.}

  \item{ylab}{A string for labelling the y-axis.}

  \item{interval}{A string that indicates whether to plot
  confidence (\code{="confidence"}), prediction
  (\code{="prediction"}), or both (\code{="both"})
  intervals.}

  \item{center.value}{A numeric that indicates the value of
  log length used when centering the log length data.}

  \item{ylim}{A numeric of length two that indicates the
  limits of the y-axis to be used for each plot.  If null
  then limits will be chosen for each graph individually.}

  \item{main.pre}{A character string to be used as a prefix
  for the main title. See details.}

  \item{lwd}{A numeric used to indicate the line width to
  be used.  Note that this will be the width of the
  confidence interval lines and the prediction interval and
  mean connection lines will be one less than this value.}

  \item{connect.mean}{A logical that indicates whether the
  means should be connected with a line or not.}

  \item{rows}{A numeric that contains the number of rows to
  use on the graphic.}

  \item{cols}{A numeric that contains the number of columns
  to use on the graphic.}

  \item{\dots}{Other arguments to pass through to the
  \code{hist} function.}
}
\value{
None.  However, a plot is produced.
}
\description{
Constructs plots of predicted weights at given lengths
among different groups. These plots allow the user to
explore differences in predicted weights at a variety of
lengths when the length-weight relationship is not the same
across a variety of groups.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/LengthWeight.pdf}
}
\examples{
# load ruffe length-weight data
data(RuffeWs)
# extract just data from the St. Louis River Harbor
slrh <- subset(RuffeWs,grepl("St. Louis",RuffeWs$locShort))
# add log length and weight data
slrh$logtl <- log(slrh$tl)
slrh$logwt <- log(slrh$wt)
# create a year factor variable
slrh$fyear <- factor(slrh$year)
# fit model to assess equality of slopes
lm1 <- lm(logwt~logtl*fyear,data=slrh)
anova(lm1)

# show predicted weights (w/ CI and PI) at the default quantile lengths for each year
lwPredsComp(lm1,xlab="Year")
# show predicted weights (w/ CI and PI) at the quartile lengths for each year
lwPredsComp(lm1,xlab="Year",quant.lens=c(0.25,0.5,0.75))
# show predicted weights (w/ CI and PI) at certain lengths for each year
lwPredsComp(lm1,xlab="Year",lens=c(60,90,120,150))
# show predicted weights (just PI) at certain lengths for each year
lwPredsComp(lm1,xlab="Year",lens=c(60,90,120,150),interval="p")
# show predicted weights (just CI) at certain lengths for each year
lwPredsComp(lm1,xlab="Year",lens=c(60,90,120,150),interval="c")

# fit model with centered data
mn.logtl <- mean(slrh$logtl,na.rm=TRUE)
slrh$clogtl <- slrh$logtl-mn.logtl
lm2 <- lm(logwt~clogtl*fyear,data=slrh)
lwPredsComp(lm2,xlab="Year",center.value=mn.logtl)
lwPredsComp(lm2,xlab="Year",lens=c(60,90,120,150),center.value=mn.logtl)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\keyword{manip}

