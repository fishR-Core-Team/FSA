\name{plotBinResp}
\alias{plotBinResp}
\alias{plotBinResp.default}
\alias{plotBinResp.formula}
\title{Plots a binary response variable versus a quantitative explanatory variable.}
\usage{
plotBinResp(x, ...)

\method{plotBinResp}{formula}(x, data = NULL, xlab = names(mf)[2],
  ylab = names(mf)[1], ...)

\method{plotBinResp}{default}(x, y, xlab = paste(deparse(substitute(x))),
  ylab = paste(deparse(substitute(y))), plot.pts = TRUE, col.pt = "black",
  trans.pt = NULL, plot.p = TRUE, breaks = 25, p.col = "blue",
  p.pch = 3, p.cex = 1.25, ...)
}
\arguments{
  \item{x}{A quantitative explanatory variable or a formula
  of the form \code{factor~quant}.}

  \item{y}{A binary response variable.}

  \item{data}{The data frame from which the formula should
  be evaluated.}

  \item{xlab}{A string for labelling the x-axis.}

  \item{ylab}{A string for labelling the y-axis.}

  \item{plot.pts}{A logical indicating (\code{TRUE}
  (default)) whether the points should be plotted
  (\code{TRUE}; default) or not (\code{FALSE}).}

  \item{col.pt}{A string used to indicate the color of the
  plotted points. Will be transparent unless
  \code{trans.pt=1}.}

  \item{trans.pt}{A numeric indicating how many points
  would be plotted on top of each other before the
  \sQuote{point} would have the full \code{col.pt} color.
  The reciprocal of this value is the alpha transparency
  value.}

  \item{plot.p}{A logical indicating if the proportion for
  categorized values of X are plotted (\code{TRUE};
  default).}

  \item{breaks}{A number indicating how many intervals over
  which to compute proportions or a numeric vector
  identifying the endpoints of the intervals over which to
  compute proportions if \code{plot.p=TRUE}.}

  \item{p.col}{A color to plot the proportions.}

  \item{p.pch}{A plotting character for plotting the
  proportions.}

  \item{p.cex}{A character expansion factor for plotting
  the proportions.}

  \item{\dots}{Other arguments to be passed to the plot
  functions.}
}
\value{
None.  However, a plot is produced.
}
\description{
A function to plot a binary response variable versus a
quantitative explanatory variable.
}
\details{
This function produces a plot that can be used to visualize
the density of points for a binary response variable as a
function of a quantitative explanatory variable.  In
addition, the proportion of \dQuote{1}s for the response
variable at various \dQuote{levels} of the explanatory
variable are shown.
}
\note{
This function is meant to allow newbie students the ability
to visualize the data corresponding to a binary logistic
regression without getting \dQuote{bogged-down} in the
gritty details of how to produce this plot.
}
\examples{
## NASA space shuttle o-ring failures -- from graphics package
fail <- factor(c(2,2,2,2,1,1,1,1,1,1,2,1,2,1,1,1,1,2,1,1,1,1,1),
levels = 1:2, labels = c("no","yes"))
temperature <- c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)
d <- data.frame(fail,temperature)

## Default plot (using formula notation)
plotBinResp(fail~temperature,data=d)

## Controlling where proportions are computed with a sequence in breaks
plotBinResp(fail~temperature,data=d,breaks=seq(50,85,5))

## Controlling where proportions are computed with an integer in breaks
plotBinResp(fail~temperature,data=d,breaks=10)

## Don't plot points, just plot proportions
plotBinResp(fail~temperature,data=d,plot.pts=FALSE)

## Don't plot proportions, just plot points
plotBinResp(fail~temperature,data=d,plot.p=FALSE)

## Change points colors, and eliminate transparency
plotBinResp(fail~temperature,data=d,col.pt="red",trans.pt=1)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\seealso{
\code{\link{fitPlot}} and \code{\link{cdplot}}.
}
\keyword{hplot}
\keyword{models}

