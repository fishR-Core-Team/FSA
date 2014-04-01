\name{rlp}
\alias{anova.rlp}
\alias{coef.rlp}
\alias{fitPlot.rlp}
\alias{plot.rlp}
\alias{predict.rlp}
\alias{residPlot.rlp}
\alias{rlp}
\alias{summary.rlp}
\title{Computes the standard weight equation using the regression-line-percentile method.}
\usage{
rlp(log.a, b, min, max, w = 10, qtype = 8, probs = 0.75, digits = NULL)

\method{plot}{rlp}(x, what = c("both", "raw", "log"), col.pop = "rich",
  lwd.pop = 1, lty.pop = 1, order.pop = TRUE, col.Ws = "black",
  lwd.Ws = 3, lty.Ws = 1, ...)

\method{anova}{rlp}(object, ...)

\method{coef}{rlp}(object, ...)

\method{predict}{rlp}(object, ...)

\method{summary}{rlp}(object, ...)

\method{fitPlot}{rlp}(object, pch = 16, col.pt = "black", col.Ws = "red",
  lwd.Ws = 3, lty.Ws = 1, xlab = "log10(midpt Length)",
  ylab = paste("log10(", 100 * object$prob,
  " Percentile of Predicted Weight)", sep = ""), main = "RLP Equation Fit",
  ...)

\method{residPlot}{rlp}(object, ...)
}
\arguments{
  \item{log.a}{A numeric vector containing the
  \eqn{log_{10}(a)} values for the population of
  length-weight regression equations.}

  \item{b}{A numeric vector containing the b values for the
  population of length-weight regression equations}

  \item{min}{A number indicating the midpoint value of the
  smallest X-mm length category.}

  \item{max}{A number indicating the midpoint value of the
  largest X-mm length category.}

  \item{w}{A number indicating the widths for which to
  create length categories.}

  \item{qtype}{Type of quantile method to use.  See
  details.}

  \item{probs}{A number indicating the probability of the
  quantile.  Must be between 0 and 1.}

  \item{digits}{Number of digits to round predicted
  weights.}

  \item{x}{An object saved from the \code{rlp} call (i.e.,
  of class \code{rlp}).}

  \item{object}{An object saved from \code{rlp()} or
  \code{emp()} (i.e., of class \code{rlp}) for the
  \code{anova}, \code{coef}, and \code{summary}
  functions..}

  \item{what}{A string indicating the type of plot to
  produce.  See details.}

  \item{col.pop}{A string indicating the type of color or
  palette to use for the population of length-weight
  regression lines.  See details.}

  \item{order.pop}{A logical indicating whether the
  populations should be plotted from the smallest to
  largest weight in the initial length category.  See
  details.}

  \item{lwd.pop}{A numeric indicating the width of the line
  to use for the population of length-weight regression
  lines.}

  \item{lty.pop}{A numeric indicating the type of line to
  use for the population of length-weight regression
  lines.}

  \item{col.Ws}{A string indicating the type of color to
  use for the standard length-weight regression line.}

  \item{lwd.Ws}{A numeric indicating the width of the line
  to use for the standard length-weight regression line.}

  \item{lty.Ws}{A numeric indicating the type of line to
  use for the standard length-weight regression line.}

  \item{pch}{A single numeric indicating what plotting
  characther codes should be used for the points in the
  fitPlot.}

  \item{col.pt}{A string used to indicate the color of the
  plotted points.}

  \item{xlab}{A label for the x-axis of fitPlot.}

  \item{ylab}{A label for the y-axis of fitPlot.}

  \item{main}{A label for the main title of fitPlot.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
A list is returned with the following items: \itemize{
\item \code{log.a} is a numeric vector of the observed
\eqn{log_{10}(a)} values sent in the \code{log.a} argument.
\item \code{b} is a numeric vector of the observed \eqn{b}
values sent in the \code{b} argument. \item
\code{data.pred} is a matrix of the predicted weight at
length for all populations. \item \code{data.reg} contains
a data frame with the \code{prob}th quartile of predicted
weights and the midpoint lengths. \item \code{Ws} is an
\code{lm} object containing the results of the regression
of \eqn{log_{10}(wq)} on \eqn{log_{10}(midpoint length)}. }
}
\description{
Computes the standard weight equation using the
regression-line-percentile method when given the log(a) and
b values for a population of length-weight regression
equations.
}
\details{
The main function follows the steps of the
regression-line-percentile method detailed in Murphy et al.
(1990).  In summary, a predicted weight is constructed for
each 1-cm length class from each population from the given
\eqn{log_{10}(a)} and \eqn{b} values, the predicted weight
at the \code{prob}th percentile (wq) is identified, and a
linear regression equation is fit to the \eqn{log_{10}(wq)}
and \eqn{log_{10}(midpoint length)} data.

Note that \eqn{log_{10}(a)} and \eqn{b} must be from the
regression of \eqn{log_{10}(W)} on \eqn{log_{10}(L)} where
W is measured in grams and L is the total length measured
in mm.

It appears that Murphy et al. (1990) used \code{qtype=6} in
their SAS program.  Types of quantile calculation methods
are discussed in the details of of \code{quantile}.

The \code{plot}, \code{coef}, and \code{summary} methods
are used to construct a plot (see below), extract the
coefficients of the standard weight equation, and find
summary results of the \code{lm} object returned by the
main function.  The \code{what} argument in the \code{plot}
method can be set to \code{"both"}, \code{"log"}, or
\code{"raw"}.  The \code{"raw"} plot plots lines on the
length-weight scale for each population represented in the
\code{log.a} and \code{b} vectors with the resultant
standard weight equation superimposed in red.  The
\code{"log"} plot constructs a similar plot but on the
\eqn{log_{10}(weight)}-\eqn{log_{10}(length)} scale.  The
\code{"both"} option produces both plots side-by-side.

If the \code{col.pop} argument is set equal to one of these
palettes -- \dQuote{rich}, \dQuote{cm}, \dQuote{default},
\dQuote{grey}, \dQuote{gray}, \dQuote{heat}, \dQuote{jet},
\dQuote{rainbow}, \dQuote{topo}, or \dQuote{terrain} -- and
the \code{order.pop=TRUE} then the populations plotted
should form a general color gradient from smallest to
largest weight in the initial length category.  This will
make it easier to identify populations that \dQuote{cross
over} other populations.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}
}
\examples{
## Recreate Murphy et al. (1990) results for largemouth bass
# min and max lengths were 152 and 816
# compare to log.a=-5.379 and b=3.221
data(LMBassWs)
lmb.rlp <- rlp(LMBassWs$log.a,LMBassWs$b,155,815,qtype=6)
coef(lmb.rlp)
plot(lmb.rlp)
fitPlot(lmb.rlp)
residPlot(lmb.rlp)
}
\references{
Murphy, B.R., M.L. Brown, and T.A. Springer.  1990.
Evaluation of the relative weight (Wr) index, with new
applications to walleye.  North American Journal of
Fisheries Management, 10:85-97.
}
\seealso{
\code{\link{emp}}, \code{\link{FroeseWs}},
\code{\link{wsValidate}}, \code{quantile} in \pkg{stats}
}
\keyword{hplot}
\keyword{manip}

