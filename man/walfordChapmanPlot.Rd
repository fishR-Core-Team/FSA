\name{walfordPlot}
\alias{chapmanPlot}
\alias{chapmanPlot.default}
\alias{chapmanPlot.formula}
\alias{walfordPlot}
\alias{walfordPlot.default}
\alias{walfordPlot.formula}
\title{Constructs Ford-Walford and Chapman plots for mean length-at-age data.}
\usage{
walfordPlot(age, ...)

\method{walfordPlot}{formula}(age, data = NULL, ...)

\method{walfordPlot}{default}(age, len, xlab = expression(L[t]),
  ylab = expression(L[t + 1]), colLS = "blue", colRL = "red", lwdLS = 2,
  lwdRL = 2, ltyLS = 1, ltyRL = 2, pch = 19, col = "black",
  pchLinf = 9, colLinf = "red", cexLinf = 1.5, showLS = TRUE,
  showVB = TRUE, ...)

chapmanPlot(age, ...)

\method{chapmanPlot}{formula}(age, data = NULL, ...)

\method{chapmanPlot}{default}(age, len, xlab = expression(L[t]),
  ylab = expression(L[t + 1] - L[t]), colLS = "blue", ltyLS = 1,
  lwdLS = 2, pch = 19, col = "black", pchLinf = 9, colLinf = "red",
  cexLinf = 1.5, showLS = TRUE, showVB = TRUE, ...)
}
\arguments{
  \item{age}{Either a numeric vector of lengths ages or a
  formula of the form \code{len~age}.}

  \item{len}{A numeric vector of observed lengths.  Ignored
  if \code{age} is a formula.}

  \item{data}{A data frame in which \code{age} and
  \code{len} can be found.}

  \item{xlab}{A string for labeling the x-axis.}

  \item{ylab}{A string for labeling the y-axis.}

  \item{colLS}{A string indicating the color of the
  least-squares line.}

  \item{colRL}{A string indicating the color of the 1:1
  reference line.}

  \item{lwdLS}{A numeric indicating the line width of the
  least-squares line.}

  \item{lwdRL}{A numeric indicating the line width of the
  1:1 reference line.}

  \item{ltyLS}{A numeric indicating the line type of the
  least-squares line.}

  \item{ltyRL}{A numeric indicating the line type of the
  1:1 reference line.}

  \item{pch}{A numeric indicating the plotting character
  type for the observed data.}

  \item{col}{A string indicating the color for the observed
  data.}

  \item{pchLinf}{A numeric indicating the plotting
  character type indicating the Linf value.}

  \item{colLinf}{A string indicating the color of the point
  indicating the Linf value.}

  \item{cexLinf}{A numeric indicating the character
  expansion factor of the point indicating the Linf value.}

  \item{showLS}{A logical indicating whether the
  coefficients from the least-squares fit should be shown
  on the plot.}

  \item{showVB}{A logical indicating whether the estimate
  of Linf and K should be shown on the plot.}

  \item{\dots}{Other arguments to be passed to the
  \code{plot} function.}
}
\value{
None.  However, a plot is produced.
}
\description{
Constructs Ford-Walford and Chapman plots for mean
length-at-age data and computes estimates of asymptotic
mean length (Linf) and the Brody growth coefficient (K).
}
\examples{
data(SMBassWB)
walfordPlot(lencap~agecap,data=SMBassWB)
chapmanPlot(lencap~agecap,data=SMBassWB)
}
\seealso{
\code{\link{vbStarts}}
}
\keyword{hplot}

