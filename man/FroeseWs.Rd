\name{FroeseWs}
\alias{FroeseWs}
\alias{coef.FroeseWs}
\alias{plot.FroeseWs}
\title{Computes the standard weight equation using the methods described in Froese (2006).}
\usage{
FroeseWs(log.a, b)

\method{coef}{FroeseWs}(object, ...)

\method{plot}{FroeseWs}(x, min, max, what = c("both", "raw", "log"),
  col.pop = "rich", lwd.pop = 1, lty.pop = 1, order.pop = TRUE,
  col.Ws = "black", lwd.Ws = 3, lty.Ws = 1, ...)
}
\arguments{
  \item{log.a}{A numeric vector that contains the
  \eqn{log_{10}(a)} values for the population of
  length-weight regression equations.}

  \item{b}{A numeric vector that contains the b values for
  the population of length-weight regression equations}

  \item{x}{An object saved from the \code{FrowseWs()} call
  (i.e., of class \code{Froese}).}

  \item{min}{A number that indicates the smallest X-mm
  length to model.}

  \item{max}{A number that indicates the midpoint value of
  the largest X-mm length category.}

  \item{object}{An object saved from \code{FroeseWs()}
  (i.e., of class \code{FroeseWs}).}

  \item{what}{A string that indicates the type of plot to
  produce.  See details.}

  \item{col.pop}{A string that indicates the type of color
  or palette to use for the population of length-weight
  regression lines.  See details.}

  \item{order.pop}{A logical that indicates whether the
  populations should be plotted from the smallest to
  largest weight in the initial length category. See
  details.}

  \item{lwd.pop}{A numeric that indicates the width of the
  line to use for the population of length-weight
  regression lines.}

  \item{lty.pop}{A numeric that indicates the type of line
  to use for the population of length-weight regression
  lines.}

  \item{col.Ws}{A string that indicates the type of color
  to use for the standard length-weight regression line.}

  \item{lwd.Ws}{A numeric that indicates the width of the
  line to use for the standard length-weight regression
  line.}

  \item{lty.Ws}{A numeric that indicates the type of line
  to use for the standard length-weight regression line.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
A list is returned with five items.  The first
(\code{log.a}) is a numeric vector of the observed
\eqn{log_{10}(a)} values sent in the \code{log.a} argument.
The second (\code{b}) is a numeric vector of the observed
\eqn{b} values sent in the \code{b} argument.  The third
(\code{gm.a}) is a numeric that contains the geometric mean
of the \eqn{a} parameter.  This is simply the
back-transformed mean \eqn{log_{10}(a)} value -- i.e.,
\eqn{10^{log_{10}(a)}}.  The fourth (\code{mn.b}) is the
arithmetic mean of the \eqn{b} parameter.  The fifth item
(\code{mn.log.a}) is the arithmetic mean of
\eqn{log_{10}(a)}.
}
\description{
Computes the standard weight equation using the geometric
mean of a and the mean of b from weight-length regression
equations as described in Froese (2006).
}
\details{
The main function computes the mean of the
\eqn{log_{10}(a)} and b values for the standard weight
equation as detailed in Froese (2006).  Note that log(a)
and b must be from the regression of \eqn{log_{10}(W)} on
\eqn{log_{10}(L)} where W is measured in grams and L is the
total length measured in mm.

The \code{plot} and \code{coef} methods are used to
construct a plot (see below) and extract the coefficients
of the standard weight equation.  The \code{what} argument
in the \code{plot} method can be set to \code{"both"},
\code{"log"}, or \code{"raw"}.  The \code{"raw"} plot plots
lines on the length-weight scale for each population
represented in the \code{log.a} and \code{b} vectors with
the resultant standard weight equation superimposed in red.
The \code{"log"} plot constructs a similar plot but on the
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
#See examples in RuffeWs in the FSAdata package.
}
\references{
Froese, R.  2006.  Cube law, condition factor and
weight-length relationships: history, meta-analysis and
recommendations.  Journal of Applied Ichthyology
22:241-253.
}
\seealso{
\code{\link{rlp}}, \code{\link{emp}},
\code{\link{wsValidate}}, \code{quantile} in \pkg{stats}
}
\keyword{hplot}
\keyword{manip}

