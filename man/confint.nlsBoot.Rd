\name{confint.nlsBoot}
\alias{confint.nlsBoot}
\alias{confint.nlsboot}
\title{Construct a confidence interval from nlsBoot results.}
\usage{
\method{confint}{nlsBoot}(object, parm = NULL, level = 0.95,
  conf.level = level, plot = FALSE,
  rows = round(sqrt(ncol(object$coefboot))),
  cols = ceiling(sqrt(ncol(object$coefboot))), ...)
}
\arguments{
  \item{object}{An object saved from \code{nlsBoot()}.}

  \item{parm}{An integer that indicates which parameter to
  compute the confidence interval for.  Will compute for
  all parameters if \code{NULL}.}

  \item{conf.level}{A level of confidence as a proportion.}

  \item{level}{Same as \code{conf.level}.  Used for
  compatability with the main \code{confint}.}

  \item{plot}{A logical that indicates whether a histogram
  of the \code{parm} parameters from the bootstrap samples
  with error bars that illustrate the bootstrapped
  confidence intervals should be constructed.}

  \item{rows}{A numeric that contains the number of rows to
  use on the graphic.}

  \item{cols}{A numeric that contains the number of columns
  to use on the graphic.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
A matrix with as many rows as columns (i.e., parameter
estimates) in the \code{object$coefboot} data frame and two
columns of the quantiles that correspond to the approximate
confidence interval.
}
\description{
Constructs a non-parametric bootstrap confidence interval
from an object of \code{nlsBoot()} (in the \pkg{nlstools}
package) results.
}
\details{
This function finds the two quantiles that have the
proportion (1-\code{conf.level})/2 of the bootstrapped
parameter estimates below and above.  This is an
approximate 100\code{conf.level}\% confidence interval.
}
\examples{
## Not run, because nlsBoot takes a great deal of time:
## Should work if copied and pasted into console
\dontrun{
require(nlstools)
example(nlsBoot)
confint(boo,conf.level=0.90)
}
}
\seealso{
\code{\link{predict.nlsBoot}}, \code{summary.nlsBoot} in
\pkg{nlstools}
}
\keyword{htest}

