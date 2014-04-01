\name{lagratio}
\alias{lagratio}
\title{Ratio of lagged observations.}
\usage{
lagratio(x, lag = 1L, differences = 1L, ...)
}
\arguments{
  \item{x}{A numeric vector or matrix.}

  \item{lag}{An integer representing the lag
  \sQuote{distance}.}

  \item{differences}{An integer describing the direction of
  calculation.  A \code{1} indicates that \sQuote{future}
  values are divided by \sQuote{past} values.  A \code{2}
  is the opposite; \sQuote{past} values are divided by
  \sQuote{future} values.  See examples.}

  \item{\dots}{Additional arguments to \code{diff()}.}
}
\value{
A vector or matrix of lagged ratios.
}
\description{
Computes the ratio of lagged observations in a vector.
}
\details{
This function behaves similarly to \code{diff()} except
that it returns a vector or matrix of ratios rather than
differences.
}
\examples{
## same examples as in diff()
lagratio(1:10, 2)
lagratio(1:10, 2, 2)
x <- cumsum(cumsum(1:10))
lagratio(x, lag = 2)
lagratio(x, differences = 2)

## the following will produce an error because POSIXt objects don't support logs
##   which lagratio uses
\dontrun{
lagratio(.leap.seconds)
}
}
\seealso{
\code{diff}
}
\keyword{manip}

