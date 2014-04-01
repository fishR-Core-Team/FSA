\name{hoCoef}
\alias{hoCoef}
\title{Performs a hypothesis test that a parameter in a linear model is equal to
some specific value.}
\usage{
hoCoef(lmobj, term = 2, bo = 0, alt = c("two.sided", "less", "greater"))
}
\arguments{
  \item{lmobj}{A \code{lm} object.}

  \item{term}{A number indicating which term in the model
  to use in the hypothesis test.}

  \item{bo}{The null hypothesized parameter value.}

  \item{alt}{A string identifying the "direction" of the
  alternative hypothesis.  See details.}
}
\value{
Returns a matrix containing the term number, hypothesized
value, parameter estimate, standard error of the parameter
estimate, t test statistic, and corresponding p-value.
}
\description{
Performs a hypothesis test that a parameter in a linear
model is equal to some specific value.  Useful for testing
that a parameter is equal to some value other than 0.
}
\details{
The \dQuote{direction} of the alternative hypothesis is
identified by a string in the \code{alt} argument.  The
strings may be \code{"less"} for a \dQuote{less than}
alternative, \code{"greater"} for a \dQuote{greater than}
alternative, or \code{"two.sided"} for a \dQuote{not
equals} alternative (the DEFAULT).

If the \code{lm} object is from a simple linear regression
with an intercept then \code{term=1} will use the intercept
and \code{term=2} will use the slope in the hypothesis
test.
}
\examples{
data(Mirex)
# Simple linear regression test HA:slope!=0.1
lm1 <- lm(mirex~weight, data=Mirex)
hoCoef(lm1,2,0.1)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\seealso{
\code{htest.nlsBoot} in \pkg{FSA}.
}
\keyword{htest}

