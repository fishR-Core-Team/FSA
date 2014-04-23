\name{wsVal}
\alias{wsVal}
\title{Finds standard weight equation coefficients for a particular species.}
\usage{
wsVal(species = "List", units = c("metric", "English"), ref = 75,
  simplify = FALSE)
}
\arguments{
  \item{species}{A string that contains the species name
  for which to find coefficients.}

  \item{units}{A string that indicates whether the
  coefficients for the (\code{"metric"} (DEFAULT; mm and g)
  or \code{"English"} (in and lbs) units should be
  returned.}

  \item{ref}{A numeric that indicates which quantile the
  equation should be returned for.  Note that the vast
  majority of equations only exist for the \code{75}th
  percentile (DEFAULT).}

  \item{simplify}{A logical that indicates whether the
  \sQuote{units}, \sQuote{ref}, \sQuote{measure},
  \sQuote{method}, \sQuote{comments}, and \sQuote{source}
  fields should be included (\code{=FALSE}) or not
  (\code{=TRUE}; DEFAULT).}
}
\value{
A data frame that consists of one line from \code{WSlit}
related to the standard weight equation for the given
species, units, and ref.
}
\description{
Returns a vector with the standard weight coefficients from
the WSlit data frame for a given species and type of
measurement units.
}
\details{
This function finds the coefficients for the given species
names in the \code{data(WSlit)} data frame.  The
coefficients are the intercept, linear, and, if
appropriate, quadratic terms from the model

\deqn{log_{10}(Ws) = log_{10}(a) + blog_{10}(L) +
blog_{10}(L)^{2}}

Thus, to obtain the standard weight (Ws) from the returned
equation, one must use the equation to first compute the
common log of Ws and then raise this to the power of 10 to
compute the Ws.

If no arguments are given to this function or if a species
name does not exist in the database then a list of species
will be printed.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}
}
\examples{
wsVal()
wsVal("Bluegill")
wsVal("Bluegill",units="metric")
wsVal("Bluegill",units="English")
wsVal("Bluegill",units="English",simplify=TRUE)
wsVal("Ruffe",units="metric",simplify=TRUE)
wsVal("Ruffe",units="metric",ref=50,simplify=TRUE)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\seealso{
\code{\link{wrAdd}} and \code{\link{WSlit}}
}
\keyword{manip}

