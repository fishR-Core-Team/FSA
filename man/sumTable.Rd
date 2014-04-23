\name{sumTable}
\alias{sumTable}
\alias{sumTable.formula}
\title{Creates a one- or two-way table of summary statistics.}
\usage{
sumTable(formula, ...)

\method{sumTable}{formula}(formula, data = NULL, FUN = mean,
  digits = getOption("digits"), ...)
}
\arguments{
  \item{formula}{A formula with a quantitative variable on
  the left-hand-side and one or two factor variables on the
  right-hand-side.}

  \item{data}{An optional data frame that contains the
  variables in the formula.}

  \item{FUN}{A scalar function to compute the summary
  statistics which can be applied to the quantitative
  variable for all data subsets identified by the
  combination of the factor(s).  Defaults to \code{mean}.}

  \item{digits}{A single numeric that indicates the number
  of digits to be used for the result.}

  \item{\dots}{Other arguments to pass through to
  \code{FUN}.}
}
\value{
A one-way array of values if only one factor variable is
supplied on the right-hand-side of the formula.  A two-way
matrix of values if two factor variables are supplied on
the right-hand-side.
}
\description{
Creates a one- or two-way table of summary statistics for a
quantitative variable.
}
\details{
The formula must be of the form \code{quantitative~factor}
or \code{quantitative~factor*factor2} where
\code{quantitative} is the quantitative variable to
construct the summaries for and \code{factor} or
\code{factor2} are factor variables that contain the levels
for which separate summaries should be constructed.  If the
variables on the right-hand-side are not factors then they
will be coerced to be factors and a warning will be issued.

This function currently only works for one quantitative
variable on the left-hand-side and one or two factor
variales on the right-hand-side.  Consider using
\code{tapply} for situations with more factors on the
right-hand-side.
}
\note{
This function is largely a wrapper to \code{tapply()}.
}
\examples{
## The same examples as in the old aggregate.table in gdata package
g1 <- sample(letters[1:5], 1000, replace=TRUE)
g2 <- sample(LETTERS[1:3], 1000, replace=TRUE )
dat <- rnorm(1000)
d <- data.frame(g1,g2,dat)   # put data into a data.frame to illustrate formula notation

sumTable(dat~g1*g2,data=d,FUN=length)       # get sample size
sumTable(dat~g1*g2,data=d,FUN=mean)         # get mean
sumTable(dat~g1*g2,data=d,FUN=sd)           # get sd
sumTable(dat~g1*g2,data=d,FUN=sd,digits=1)  # show digits= argument

## Also demonstrate use in the 1-way example -- but see Summarize() in FSA package
sumTable(dat~g1,data=d,FUN=length)
sumTable(dat~g1,data=d,FUN=mean)
}
\seealso{
\code{tapply}.
}
\keyword{hplot}

