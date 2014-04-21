\name{lenFreqExpand}
\alias{lenFreqExpand}
\title{Expands a length frequency based on a subsample.}
\usage{
lenFreqExpand(x, w, additional, startcat = NULL, total = additional +
  nrow(df), decimals = decs$wdec, show.summary = TRUE, ...)
}
\arguments{
  \item{x}{A numeric vector of length measurements.}

  \item{startcat}{A number indicating the beginning of the
  first length-class.}

  \item{w}{A number indicating the width of length classes
  to create.}

  \item{additional}{The number of individuals that were not
  measured in the sample (for which measurements should be
  determined).}

  \item{total}{The total number of individuals in the
  sample (including those that were measured in the
  subsample).}

  \item{decimals}{A number indicating the number of
  decimals used in the output vector of estimated lengths.}

  \item{show.summary}{A logical indicating whether a
  summary of the process should be shown at the end.}

  \item{\dots}{Optional arguments to be passed to
  \code{\link{lencat}}.}
}
\value{
Returns a vector that consists of measurements for the
non-measured individuals in the entire sample.
}
\description{
Creates a list of lengths for the individuals not measured
based on the lengths measured in a subsample of
individuals.
}
\details{
This function can take either a vector of observed lengths
or a data.frame in which the variable containing the
lengths is identified.  If a vector is given in \code{df}
then \code{cl} must be set to \code{NULL}.  If a data.frame
is given in \code{df} then \code{cl} must contain a valid
name or number of the column containing the observed
lengths.

The function will create length categories that begin with
the value in \code{startcat} and continue by values of
\code{w} until a category value greater than the largest
observed length in \code{df}.  The code does not allow
categories of different widths.  See \code{\link{lencat}}.

The resulting lengths are created by allocating individuals
to each length class based on the proportion of measured
individuals in the subsample in that length class.
Individuals within a length class are then assigned a
specific length within that length class based on a uniform
distribution. Because the expanded number of individuals in
a length class is rounded down based on the measured number
per length class not all individuals will be assigned a
length value initially.  The remaining individuals are
assigned to a length class randomly according to weights
based on the proportion of individuals in the measured
length classes.  Finally, these individuals are assigned a
specific length within the respective length class in the
same manner as above.

The resulting length assignments are rounded to the number
of decimals shown in \code{decimal}.  If \code{decimals} is
not set by the user then it will default to the same number
of decimals shown in the \code{w} value.  Care is taken to
make sure that the rounded result will not pass out of the
given length category (i.e., will not be allowed to round
up to the next length category).  Generally speaking, one
will want to use more decimals then is shown in \code{w}.
For example, one may want to create length categories with
a width of 1 inch (i.e., \code{w=1}) but have the results
printed as if measured to within 0.1 inch (i.e.,
\code{decimals=1}).
}
\examples{
## First example
# random lengths measured to nearest 0.1 unit -- values in a vector
len1 <- round(runif(50,0.1,9.9),1)
# assignment of integer lengths to 110 non-measured indivs
( new.len1a <- lenFreqExpand(len1,w=1,total=160) )
# assignment of lengths to 0.1 to 110 non-measured indivs
( new.len1b <- lenFreqExpand(len1,w=1,total=160,decimals=1) )

## Second example -- if values are in a data.frame
# random lengths measured to nearest 0.1 unit
len2 <- data.frame(len=round(runif(50,10,117),1))
# assignment of lengths to 0.1 for 140 non-measured indivs
( new.len2a <- lenFreqExpand(len2$len,w=10,total=190,decimals=1) )

## Third example
# hypothetically measured lengths
len <- c(6.7,6.9,7.3,7.4,7.5,8.2,8.7,8.9)
# find lengths for unmeasured fish assuming a total of 30
( newlen1 <- lenFreqExpand(len,w=0.5,total=30,decimals=1) )
# set a starting category
( newlen2 <- lenFreqExpand(len,w=0.5,startcat=6.2,total=30,decimals=1) )
}
\seealso{
\code{\link{lencat}}.
}
\keyword{manip}

