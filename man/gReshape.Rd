\name{gReshape}
\alias{gReshape}
\title{Reshapes a one-fish-per-line data frame to a one-measurement-per-line data frame.}
\usage{
gReshape(df, in.pre, id.var, var.name = "prvAge", val.name = in.pre,
  last.plus = NULL, na.rm = TRUE, drop = NULL)
}
\arguments{
  \item{df}{A data frame that contains the growth
  measurement data in one-fish-per-line format with specifs
  as defined in the details.}

  \item{in.pre}{A string that represents the common part of
  the measurement variable names.  See details.}

  \item{id.var}{A vector of variables in \code{df} that do
  not change.  See details.}

  \item{var.name}{A string that indicates the name in the
  reshaped data frame that represents the level of the
  measurement variables in the original data frame.}

  \item{val.name}{A string that indicates the name in the
  reshaped data frame that represents the measurements in
  the orginal data frame.}

  \item{last.plus}{A string that if non-null indicates that
  the last measurement represents \dQuote{plus growth} and
  not an actual increment.  If \dQuote{plus growth} is
  recorded then this string should indicate the name of the
  variable in \code{df} that contains the age of the fish
  at capture.}

  \item{na.rm}{A logical that indicates whether \code{NA}
  values in the measurement variables should be removed
  after reshaping.}

  \item{drop}{A vector of variable names to drop before
  reshaping.}
}
\value{
Returns a data frame in one-measurement-per-line format.
See details.
}
\description{
Converts growth data from the one-fish-per-line format to
the one-measurement-per-line format.  One-fish-per-line
format is a common form for collecting or storing growth
data.  One-measurement-per-line format is required for many
statistical analyses.
}
\details{
This function does NOT convert the data from radial to
incremental or incremental to radial measurements (see
\code{\link{gConvert}}).

The input data frame in \code{df} must have the following
specific formats. First, the measurements of annular
increments or radii must be in one-fish-per-line format.
The measurements must be contained in columns that are
named with a common prefix (e.g., \dQuote{anu},
\dQuote{inc}, or \dQuote{rad}) followed by a number that
represents the age of the fish when that portion of the
structure formed.  This prefix must be the same for all
columns that contains measurements and \bold{must not be
found in any other variable (as a prefix or not)}.  For
example, the first annular measurement should be in a
variable named \dQuote{anu1}, the second annular
measurement in \dQuote{anu2}, and so on.  The name of the
prefix should be included in the \code{in.pre=} argument.

If \code{id.var} is left blank then the vector of variables
that will not be changed upon the reshaping will consist of
all variables that do NOT contain the \code{in.pre} prefix.

Errors may occur if a particular variable in the original
data frame, to be included in the \code{id.var=} list, is
of POSIX type.  A workaround for this error is to include
the name of that variable in \code{drop=}.

The name of the variable in the reshaped output data frame
that contains the measurements will be called the same as
\code{in.pre} by default.  This can be changed by including
a new name as a string in \code{val.name}.
}
\examples{
data(SMBassWB)
head(SMBassWB)

# convert radial measurements to increments
SMBi1 <- gConvert(SMBassWB,in.pre="anu",type="inc")
head(SMBi1)

SMBi1a <- gReshape(SMBi1,in.pre="inc")
head(SMBi1a)

# same as above but assume that last increment (in agecap variable) is plus-growth
SMBi2a <- gReshape(SMBi1,in.pre="inc",last.plus="agecap")
head(SMBi2a)

# example of dropping some variables before reshaping
SMBi1b <- gReshape(SMBi1,in.pre="inc",drop=c("species","lake"))
head(SMBi1b)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\seealso{
\code{\link{gConvert}} and \code{\link{reshape}}
}
\keyword{manip}

