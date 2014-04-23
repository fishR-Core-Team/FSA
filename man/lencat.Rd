\name{lencat}
\alias{lencat}
\alias{lencat.default}
\alias{lencat.formula}
\title{Constructs length class/category variable.}
\usage{
lencat(x, ...)

\method{lencat}{default}(x, w = 1, breaks = NULL, startcat = NULL,
  right = FALSE, use.names = FALSE, as.fact = FALSE,
  drop.levels = FALSE, ...)

\method{lencat}{formula}(x, data, w = 1, breaks = NULL, startcat = NULL,
  right = FALSE, use.names = FALSE, as.fact = FALSE,
  drop.levels = FALSE, vname = NULL, ...)
}
\arguments{
  \item{x}{A numeric vector that contains the length
  measurements or a formula of the form \code{~x} where
  \dQuote{x} generically represents a variable in
  \code{data} that contains length measurements.  This
  formula can only contain one variable.}

  \item{data}{A data.frame that minimally contains the
  length measurements given in the variable in the
  \code{formula}.}

  \item{w}{A single numeric that indicates the width of
  length categories to create.}

  \item{breaks}{A numeric vector of lower values for the
  break points of the length categories.}

  \item{startcat}{A single numeric that indicates the
  beginning of the first length category.}

  \item{right}{A logical that indicates if the intervals
  should be closed on the right (and open on the left) or
  vice versa.}

  \item{use.names}{A logical that indicates whether the
  names for the values in \code{breaks} should be used for
  the levels in the new variable.  Will throw a warning and
  then use default levels if \code{TRUE} but
  \code{names(breaks)} is \code{NULL}.}

  \item{as.fact}{A logical that indicates that the new
  variable should be returned as a factor (\code{=TRUE};
  default) or not.}

  \item{drop.levels}{A logical that indicates that the new
  variable should retain all levels indicated in
  \code{breaks} (\code{=FALSE}; default) or not.  Ignored
  if \code{as.fact=FALSE}.}

  \item{vname}{A string that contains the name for the new
  length class variable.}

  \item{\dots}{Not implemented.}
}
\value{
If the formula version of the function is used then a data
frame will be returned with the a new variable, named as in
\code{vname} or \code{LCat} if no name is given by the
user, appended to the original data frame.  If the default
version of the function is used then a single vector will
be returned.  The returned values will be numeric unless
\code{breaks} is named and \code{use.names=TRUE} or if
\code{as.fact=TRUE}.
}
\description{
Constructs a vector that contains the length class or
category to which an individual belongs.  Optionally, that
vector can be appened to the original data frame.
}
\details{
If \code{breaks} is non-NULL then \code{w} and
\code{startcat} will be ignored. The vector of values in
\code{breaks} should begin with a value smaller than the
minimum observed value and end with a value larger than the
maximum observed value.  If the lowest break value is
larger than the minimum observed value then an error will
occur.  If the largest break value is smaller than the
maximum observed value then an additional break value
larger than the maximum observed value will be added to
\code{breaks} (and a warning will be sent).  The values in
\code{breaks} do not have to be equall spaced.

If \code{breaks=NULL} (the default) then the value in
\code{w} is used to create equally spaced categories.  The
start of the length categories can be set with
\code{startcat}.  However, if \code{startcat=NULL} (the
default) the the length categories will begin with the
first value less than the minimum observed value when
\dQuote{rounded} by \code{w}.  For example, if the minimum
observed value is 67 then the first length category will be
65 if \code{w=5}, 60 if \code{w=10}, 50 if \code{w=25}, and
50 if \code{w=50}.  The length categories will continue
from this starting value by values of \code{w} until a
category value is greater than the largest observed value
in \code{v}.  The length categories are left-inclusive and
right-exclusive by default (i.e., \code{right=FALSE}).  The
number in the \code{startcat} argument should be less than
the smallest value in \code{v}. Additionally, the number of
decimals in \code{startcat} should not be more than the
number of decimals in \code{w} (e.g., \code{startcat=0.4}
and \code{w=1} will result in an error).

One may want to convert apparent numeric values to factor
values if some of the length categories are missing (e.g.,
if factor values are used, for example, then tables of the
length categories values will have values for all length
categories; i.e., it will have zeroes for the length
categories that are missing). The numeric values can be
converted to factors by including \code{as.fact}. See the
\dQuote{real data} example.

The observed values in the \code{v} should be rounded to
the appropriate number of decimals to avoid misplacement of
individuals into incorrect length categories due to issues
with machine-precision (see discussion in
\code{all.equal}.)
}
\examples{
# Create random lengths measured to nearest 0.1 unit
df1 <- data.frame(len=round(runif(50,0.1,9.9),1))

# Create length categories by 0.1 unit
df1$LCat1 <- lencat(df1$len,w=0.1)
table(df1$LCat1)

# length categories by 0.2 units
df1$LCat2 <- lencat(df1$len,w=0.2)
table(df1$LCat2)

# length categories by 0.2 units starting at 0.1
df1$LCat3 <- lencat(df1$len,w=0.2,startcat=0.1)
table(df1$LCat3)

# length categories as set by breaks
df1$LCat4 <- lencat(df1$len,breaks=c(0,2,4,7,10))
table(df1$LCat4)


## A Second example
# random lengths measured to nearest unit
df2 <- data.frame(len=round(runif(50,10,117),0))

# length categories by 5 units
df2$LCat1 <- lencat(df2$len,w=5)
table(df2$LCat1)

# length categories by 5 units starting at 7
df2$LCat2 <- lencat(df2$len,w=5,startcat=7)
table(df2$LCat2)

# length categories by 10 units
df2$LCat3 <- lencat(df2$len,w=10)
table(df2$LCat3)

# length categories by 10 units starting at 5
df2$LCat4 <- lencat(df2$len,w=10,startcat=5)
table(df2$LCat4)

# length categories as set by breaks
df2$LCat5 <- lencat(df2$len,breaks=c(5,50,75,150))
table(df2$LCat5)


## A Third example
# random lengths measured to nearest 0.1 unit
df3 <- data.frame(len=round(runif(50,10,117),1))

# length categories by 5 units
df3$LCat1 <- lencat(df3$len,w=5)
table(df3$LCat1)


## A Fourth example
# random lengths measured to nearest 0.01 unit
df4 <- data.frame(len=round(runif(50,0.1,9.9),2))

# length categories by 0.1 unit
df4$LCat1 <- lencat(df4$len,w=0.1)
table(df4$LCat1)

# length categories by 2 unit
df4$LCat2 <- lencat(df4$len,w=2)
table(df4$LCat2)


## A Fifth example -- with real data
data(SMBassWB)
#remove variables with "anu" and "radcap" just for simplicity
smb1 <- smb2 <- SMBassWB[,-c(8:20)]

# 10 mm length classes - in default LCat variable
smb1$LCat10 <- lencat(smb1$lencap,w=10)
view(smb1)
table(smb1$LCat10)

# Same as previous but returned as a factor so that levels with no fish are still seen
smb1$LCat10A <- lencat(smb1$lencap,w=10,as.fact=TRUE)
view(smb1)
table(smb1$LCat10A)

# Same as previous but returned as a factor with unused levels dropped
smb1$LCat10B <- lencat(smb1$lencap,w=10,as.fact=TRUE,drop.levels=TRUE)
view(smb1)
table(smb1$LCat10B)

# 25 mm length classes - in custom variable name
smb1$LCat25 <- lencat(smb1$lencap,w=25)
view(smb1)
table(smb1$LCat25)

# using values from psdVal for Smallmouth Bass
smb1$PSDCat1 <- lencat(smb1$lencap,breaks=psdVal("Smallmouth Bass"))
view(smb1)
table(smb1$PSDCat1)

# add category names
smb1$PSDCat2 <- lencat(smb1$lencap,breaks=psdVal("Smallmouth Bass"),use.names=TRUE)
view(smb1)
table(smb1$PSDCat2)

# same as above but drop the unused levels
smb1$PSDCat2A <- lencat(smb1$lencap,breaks=psdVal("Smallmouth Bass"),
                       use.names=TRUE,drop.levels=TRUE)
view(smb1)
table(smb1$PSDCat2A)

str(smb1)

## A Sixth example -- similar to the fifth example but using the formula notation
# 10 mm length classes - in default LCat variable
smb2 <- lencat(~lencap,data=smb2,w=10)
view(smb2)

# 25 mm length classes - in custom variable name
smb2 <- lencat(~lencap,data=smb2,w=25,vname="LenCat25")
view(smb2)

# using values from psdVal for Smallmouth Bass
smb2 <- lencat(~lencap,data=smb2,breaks=psdVal("Smallmouth Bass"),vname="LenPsd")
view(smb2)

# add category names
smb2 <- lencat(~lencap,data=smb2,breaks=psdVal("Smallmouth Bass"),vname="LenPsd2",
              use.names=TRUE,drop.levels=TRUE)
view(smb2)

str(smb2)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\keyword{manip}

