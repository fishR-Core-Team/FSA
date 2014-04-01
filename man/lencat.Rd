\name{lencat}
\alias{lencat}
\title{Constructs length class/category variable.}
\usage{
lencat(formula, data, startcat = 0, w = 1, breaks = NULL, right = FALSE,
  vname = NULL, as.fact = TRUE, drop.levels = FALSE,
  use.catnames = FALSE)
}
\arguments{
  \item{formula}{A formula of the form \code{~length} where
  \dQuote{length} generically represents a variable in
  \code{data} that contains length measurements.  Note that
  this formula can only contain one variable.}

  \item{data}{A data.frame that minimally contains the
  length measurements given in the variable in the
  \code{formula}.}

  \item{startcat}{A number indicating the beginning of the
  first length-class.}

  \item{w}{A number indicating the width of length classes
  to create.}

  \item{breaks}{A numeric vector of lower values for the
  break points of the length categories.}

  \item{right}{A logical indicating if the intervals should
  be closed on the right (and open on the left) or vice
  versa.}

  \item{vname}{A string containing the name for the new
  length class variable.}

  \item{as.fact}{A logical indicating if the new variable
  should be returned as a factor (\code{=TRUE}; default) or
  not.}

  \item{drop.levels}{A logical indicating if the new
  variable should retain all levels indicated in
  \code{breaks} (\code{=FALSE}; default) or not.  This is
  ignored if \code{as.fact=FALSE}.}

  \item{use.catnames}{A logical indicating whether the
  names for the values in \code{breaks} should be used for
  the levels in the new variable.  Will throw a warning and
  then use default levels if \code{TRUE} but
  \code{names(breaks)} is \code{NULL}.}
}
\value{
Returns a data frame that consists of the original data
frame, \code{data}, with the new length category variable
appended and named as \code{vname}.
}
\description{
Constructs a new variable that contains the length class or
category that an individual fish belongs to and appends
that new variable to the original data.frame.
}
\details{
If \code{breaks=NULL} (the default) then this function will
create length categories that begin with the value in
\code{startcat} and continue by values of \code{w} until a
category value greater than the largest observation in
\code{data[,cl]}.  In this instance categories of different
widths are not allowed.  The length categorizations are
left-inclusive and right-exclusive by default (i.e.,
\code{right=FALSE}).  The number in the \code{startcat}
argument should be less than the smallest value in
\code{data[,cl]}.  In addition the number of decimals in
\code{startcat} should not be more than the number of
decimals in \code{w}.  For example, \code{startcat=0.4} and
\code{w=1} will result in the function being terminated
with an error.

If \code{breaks} is non-NULL then \code{startcat} and
\code{w} will be ignored.  In this instance the vector of
values in \code{breaks} should begin with a value at least
as small as the minimum observation and end with a value at
least as big as the maximum observation.  If the lowest
break value is larger than the minimum observation then the
function will stop with an error.  If the largest break is
smaller than the maximum observation then an additional
break larger than the maximum observation will be added to
breaks. The use of \code{breaks} allows the use of length
categories of different widths.

The observed values in the \code{data[,cl]} should be
rounded to the appropriate number of decimals to avoid
misplacement of individuals into incorrect length
categories due to machine-precision issues.  For example,
see discussion in \code{all.equal} function.

If no variable name is supplied in \code{vname} then the
default variable name will be \code{LCat}.
}
\examples{
# random lengths measured to nearest 0.1 unit
df1 <- data.frame(len=round(runif(50,0.1,9.9),1))

# length categories by 0.1 unit starting at 0
df1a <- lencat(~len,data=df1,startcat=0,w=0.1)
table(df1a$LCat)

# length categories by 0.2 units starting at 0
df1b <- lencat(~len,data=df1,startcat=0,w=0.2)
table(df1b$LCat)

# length categories by 0.2 units starting at 0.1
df1c <- lencat(~len,data=df1,startcat=0.1,w=0.2)
table(df1c$LCat)

# length categories as set by breaks
df1d <- lencat(~len,data=df1,breaks=c(0,2,4,7,10))
table(df1d$LCat)


## A Second example
# random lengths measured to nearest unit
df2 <- data.frame(len=round(runif(50,10,117),0))

# length categories by 5 units starting at 0
df2a <- lencat(~len,data=df2,startcat=0,w=5)
table(df2a$LCat)

# length categories by 5 units starting at 7
df2b <- lencat(~len,data=df2,startcat=7,w=5)
table(df2b$LCat)

# length categories by 10 units starting at 5
df2c <- lencat(~len,data=df2,startcat=5,w=10)
table(df2c$LCat)

# length categories as set by breaks
df2d <- lencat(~len,data=df2,breaks=c(5,50,75,150))
table(df2d$LCat)


## A Third example
# random lengths measured to nearest 0.1 unit
df3 <- data.frame(len=round(runif(50,10,117),1))

# length categories by 5 units starting at 0
df3a <- lencat(~len,data=df3,startcat=0,w=5)
table(df3a$LCat)


## A Fourth example
# random lengths measured to nearest 0.01 unit
df4 <- data.frame(len=round(runif(50,0.1,9.9),2))

# length categories by 0.1 unit starting at 0
df4a <- lencat(~len,data=df4,startcat=0,w=0.1)
table(df4a$LCat)

# length categories by 2 unit starting at 0
df4b <- lencat(~len,data=df4,startcat=0,w=2)
table(df4b$LCat)


## A Fifth example -- with real data
data(SMBassWB)
#remove variables with "anu" and "radcap" just for simplicity
smb1 <- SMBassWB[,-c(8:20)]

# summarize to help find a good starting category
summary(smb1$lencap)

# 10 mm length classes - in default LCat variable
smb1 <- lencat(~lencap,data=smb1,startcat=50,w=10)
view(smb1)

# 25 mm length classes - in custom variable name
smb1 <- lencat(~lencap,data=smb1,startcat=50,w=25,vname="LenCat25")
view(smb1)

# using values from psdVal for Smallmouth Bass
smb1 <- lencat(~lencap,data=smb1,breaks=psdVal("Smallmouth Bass"),vname="LenPsd")
view(smb1)
# add category names
smb1 <- lencat(~lencap,data=smb1,breaks=psdVal("Smallmouth Bass"),vname="LenPsd2",use.catnames=TRUE)
view(smb1)
}
\keyword{manip}

