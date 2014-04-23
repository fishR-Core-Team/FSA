\name{ageKey}
\alias{ageKey}
\title{Applies an age-length key to individuals in a length-sample.}
\usage{
ageKey(key, formula, data, type = c("SR", "CR"), len.breaks = NULL)
}
\arguments{
  \item{key}{A numeric matrix that contains the age-length
  key.}

  \item{formula}{A formula of the form \code{age~length}
  where \dQuote{age} generically represents a variable in
  \code{data} that will contain the estimated age
  measurements once the key is applied (i.e., currently
  missing) and \dQuote{length} generically represents a
  variable in \code{data} that contains known length
  measurements.  It is possible to use \code{~length} in
  which case a new variable called \dQuote{age} will be
  created in the resulting data frame.}

  \item{data}{A data.frame that minimally contains the
  length measurements and possibly contains a variable that
  will receive the age estimates.  See description for
  \code{formula}.}

  \item{type}{A string that indicates whether to use the
  semi-random (\code{type="SR"}, default) or
  completely-random (\code{type="CR"}) techniques for
  assigning ages to individual fish.}

  \item{len.breaks}{A numeric vector of lower values for
  the break points of the length categories.}
}
\value{
The original data frame in \code{data} is returned with
assigned ages added to the column supplied in
\code{formula} (see details) or in an additional column
labeled as \code{age}.
}
\description{
This function uses either the semi- or completely-random
methods to assign ages to individual fish in a
length-sample according to the information in a supplied
age-length key.  The methods generally follow the algorithm
outlined by Isermann and Knight (2005).
}
\details{
The age-length key sent in \code{key} must be constructed
with length intervals as rows and age values as columns.
The row names of \code{key} (i.e., \code{rownames(key)})
must contain the mininum values of each length interval
(e.g., if an interval is 100-109 then the corresponding row
name must be 100).  The column names of \code{key} (i.e.,
\code{colnames(key)}) must contain the age values (e.g.,
the columns can NOT be named with \dQuote{age.1}, for
example).

The length intervals in the rows of the age-length key sent
in \code{key} must contain all of the length intervals
present in the length sample to which the age-length key is
to be applied (i.e., sent in the \dQuote{length} portion of
the \code{formula}).  If this constraint is not met then
the function will stop with an error message.

If \code{len.breaks=NULL} then the length categories for
the length sample are determined with a starting category
as the minimum value of the row names and a width of the
length interval categories as determined by the minimum
difference in adjacent row names of the age-length key
matrix.  If length categories of differing widths were used
in construction of the age-length key then the breaks used
should be supplied to \code{len.breaks=}.  Use of the
\code{len.breaks=} argument may be useful when
\dQuote{uneven} width length categories must be used
because the lengths in the length sample are not fully
represented in the age sample if \dQuote{narrower} widths
or intervals are used. See the examples below.

Assigned ages will be stored in the column identified in
the left-hand-side of \code{formula} (if the formula has
both a left- and right-hand-side.  If this variable is
missing in the formula then the new column wil be labeled
with \code{age}.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/AgeLengthKey.pdf}
}
\examples{
## Get data with length measurements and some assigned ages
data(WR79)
# create the age and length samples from the data frame
WR.age <- Subset(WR79, !is.na(age))
WR.len <- Subset(WR79, is.na(age))

## First Example -- Even breaks for length categories
# find smallest length in age sample -- create appropriate length categories
summary(WR.age$len)
WR.age.mod <- lencat(~len,data=WR.age,startcat=35,w=5,drop.levels=TRUE)
# create age-length key
raw <- table(WR.age.mod$LCat, WR.age.mod$age)
( WR.key <- prop.table(raw, margin=1) )
# apply age-length key to length sample
WR.len.mod <- ageKey(WR.key,age~len,data=WR.len)
head(WR.len.mod)
# combine original age sample with new length sample and summarize
WR.comb <- rbind(WR.age, WR.len.mod)
Summarize(len~age,data=WR.comb,digits=2)

## Second Example -- Uneven breaks for length categories
# create breaks as 35-40, 40-45, ..., 95-100, 100-110, 110-130
brks <- c(seq(35,100,5),110,130)
WR.age.mod <- lencat(~len,data=WR.age,breaks=brks,drop.levels=TRUE)
# create age-length key
raw <- table(WR.age.mod$LCat, WR.age.mod$age)
( WR.key <- prop.table(raw, margin=1) )
# apply age-length key to length sample
WR.len.mod <- ageKey(WR.key,age~len,data=WR.len,len.breaks=brks)
head(WR.len.mod)
# combine original age sample with new length sample and summarize
WR.comb <- rbind(WR.age, WR.len.mod)
Summarize(len~age,data=WR.comb,digits=2)

## Third Example -- length sample does not have an age variable (checking purposes only)
WR.len1 <- WR.len[,-3]
# apply age-length key to length sample
WR.len1.mod <- ageKey(WR.key,~len,data=WR.len1)
head(WR.len1.mod)
}
\references{
Isermann, D.A. and C.T. Knight.  2005.  A Computer Program
for Age-Length Keys Incorporating Age Assignment to
Individual Fish.  North American Journal of Fisheries
Management, 25:1153-1160.
}
\seealso{
\code{alk}, \code{alkprop}, and \code{alkss} in
\pkg{fishmethods} and the entire \pkg{ALKr} package.
}
\keyword{manip}

