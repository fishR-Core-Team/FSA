\name{mrClosed}
\alias{confint.mrClosed}
\alias{mrClosed}
\alias{plot.mrClosed}
\alias{summary.mrClosed}
\title{Estimate initial population size for single or multiple census mark-recapture data.}
\usage{
mrClosed(M, n, m, R, type = c("Petersen", "Chapman", "Ricker", "Bailey",
  "Schnabel", "SchumacherEschmeyer"), labels = NULL, chapman.mod = TRUE)

\method{summary}{mrClosed}(object, digits = 0, incl.SE = FALSE,
  incl.all = FALSE, incl.inputs = FALSE, ...)

\method{confint}{mrClosed}(object, parm = NULL, level = conf.level,
  conf.level = 0.95, digits = 0, ci.type = c("suggested", "binomial",
  "hypergeom", "normal", "Poisson"), bin.type = c("wilson", "exact",
  "asymptotic"), incl.inputs = FALSE, ...)

\method{plot}{mrClosed}(x, pch = 19, col.pt = "black",
  xlab = expression(M[i]), ylab = expression(m[i]\%/\%n[i]),
  loess = FALSE, lty = 2, lwd = 2, col.loess = "red", f = 2/3,
  iter = 5, ...)
}
\arguments{
  \item{M}{A numeric representing the number of marked fish
  from the first sample (single-census), an object from
  \code{capHistSum()} (single- or multiple-census), or
  numeric vector of marked fish prior to ith samples
  (multiple-census).}

  \item{n}{A numeric representing the number of captured
  fish in the second sample (single-census) or numeric
  vector of captured fish in ith sample (multiple-census).}

  \item{m}{A numeric representing the number of recaptured
  (marked) fish in the second sample (single-census) or
  numeric vector of recaptured (marked) fish in ith sample
  (multiple-census).}

  \item{R}{A numeric vector representing the number of
  marked fish returned to the population
  (multiple-census).}

  \item{type}{a string that identifies the type of
  calculation method to use. See details.}

  \item{labels}{A character or character vector used to
  label the rows of the resulting output matrix when using
  a single census method separated by groups.  Must be the
  same length as \code{M}, \code{n}, and \code{m}. Defaults
  to upper-case letters if no values are given.}

  \item{chapman.mod}{A logical that represents whether the
  Chapman modification method should be used (\code{=TRUE},
  default) or not (\code{=FALSE}) when performing the
  Schnabel multiple census method.}

  \item{object}{An object saved from the \code{mrClosed}
  call (i.e., of class \code{mrClosed}).}

  \item{x}{An object saved from the \code{mrClosed} call
  when using \code{type="Schnabel"} or
  \code{type="SchumacherEschmeyer"} (i.e., of class
  \code{mrClosed}).}

  \item{digits}{The number of decimal digits to round the
  population estimates to.  If \code{incl.SE=TRUE} then SE
  will be rounded to one more decimal place then given in
  \code{digits}.}

  \item{incl.SE}{A logical that indicates whether the
  results should include the SE calculation.  See details.}

  \item{incl.all}{A logical that indicates whether an
  overall population estimate should be computed when using
  a single census method that has been separated into
  sub-groups.  See details.}

  \item{incl.inputs}{A logical that indicates whether a
  reminder of the inputted values and what type of method
  was used should be printed with the summary and
  confidence interval results.}

  \item{parm}{a specification of which parameters are to be
  given confidence intervals, either a vector of numbers or
  a vector of names.  If missing, all parameters are
  considered.}

  \item{level}{Same as \code{conf.level} but used for
  compatability with generic \code{confint} function.}

  \item{conf.level}{A numeric representing the level of
  confidence to use for constructing confidence intervals.}

  \item{ci.type}{A string that identifies the type of
  confidence interval to contstruct.  See details.}

  \item{bin.type}{A string that identifies the method used
  to construct binomial confidence intervals (default is
  \code{"wilson"}).  This is only used if
  \code{citype="binomial"}.  See details of \code{binCI}}

  \item{pch}{A numeric used to indicate the type of
  plotting character.}

  \item{col.pt}{a string used to indicate the color of the
  plotted points.}

  \item{xlab}{A label for the x-axis (\code{"Age"} is the
  default).}

  \item{ylab}{A label for the y-axis (\code{"log(Catch)"}
  is the default).}

  \item{loess}{A logical that indicates if a loess smoother
  line is fit to and shown on plot.}

  \item{lty}{a numeric used to indicate the type of line
  used for the loess line.}

  \item{lwd}{a numeric used to indicate the line width of
  the loess line.}

  \item{col.loess}{a string used to indicate the color of
  the loess line.}

  \item{f}{a numeric for the loess smoother span.  This
  gives the proportion of points in the plot which
  influence the smooth at each value.  Larger values give
  more smoothness.}

  \item{iter}{a numeric for the number of
  \dQuote{robustifying} iterations which should be
  performed.  Using smaller values of iter will make lowess
  run faster.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
A list with the following items \itemize{ \item M The
number of marked fish from the first sample that was
provided. \item n The number of captured fish in the second
sample that was provided. \item m The number of recaptured
(marked) fish in the second sample that was provided. \item
M1 The adjusted (depending on \code{type}) number of marked
fish from the first sample. \item n1 The adjusted
(depending on \code{type}) number of captured fish in the
second sample. \item m1 The adjusted (depending on
\code{type}) number of recaptured (marked) fish in the
second sample. \item cf A correction factor for the
population estimate that depends on \code{type}. \item type
The type of method used (provided by the user). \item meth
A label for the type of method used. \item N The estimated
initial population size. \item labels Labels for the rows
of summary matrix. }
}
\description{
This function takes the number of marked animals, number of
captured animals from a second sample, and the number of
marked animals in the second sample and uses one of four
methods to estimate the initial population size with
associated confidence interval.  This function will accept
these data for multiple groups of fish and produce an
overall estimate of the population size.  In addition, this
function takes the number of captured animals, number of
marked animals, and the number of unmarked animals that are
marked and returned to the population on each of a series
of samples and uses one of two methods (Schnabel (1938) or
Schumacher-Eschmeyer (1943)) to estimate the initial
population size with associated confidence interval.  These
methods assume that the population is closed.
}
\details{
The main functions computes estimates of the initial
population size for four possible single census methods
chosen with the \code{type} argument.  These methods are as
follows:

\tabular{ll}{ \code{type="Petersen"} \tab naive
Petersen.\cr \code{type="Chapman"} \tab Chapman(1951)
modification of the Petersen.\cr \code{type="Ricker"} \tab
Ricker(1975) modification of the Chapman modification.\cr
\code{type="Bailey"} \tab Bailey(1951,1952) modification of
the Petersen. }

In addition, two possible multiple census methods can also
be chosen with the \code{type} argument.  These methods are

\tabular{ll}{ \code{type="Schnabel"} \tab Schnabel (1938)
method.\cr \code{type="SchumacherEschmeyer"} \tab
Schumacher and Eschmeyer (1943) method. }

A standard error for the single census population estimate
will be computed if \code{incl.SE=TRUE} is used in
\code{summary}.  This is used primarily for historical
purposes.  The SE for the Petersen method is from
Ricker(1975). The SE for the Chapman method uses the
formula on page 60 of Seber(1982). The SE for the Ricker
method uses formula 3.8 on page 78 of Ricker(1975). The SE
for the Bailey method uses the formula on page 61 of
Seber(1982).  It is advised to use confidence intervals for
the population estimate as described below and implemented
with \code{confint}.

If \code{incl.all=TRUE} in the \code{summary} function and
population estimates have been constructed for multiple
sub-groups then an overall population estimate will be
included by summing the population estimates for the
multiple sub-grops.  In this case, if \code{incl.SE=TRUE},
then an overall SE will be computed by taking the square
root of the summed VARIANCES for the multiple sub-groups.

Confidence intervals for the initial population size
estimated with the single census methods can be constructed
using four different distributions as chosen with the
\code{ci.type} argument.  The hypergeometric method should
be exact (with sampling without replacement) but is
experimental at this point.  The other three types of
confidence intervals are constructed according to Seber
(1982) but using computer algorithms to estimate the
distributions rather than tables and graphs as in Seber
(1982).  If \code{citype="suggested"} then the type of
confidence interval suggested by the rules in Seber (1982)
is used.

If \code{M} contains an object from the \code{capHistSum}
function and one of Petersen, Chapman, Ricker, or Bailey
methods has been chosen then \code{n} and \code{m} can be
left missing or will be ignored.  In this case, the
function will extract the needed data from the \code{sum}
portion of the \code{CapHist} class object.

If \code{M} contains an object from the \code{capHistSum}
function and the Schnabel or Schumacher-Eschmeyer methods
has been chosen then \code{n}, \code{m} and \code{R} can be
left missing or will be ignored.  In this case, the
function will extract the needed data from the \code{sum}
portion of the \code{CapHist} class object.

Confidence intervals for the initial population size using
multiple census methods can be constructed using two
different distributions (normal, Poisson) for the Schnabel
method or the normal distribution for the
Shumacher-Eschmeyer method as chosen with the
\code{ci.type} argument.  The confidence intervals are
constructed according to Seber (1982) but using computer
algorithms to estimate the distributions rather than tables
and graphs as in Seber (1982).  If
\code{citype="suggested"} then the type of confidence
interval suggested by the rules in Seber (1982) is used.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/MRClosed.pdf}
}
\examples{
## Single census with no sub-groups
# Petersen estimate -- the default
mr1 <- mrClosed(346,184,49)
summary(mr1)
confint(mr1)
confint(mr1,ci.type="hypergeom")

# Chapman modification of the Petersen estimate
mr2 <- mrClosed(346,184,49,type="Chapman")
confint(mr2)

# Chapman estimate showing the SE
summary(mr2,incl.SE=TRUE)
summary(mr2,incl.SE=TRUE,digits=1)

## Single census, using capHistSum() results
# data in capture history format
data(BluegillJL)
str(BluegillJL)
ch1 <- capHistSum(BluegillJL)
mr3 <- mrClosed(ch1)
summary(mr3)
confint(mr3)

## Single census with sub-groups
marked <- c(93,35,72,16,46,20)
captured <- c(103,30,73,17,39,18)
recaps <- c(20,23,52,15,35,16)
lbls <- c("YOY","Juvenile","Stock","Quality","Preferred","Memorable")
mr4 <- mrClosed(marked,captured,recaps,type="Ricker",labels=lbls)
summary(mr4)
summary(mr4,incl.SE=TRUE)
summary(mr4,incl.SE=TRUE,incl.all=TRUE)
summary(mr4,incl.SE=TRUE,incl.all=TRUE,incl.inputs=FALSE)
confint(mr4)

## Multiple Census
#Data in summarized form
data(PikeNY)

# Schnabel method
mr5 <- with(PikeNY,mrClosed(n=n,m=m,R=R,type="Schnabel"))
plot(mr5)
summary(mr5)
confint(mr5)

# Schumacher-Eschmeyer method
mr6 <- with(PikeNY,mrClosed(n=n,m=m,R=R,type="Schumacher"))
summary(mr6)
confint(mr6)

## Capture history data summarized by capHistSum()
data(PikeNYPartial1)
ch2 <- capHistSum(PikeNYPartial1,cols=-1)  # ignore first column of ID numbers

# Schnabel method
mr7 <- mrClosed(ch2,type="Schnabel")
plot(mr7,loess=TRUE)
summary(mr7)
confint(mr7)
}
\references{
Krebs, C.J.  1999. Ecological Methodology. Addison-Welsey
Educational Publishing, second edition.

Ricker, W.E.  1975. Computation and interpretation of
biological statistics of fish populations. Technical Report
Bulletin 191, Bulletin of the Fisheries Research Board of
Canada.

Seber, G.A.F. 1982. The Estimation of Animal Abundance.
Edward Arnold, second edition.

Schnabel, Z.E.  1938. The estimation of the total fish
population of a lake. American Mathematician Monthly,
45:348-352.

Schumacher, F.X. and R.W. Eschmeyer. 1943.  The estimation
of fish populations in lakes and ponds.  Journal of the
Tennessee Academy of Sciences, 18:228-249.
}
\seealso{
\code{\link{capHistSum}}, \code{\link{poiCI}},
\code{\link{binCI}}, \code{\link{hyperCI}}
\code{\link{mrOpen}}; \code{\link{mrClosed1Sim}}; and
\code{mrN.single} and \code{schnabel} in \pkg{fishmethods},
}
\keyword{manip}

