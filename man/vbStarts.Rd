\name{vbStarts}
\alias{vbStarts}
\alias{vbStarts.default}
\alias{vbStarts.formula}
\title{Finds reasonable starting values for the parameters in a specific von
Bertalanffy model parameterization.}
\usage{
vbStarts(age, ...)

\method{vbStarts}{formula}(age, data = NULL, ...)

\method{vbStarts}{default}(age, len, type = c("typical", "original",
  "BevertonHolt", "Francis", "GallucciQuinn", "Mooij", "Schnute", "Somers",
  "Somers2", "vonBertalanffy"), tFrancis = NULL, methEV = c("poly",
  "means"), meth0 = c("poly", "yngAge"), plot = FALSE, ...)
}
\arguments{
  \item{age}{Either a vector of observed ages or a formula
  of the form \code{len~age}.}

  \item{len}{A vector of observed lengths.}

  \item{data}{A data frame from which the vectors of
  observed ages and lengths can be found if a formula is
  used.}

  \item{type}{A string that indicates the parameterization
  of the von Bertalanffy #'model.}

  \item{tFrancis}{A numerical vector of the three ages to
  be used in the Francis paramaterization.  See details.}

  \item{methEV}{A string that indicates how the lengths of
  the two ages in the Schnute paramaterization or the three
  ages in the Francis paramaterization should be derived.
  See details.}

  \item{meth0}{A string that indicates how the t0 and L0
  paramaters should be derived.  See details.}

  \item{plot}{A logical that indicates whether a plot of
  the data with the model fit at the starting values
  superimposed should be created.}

  \item{\dots}{Further arguments passed to the methods.}
}
\value{
A list that contains reasonable starting values.  Note that
the parameters will be listed in the same order and with
the same names as listed in \code{\link{vbFuns}}.
}
\description{
Finds reasonable starting values for the parameters in a
specific von Bertalanffy model parameterization.
}
\details{
This function attempts to find reasonable starting values
for a variety of parameterizations of the von Bertalanffy
growth model.  There is no guarantee that these starting
values are the \sQuote{best} starting values.  One should
use them with caution and should perform sensitivity
analyses to determine the impact of different starting
values on the final model results.

The Linf and K paramaters are estimated via the common
method of a Ford-Walford plot.  The product of the starting
values for Linf and K is used as a starting value for omega
in the Gallucci and Quinn and Mooij et al. methods.

If \code{meth0="yngAge"} then a starting value for t0 is
found by algebraically solving the typical paramaterization
model for t0 using the first age with more than one data
point.  If \code{meth0="poly"} then a starting value for t0
is set equal to the root of the second-degree polynomial
model fit to the mean length-at-age data that is closest to
zero.

If \code{meth0="yngAge"} then a starting value for L0 is
found by algebraically solving the original
paramaterization model for t0 using the first age with more
than one data point.  If \code{meth0="poly"} then a
starting value for L0 is set equal to the mean length at
age-0 predicted from the second-degree polynomial model fit
to the mean length-at-age data.

Starting values for the L1 and L2 parameters in the Schnute
paramaterization and the L1, L2, and L3 parameters in the
Francis parameterization can be found in two ways.  If
\code{methEV="poly"} then the mean lengths are predicted
from a second-degree polynomial fit to the mean
length-at-age data. If \code{methEV="means"} then the
observed sample means at the corresponding ages are used.
In the case where one of the supplied ages is fractional
then the value returned will be linear interpolated between
the mean lengths of the two closest ages.  Note that the
Schnute method always uses the minimum and maximum ages to
define L1 and L2, respectively, whereas the ages used for
the Francis method must be supplied as a numerical vector
of length three in \code{tFrancis}.  An error will occur if
\code{methEV="means"} and \code{tFrancis=NULL}.  In
addition, a warning will be given if L2<L1 for the Schnute
method or if L2<L1 or L3<L2 for the Francis method.
}
\note{
The \sQuote{original} and \sQuote{vonBertalanffy} and the
\sQuote{typical} and \sQuote{BevertonHolt} versions are
synonomous.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffy.pdf},
  \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffyExtra.pdf}
}
\examples{
## Simple Examples
data(SpotVA1)
vbStarts(tl~age,data=SpotVA1)
vbStarts(tl~age,data=SpotVA1,type="Francis",tFrancis=c(0,5))

## Simple Example with a Plot
vbStarts(tl~age,data=SpotVA1,type="GallucciQuinn",plot=TRUE)

## See examples in vbFuns() for use of vbStarts() when fitting Von B models
}
\references{
Francis, R.I.C.C.  1988.  Are growth parameters estimated
from tagging and age-length data comparable?  Canadian
Journal of Fisheries and Aquatic Sciences, 45:936-942.

Gallucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing,
fitting, and testing a simple growth model.  Transactions
of the American Fisheries Society, 108:14-25.

Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.
Analysis and comparison of fish growth from small samples
of length-at-age data: Detection of sequal dimorphism in
Eurasian perch as an example.  Transactions of the American
Fisheries Society 128:483-490.

Schnute, J.  1981.  A versatile growth model with
statistically stable parameters. Canadian Journal of
Fisheries & Aquatic Sciences, 38:1128-1140.

Somers, I. F. 1988. On a seasonally oscillating growth
function. Fishbyte 6(1):8-11.
}
\seealso{
\code{\link{growthModels}}, \code{\link{vbFuns}}, and
\code{\link{growthModelSim}}
}
\keyword{manip}

