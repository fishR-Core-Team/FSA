\name{chapmanRobson}
\alias{chapmanRobson}
\alias{chapmanRobson.default}
\alias{chapmanRobson.formula}
\alias{confint.chapmanRobson}
\alias{plot.chapmanRobson}
\alias{summary.chapmanRobson}
\title{Computes Chapman-Robson estimates of S and Z.}
\usage{
chapmanRobson(x, ...)

\method{chapmanRobson}{formula}(x, data, ages2use = age,
  zmethod = c("Hoenigetal", "original"), ...)

\method{chapmanRobson}{default}(x, catch, ages2use = age,
  zmethod = c("Hoenigetal", "original"), ...)

\method{plot}{chapmanRobson}(x, pos.est = "bottomleft", ylab = "Catch",
  xlab = "Age", col.pt = "black", ...)

\method{summary}{chapmanRobson}(object, ...)

\method{confint}{chapmanRobson}(object, parm = c("all", "both", "S", "Z"),
  level = conf.level, conf.level = 0.95, ...)
}
\arguments{
  \item{x}{A numerical vector of the assigned ages in the
  catch curve or a formula of the form \code{catch~age}
  when used in \code{chapmanRobson}.  An object saved from
  \code{chapmanRobson} (i.e., of class
  \code{chapmanRobson}) when used in the methods.}

  \item{object}{An object saved from the
  \code{chapmanRobson} call (i.e., of class
  \code{chapmanRobson}).}

  \item{catch}{A numerical vector of the catches or CPUEs
  for the ages in the catch curve.  Not used if \code{x} is
  a formula.}

  \item{data}{A data frame from which the variables in the
  \code{x} formula can be found.  Not used if \code{x} is
  not a formula.}

  \item{ages2use}{A numerical vector of the ages that
  define the descending limb of the catch curve.}

  \item{zmethod}{A string that indicates the method to use
  for estimating Z.  See details.}

  \item{pos.est}{A string to identify where to place the
  estimated mortality rates on the graph.  Can be set to
  one of \code{"bottomright"}, \code{"bottom"},
  \code{"bottomleft"}, \code{"left"}, \code{"topleft"},
  \code{"top"}, \code{"topright"}, \code{"right"} or
  \code{"center"} for positioning the estimated mortality
  rates on the plot.  Typically \code{"bottomleft"}
  (DEFAULT) and \code{"topright"} will be
  \dQuote{out-of-the-way} placements.  Set \code{pos.est}
  to \code{NULL} to remove the estimated mortality rates
  from the plot.}

  \item{ylab}{A label for the y-axis (\code{"Catch"} is the
  default).}

  \item{xlab}{A label for the x-axis (\code{"Age"} is the
  default).}

  \item{col.pt}{a string that indicates the color of the
  plotted points.}

  \item{parm}{A numeric or string (of parameter names)
  vector that specifies which parameters are to be given
  confidence intervals If missing, all parameters are
  considered.}

  \item{conf.level}{A number representing the level of
  confidence to use for constructing confidence intervals.}

  \item{level}{Same as \code{conf.level}.  Used for
  compatability with the generic \code{confint} function.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
A list with the following items: \itemize{ \item age the
original vector of assigned ages. \item catch the original
vector of observed catches or CPUEs. \item age.e a vector
of assigned ages used to estimate mortalities. \item
catch.e a vector of catches or CPUEs used to estimate
mortalities. \item age.r a vector of recoded ages used to
estimate mortalities.  See references. \item n a numeric
holding the intermediate calcualtion of n.  See references.
\item T a numeric holding the intermediate calcualtion of
T.  See references. \item est A 2x2 matrix that contains
the estimates and standard errors for S and Z. }
}
\description{
Computes the Chapman-Robson estimates of annual survival
rate (S) and instantaneous mortality rate (Z), along with
associated standard errors, from catch-at-age data on the
descending limb of a catch-curve.
}
\details{
The default is to use all ages in the age vector.  This is
only appropriate if the age and catch vector contain only
the ages and catches on the descending limb of the catch
curve.  Use \code{ages2use} to isolate only the catch and
ages on the descending limb.

The Chapman-Robson method provides an estimate of the
annual survival rate, with the annual mortality rate (A)
determined by 1-S.  The instantaneous mortality rate is
often computed as -log(S).  However, Hoenig et al. (1983)
showed that this produced a biased (over)estimate of Z and
provided a correction. The correction is applied by setting
\code{zmethod="Hoenigetal"} (which is the default
behavior).
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/CatchCurve.pdf}
}
\examples{
data(BrookTroutTH)
cr <- with(BrookTroutTH,chapmanRobson(age,catch,2:6))
summary(cr)
confint(cr)

## demonstration of formula notation
cr2 <- chapmanRobson(catch~age,BrookTroutTH,2:6)
summary(cr2)

## demonstration of ability to work with missing age classes
age <- c(  2, 3, 4, 5, 7, 9,12)
ct  <- c(100,92,83,71,56,35, 1)
cr3 <- chapmanRobson(age,ct,4:12)
summary(cr3)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\references{
D.G. Chapman and D.S. Robson. 1960. The analysis of a catch
curve. Biometrics. 16:354-368.

D.S. Robson and D.G. Chapman. 1961. Catch curves and
mortality rates. Transactions of the American Fisheries
Society. 90:181-189.

Hoenig, J.M. and W.D. Lawing, and N.A. Hoenig.  1983.
Useing mean age, mean length and median length data to
estimate the total mortality rate.  International Council
for the Exploration of the Sea, CM 1983/D:23, Copenhagen.
}
\seealso{
\code{\link{catchCurve}}
}
\keyword{htest}
\keyword{manip}

