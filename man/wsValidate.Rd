\name{wsValidate}
\alias{anova.empq}
\alias{coef.empq}
\alias{fitPlot.empq}
\alias{plot.empq}
\alias{predict.empq}
\alias{print.willis}
\alias{summary.empq}
\alias{summary.willis}
\alias{wsValidate}
\title{Methods to assess length-bias in a proposed standard weight equation.}
\usage{
wsValidate(object, df, pops, len, wt, min, max, w = 10, type = c("EmpQ",
  "Willis"), n.cutoff = 3, cutoff.tail = TRUE, qtype = 8, probs = 0.75,
  use.means = FALSE, quadratic = TRUE, weighted = FALSE, alpha = 0.05)

\method{print}{willis}(x, ...)

\method{summary}{willis}(object, ...)

\method{summary}{empq}(object, ...)

\method{anova}{empq}(object, ...)

\method{coef}{empq}(object, ...)

\method{predict}{empq}(object, ...)

\method{plot}{empq}(x, pch = 16, col.pt = "black",
  xlab = "Midpoint Length Category", ylab = paste("Standardized", 100 *
  x$probs, "Percentile Mean Weight"), ...)

\method{fitPlot}{empq}(object, pch = 16, col.pt = "black",
  col.mdl = "red", lwd.mdl = 3, lty.mdl = 1,
  xlab = "Midpoint Length Category", ylab = paste("Standardized", 100 *
  object$probs, "Percentile Mean Weight"), main = "EmpQ Method", ...)
}
\arguments{
  \item{object}{An object of class \code{RLP} or \code{EMP}
  returned from calling \code{rlp()} or \code{emp()} in the
  main function and an object of class class \code{empq} or
  \code{willis} (saved from the \code{wsValidate}) in the
  generic functions.}

  \item{df}{A data frame that contains the length-weight
  data for each population.}

  \item{pops}{A string or numeric that indicates which
  column in \code{df} contains the variable that identifies
  the different populations.}

  \item{len}{A string or numeric that indicates which
  column in \code{df} contains the variable with the length
  data.}

  \item{wt}{A string or numeric that indicates which column
  in \code{df} contains the variable with the weight data.}

  \item{min}{A number that indicates the midpoint value of
  the smallest X-mm length category.}

  \item{max}{A number that indicates the midpoint value of
  the largest X-mm length category.}

  \item{w}{A number that indicates the widths for which to
  create length categories.}

  \item{type}{A string that indicates which type of bias
  detection method should be used.}

  \item{n.cutoff}{A numeric that indicates the minimum
  sample size in a length category that should be included
  in the EmpQ regression.  Ignored if
  \code{type="Willis"}.}

  \item{cutoff.tail}{A logical that indicates if all length
  categories larger than the lowest length category with a
  sample size below \code{n.cutoff} should be excluded
  \code{=TRUE} or just those length categories with sample
  sizes lower than \code{n.cutoff}.  Ignored if
  \code{type="Willis"}.}

  \item{qtype}{Type of quantile method to use.  See
  details.  Ignored if \code{use.means=TRUE}.}

  \item{probs}{A number that indicates the probability of
  the quantile.  Must be between 0 and 1.  Ignored if
  \code{use.means=TRUE}.}

  \item{use.means}{A logical that indicates whether mean
  mean weight rather than a quantile mean weight should be
  used in the EmpQ method.}

  \item{quadratic}{A logical that indicates whether a
  quadratic regression should be fit in the EmpQ method.
  Ignored if \code{type="Willis"}.}

  \item{weighted}{A logical that indicates whether the
  regression in the EmpQ method should be weighted by the
  number of populations present in each length category.
  Ignored if \code{type="Willis"}.}

  \item{alpha}{A numeric that indicates the rejection
  criterion to be used in the Willis method.  Ignored if
  \code{type="EmpQ"}.}

  \item{x}{An object saved from the \code{wsValidate} call
  (i.e., of class \code{empq} or \code{willis}).}

  \item{pch}{A single numeric that indicates what plotting
  characther codes should be used for the points in plot or
  fitPlot.}

  \item{col.pt}{A string used to indicate the color of the
  plotted points.}

  \item{xlab}{A label for the x-axis of plot or fitPlot.}

  \item{ylab}{A label for the y-axis of plot or fitPlot.}

  \item{col.mdl}{A string that indicates the type of color
  to use for the standard length-weight regression line.}

  \item{lwd.mdl}{A numeric that indicates the width of the
  line to use for the standard length-weight regression
  line.}

  \item{lty.mdl}{A numeric that indicates the type of line
  to use for the standard length-weight regression line.}

  \item{main}{A label for the main title of fitPlot.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
If \code{type="Willis"} then a list is returned with the
following three items. \itemize{ \item \code{res.ind} is a
data frame that contains the results of the individual
regressions. \item \code{res.tbl}) is the table summarizing
the number of positive and negative significant slopes.
\item \code{res.test}) contains the results for the
chi-square test. }

If \code{type="EmpQ"} then a list is returned with the
following five items: \itemize{ \item \code{n.by.pop}) is a
table of the number of populations represented in each
length category. \item \code{regdata}) is a dataframe used
for the EmpQ regression. \item \code{quadratic}) is a
logical that indicates whether the quadratic regression was
used. \item \code{weighted}) is a logical that indicates
whether a weighted regression was used. \item \code{lm.v}
is the EmpQ regression model results. }
}
\description{
The Willis and empirical quantiles (EmpQ) methods to assess
length-bias in a proposed standard weight equation.
}
\details{
The main function can be used to assess length-bias in a
proposed standard weight equation using either the method
of Willis et al. (1991) (i.e., \code{type="Willis"}) or the
empricial quantiles method of Gerow et al. (2004) (i.e.,
\code{type="EmpQ"}).  The Willis method begins by
regressing the relative weight computed from the candidate
standard weight equation (supplied in \code{object}) for
each individual in a population in the \code{df} data frame
against length.  This is repeated for each population in
\code{df}.  The number of positive and negative slopes from
this regression that are statistically significant are
counted and a chi-square test is used to determine if there
is a statistically equal number of each.  If there is a
statistically equal number of positive and negative
significant slopes then the standard weight equation is
said not to exhibit a length bias.

The EmpQ method is performed by (1) computing the mean
actual weight per \code{w}-mm length category for each
population, (2) computing the given quartile (default is
third) of mean actual weight per length category across all
populations, (3) standardizing the quartile mean weights by
dividing each by the standard weight for the midpoint of
the length categories using the proposed standard weight
equation, and (4) regressing the standardized quartile mean
weights against the length category midpoints.  The
regression can either be quadratic (i.e.,
\code{quadratic=TRUE}) as proposed by Gerow et al. (2004)
or n-weighted (i.e., \code{weighted=TRUE}).  In addition,
length categories with fewer than \code{ncutoff} are
eliminated (see \code{cutoff.tail} description above).  A
slope of zero for the relationship between standardized
quartile mean weights and length category midpoints
indicates that no length-based biases exist with the
proposed standard weight equation.

Types of quantile calculation methods are discussed in the
details of \code{quantile}.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}
}
\examples{
## See examples in rlp(), emp(), and FroeseWs()
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\references{
Gerow, K.G., W.A. Hubert, R.C. Anderson-Sprecher.  2004.
An alternative approach to detection of length-related
biases in standard weight equations.  North American
Journal of Fisheries Management 24:903-910.

Willis, D.W., C.S. Guy, and B.R. Murphy.  1991.
Development and evaluation of the standard weight (Ws)
equation for yellow perch.  North American Journal of
Fisheries Management, 11:374-380.
}
\seealso{
\code{\link{rlp}}, \code{\link{emp}},
\code{\link{FroeseWs}}, \code{quantile} in \pkg{stats}
}
\keyword{manip}

