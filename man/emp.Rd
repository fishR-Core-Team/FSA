\name{emp}
\alias{anova.emplm}
\alias{coef.emp}
\alias{emp}
\alias{fitPlot.emplm}
\alias{fitPlot.emprq}
\alias{plot.emplm}
\alias{predict.emp}
\alias{residPlot.emplm}
\alias{summary.emp}
\title{Computes the standard weight equation using the emperical-percentile method.}
\usage{
emp(df, pops, len, wt, min, max, w = 10, n.cutoff = 3, cutoff.tail = TRUE,
  qtype = 8, probs = 0.75, method = c("lm", "rq"), quadratic = TRUE)

\method{coef}{emp}(object, ...)

\method{summary}{emp}(object, ...)

\method{predict}{emp}(object, ...)

\method{anova}{emplm}(object, ...)

\method{plot}{emplm}(x, pch = 16, col.pop = "rich", col.Ws = "black",
  lwd.Ws = 3, lty.Ws = 1, jitterX = TRUE, jitter.factor = 3, ...)

\method{fitPlot}{emplm}(object, pch = 16, col.pt = "black",
  col.Ws = "red", lwd.Ws = 3, lty.Ws = 1, xlab = "log10(midpt Length)",
  ylab = paste(100 * object$probs, "Percentile of mean log10(Weight)"),
  main = "EMP Equation Fit", ...)

\method{residPlot}{emplm}(object, ...)

\method{fitPlot}{emprq}(object, pch = 16, col.pop = "rich",
  col.Ws = "black", lwd.Ws = 3, lty.Ws = 1, jitterX = TRUE,
  jitter.factor = 3, ...)
}
\arguments{
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

  \item{n.cutoff}{A numeric that indicates the minimum
  sample size in a length category that should be included
  in the regression.}

  \item{cutoff.tail}{A logical that indicates if all length
  categories larger than the lowest length category with a
  sample size below \code{n.cutoff} should be excluded
  \code{=TRUE} or just those length categories with sample
  sizes lower than \code{n.cutoff}.}

  \item{qtype}{Type of quantile method to use.  See
  details.}

  \item{probs}{A number that indicates the probability of
  the quantile.  Must be between 0 and 1.}

  \item{method}{A string that indicates whether a linear
  regression (\code{lm} or quantile regression (\code{rq})
  should be used to construct the standard weight equation.
  See details.}

  \item{quadratic}{A logical that indicates whether a
  quadratic regression should be fit (\code{=TRUE} or not.}

  \item{x,object}{An object saved from the \code{emp} call
  (i.e., of class \code{emp}).}

  \item{pch}{A single numeric that indicates what plotting
  characther codes should be used for the points in plot or
  fitPlot.}

  \item{col.pop}{A string that indicates the type of color
  or palette to use for the population of length-weight
  regression lines.  See details.}

  \item{col.Ws}{A string that indicates the type of color
  to use for the standard length-weight regression line.}

  \item{lwd.Ws}{A numeric that indicates the width of the
  line to use for the standard length-weight regression
  line.}

  \item{lty.Ws}{A numeric that indicates the type of line
  to use for the standard length-weight regression line.}

  \item{jitterX}{A logical that indicates whether the x
  values in plot should be jittered.}

  \item{jitter.factor}{A numeric that indicates the
  relative magnitude of the jittering in x (sent to
  \code{factor} argument in \code{jitter()}.}

  \item{col.pt}{A string used to indicate the color of the
  plotted points.}

  \item{xlab}{A label for the x-axis of fitPlot.}

  \item{ylab}{A label for the y-axis of fitPlot.}

  \item{main}{A label for the main title of fitPlot.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
A list is returned with the following seven items:
\itemize{ \item \code{pop.by.len} is a table of the number
of populations represented in each length category. \item
\code{ind.by.len} is a table of the number of individual
fish in each length category. \item \code{ind.by.len.pop}
is a table of the number of individual fish in each length
category for each population. \item \code{mnlwt.by.len.pop}
is a table of the mean log10 weight for each length
category for each population. \item \code{regdata} is a
dataframe used for the Ws regression (i.e., third quartiles
of mean log10 weight and length category midpoints). \item
\code{quadratic} is a logical that indicates whether the
quadratic regression was used. \item \code{Ws} is the Ws
regression model results. }
}
\description{
Computes the standard weight equation using the
empirical-percentile method when given populations of
length-weight data.
}
\details{
The main function follows the steps of the empirical
percentile method detailed in Gerow et al. (2005).  In
general, the mean log10 weight for each population within
all length categories is computed, length categories with
fewer than \code{ncutoff} are eliminated (see
\code{cutoff.tail} description above), the third quartile
of mean log10 weights for the remaining categories are
found, and a n-weighted regression (quadratic regression if
\code{quadratic=TRUE}) is then fit to the third quartile of
mean log10 weights and the length category midpoint value.

Gerow et al. (2005) suggest using a quantile definition
that is basically the same as \code{qtype=9}.  Types of
quantile calculation methods are discussed in the details
of of \code{quantile}.

The \code{plot}, \code{coef}, and \code{summary} methods
are used to construct a plot (see below), extract the
coefficients of the standard weight equation, and find
summary results of the \code{lm} object returned by the
main function.  The \code{plot} method plots the mean log10
weights versus length category midpoint for each population
represented in the data frame with the resultant standard
weight equation superimposed in black.

If the \code{col.pop} argument is set equal to one of these
palettes -- \dQuote{rich}, \dQuote{cm}, \dQuote{default},
\dQuote{grey}, \dQuote{gray}, \dQuote{heat}, \dQuote{jet},
\dQuote{rainbow}, \dQuote{topo}, or \dQuote{terrain} -- and
the \code{order.pop=TRUE} then the populations plotted
should form a general color gradient from smallest to
largest weight in the initial length category.  This will
make it easier to identify populations that \dQuote{cross
over} other populations.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}
}
\examples{
## Walleye Ws equation for comparison to Gerow's Excel Tool
# Gerow's results were -- -4.624269, 2.477718, and 0.1461490 for the intercept, linear term,
#    and quadratic term for 75th percentile Ws equation
# Gerow's results were -- -4.950281, 2.698470, and 0.1052352 for the intercept, linear term,
#    and quadratic term for 50th percentile Ws equation
data(WalleyeGerowLW)
# compare to Ws75 results
wae1 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE)
coef(wae1)
# compare to Ws75 results -- uses same quantile type?
wae2 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,qtype=9,cutoff.tail=FALSE)
coef(wae2)
# compare to Ws50 results; note use of all length categories
wae1a <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,n.cutoff=1,probs=0.5)
coef(wae1a)
# compare to Ws50 results -- uses same quantile type?
wae2a <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,qtype=9,n.cutoff=1,probs=0.5)
coef(wae2a)

# It appears that a difference from Gerow's work is that the quantiles computed
#   here are different than his quantiles.  This can be seen by comparing the
#   regdata results with the results in his 'summarized' worksheet.  From
#   Gerow et al. (2005) it appears that he used 'qtype=9'; however, 'qtype=5'
#   provides the closest values to his Excel worksheet.
wae1$regdata

## use quantile regression method
wae1rq <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE,method="rq")
coef(wae1rq)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\references{
Gerow, K.G., R.C. Anderson-Sprecher, and W.A. Hubert.
2005.  A new method to compute standard-weight equations
that reduces length-related bias.  North American Journal
of Fisheries Management 25:1288-1300.
}
\seealso{
\code{\link{rlp}}, \code{\link{FroeseWs}},
\code{\link{wsValidate}}, \code{quantile} in \pkg{stats}
}
\keyword{hplot}
\keyword{manip}

