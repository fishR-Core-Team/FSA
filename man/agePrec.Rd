\name{agePrecision}
\alias{agePrec}
\alias{agePrecision}
\alias{plot.agePrec}
\alias{summary.agePrec}
\title{Computing precision between or among sets of ages.}
\usage{
agePrecision(formula, data)

\method{summary}{agePrec}(object, what = c("precision", "difference",
  "absolute difference", "detail", "agreement"), percent = TRUE, digits = 4,
  ...)
}
\arguments{
  \item{formula}{A formula of the form
  \code{~var1+var2+var3+...} where the \code{varX}
  generically represent the variables that contain the age
  assignments. See details.}

  \item{data}{A data.frame that minimally contains the age
  assignments.  See \code{formula} above and details.}

  \item{object}{An object saved from the
  \code{agePrecision} call (i.e., of class
  \code{agePrec}).}

  \item{what}{A string that indicates what type of summary
  to print.  See details.}

  \item{percent}{A logical indicating whether the agreement
  matrix should contain percentages (\code{TRUE}; default)
  or raw numbers.  See details.}

  \item{digits}{A numeric indicating the minimum number of
  digits to print when using \code{summary}.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
\code{agePrec} returns a list with the following items:
\itemize{ \item detail A data frame with all given and
computed information about each fish.  \item absdiff A
table of number of fish by absolute differences for each
pair of ages.  \item APE The mean average percent error.
\item CV The mean coefficient of variation.  \item n Number
of fish in \code{data}.  \item R Number of age assessments
in \code{data}.  \item PercAgree The percentage of fish for
which all age assignments perfectly agree. }

The \code{summary} function do not return anything.
}
\description{
Computes overall measures of precision for age assignments
made on the same individuals.  The age assignments can
consist of age measurements recorded for two or more
readers of the same structure, one reader at two or more
times, or for two or more stuctures (e.g., scales, spines,
otoliths).
}
\details{
The main function, \code{agePrecision}, requires a formula
of the form \code{~var1+var2+var3+...} where the
\code{varX} generically represent the variables that
contain the age assignments.  Alternatively, the formula
may be of the form \code{var1~var2+var3+...} to allow
similarity with \code{\link{ageBias}}. Note that in this
alternative formula that the RHS can have only one
variable.

If \code{what="precision"} (the default) in \code{summary}
then a summary table will be printed that contains the
following items: \itemize{ \item n Number of fish in
\code{data}.  \item R Number of age assessments in
\code{data}.  \item CV The mean coefficient of variation.
\item APE The mean average percent error.  \item PercAgree
The percentage of fish for which all age assignments
perfectly agree. }

If \code{what="agreement"} in \code{summary} then a table
that describes either the percentage (if
\code{percent=TRUE}, default) or frequency of fish by the
absolute difference in paired age assignments.  Thus, this
table will have one row for each possible pair of age
assignments.  The \dQuote{0} column represents perfect
agreement between the two age assignements, the \dQuote{1}
column represents ages that disagreed by one year (in
either direction), and so on.

If \code{what="detail"} in \code{summary} then a data frame
of the original data plus the intermediate caculations of
the average age, standard deviation of age, APE, and CV for
each individual fish will be printed.  These details are
generally only used to check or to understand calculations.

See \code{\link{ageBias}} for computation of the full age
agreement table, along with tests and plots of age bias.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/AgeComparisons.pdf}
}
\examples{
## Example with just two age assignments
data(WhitefishLC)
ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
summary(ap1)
summary(ap1,what="precision")
summary(ap1,what="difference")
summary(ap1,what="difference",percent=FALSE)
summary(ap1,what="absolute",percent=FALSE)
barplot(ap1$rawdiff,ylab="Frequency",xlab="Otolith - Scale Age")
summary(ap1,what="detail")

## Example with three age assignments
ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
summary(ap2)
summary(ap2,what="precision")
summary(ap2,what="difference")
summary(ap2,what="difference",percent=FALSE)
summary(ap2,what="absolute",percent=FALSE)
summary(ap2,what="detail")
}
\references{
Beamish, R.J. and D.A. Fournier.  1981.  A method for
comparing the precision of a set of age determinations.
Canadian Journal of Fisheries and Aquatic Sciences,
38:982-983.

Campana, S.E.  1982.  Accuracy, precision and quality
control in age determination, including a review of the use
and abuse of age validation methods. Journal of Fish
Biology, 59:197-242.

Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.
Graphical and statistical methods for determining the
consistency of age determinations. Transactions of the
American Fisheries Society, 124:131-138.

Chang, W.Y.B. 1982.  A statistical method for evaluating
the reproducibility of age determination.  Canadian Journal
of Fisheries and Aquatic Sciences, 39:1208-1210.
}
\seealso{
\code{\link{ageBias}}
}
\keyword{htest}
\keyword{manip}

