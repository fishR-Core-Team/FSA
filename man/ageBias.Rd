\name{ageBias}
\alias{ageBias}
\alias{plot.ageBias}
\alias{summary.ageBias}
\title{Viewing and computing bias between paired sets of ages.}
\usage{
ageBias(formula, data, col.lab = tmp$Rname, row.lab = tmp$Enames[1],
  method = p.adjust.methods, sig.level = 0.05, min.n.CI = 5)

\method{plot}{ageBias}(x, what = c("bias", "sunflower", "numbers"),
  difference = FALSE, xlab = x$col.lab, ylab = x$row.lab, show.n = TRUE,
  nYpos = 1.1, show.pts = FALSE, pch.pts = 19, col.pts = rgb(0, 0, 0,
  transparency), transparency = 1/10, pch.mean = 3, col.err = "blue",
  col.err.sig = "red", lwd.err = 2, show.rng = FALSE, col.rng = "gray",
  lwd.rng = 2, col.ref = "black", lwd.ref = 1, lty.ref = 2,
  xlim = NULL, ylim = NULL, yaxt = par("yaxt"), ...)

\method{summary}{ageBias}(object, what = c("table", "symmetry", "Bowkers",
  "EvansHoenig", "McNemars", "bias", "diff.bias", "n"), flip.table = FALSE,
  zero.print = "-", digits = 3, cont.corr = c("none", "Yates", "Edwards"),
  ...)
}
\arguments{
  \item{formula}{A formula of the form \code{col~row},
  where \code{col} and \code{row} generically represent the
  variables that contain the ages that will form the
  columns and rows, respectively, of the age-agreement
  table.  See details.}

  \item{data}{A data.frame that minimally contains the
  paired age assignments. See description for
  \code{formula} and details.}

  \item{col.lab}{A string that contains a label for the
  column age assignments.}

  \item{row.lab}{A string that contains a label for the row
  age assignments.}

  \item{method}{A string that indicates which method to use
  when adjusting p-values for multiple comparisons.}

  \item{sig.level}{A value to be used for determining
  whether a p-value suggests a significant result or not.
  The confidence level used in \code{plot} is
  1-\code{sig.level}.}

  \item{min.n.CI}{A value that indicates the smallest
  sample size for which a confidence interval should be
  computed.  Default is 5.}

  \item{object}{An object saved from the \code{ageBias}
  call (i.e., of class \code{ageBias}).}

  \item{x}{An object saved from the \code{ageBias} call
  (i.e., of class \code{ageBias}).}

  \item{what}{A string that indicates what type of summary
  to print or plot to construct.  See details.}

  \item{difference}{A logical that indicates whether or not
  the difference between the two ageing structures should
  be used or not.  See details.}

  \item{zero.print}{A string that indicates what should be
  printed in place of the zeroes on an age-agreement table.
  The default is to print a single dash.}

  \item{digits}{A numeric that indicates the minimum number
  of digits to print when showing \code{what="bias"} or
  \code{what="diff.bias"} in \code{summary}.}

  \item{flip.table}{A logical that indicates whether the
  age-agreement table should be \sQuote{flipped} (i.e.,
  rows are reversed so that the younger ages are at the
  bottom of the table).  This makes the table more directly
  comparable to the age-bias plot.}

  \item{cont.corr}{A continuity correction method to be
  used with (only) McNemars test. If \code{"none"}
  (default) then no continuity correction is used, if
  \code{"Yates"} then 0.5 is used, and if \code{"Edwards"}
  then 1 is used.}

  \item{xlab}{A string that contains a label for the x-axis
  age assignments.}

  \item{ylab}{A string that contains a label for the y-axis
  age assignments.}

  \item{show.n}{A logical that indicates whether the sample
  sizes for each level of the x-axis variable is shown
  (\code{=TRUE}, default) or not (\code{=FALSE}).}

  \item{nYpos}{A numeric that indicates the relative Y
  position of the sample size values when
  \code{show.n=TRUE}.  For example, if \code{nYpos=1.1}
  then the sample size values will be 10 percent above the
  end of the y-axis.}

  \item{show.pts}{A logical that indicates whether to show
  the raw data points or not.  See \code{col.pts} below.}

  \item{show.rng}{A logical that indicates whether to show
  vertical bars that represent the range of the data points
  or not.  See \code{col.rng} below.}

  \item{pch.mean}{A value that indicates the plotting
  character to be used for mean values (i.e., center of
  confidence interval bars).}

  \item{col.err}{A string or value that indicates the color
  to be used for confidence interval bars that are
  considered non-significant.}

  \item{col.err.sig}{A string or value that indicates the
  color to be used for confidence interval bars that are
  considered significant.}

  \item{lwd.err}{A value that indicates the line width for
  the confidence interval bars.}

  \item{pch.pts}{A value that indicates the plotting
  character to be used when plotting the raw data points.}

  \item{col.pts}{A string or value that indicates the color
  to be used for plotting the raw data points.  The default
  is to use black with a transparency found in
  \code{transparency}.}

  \item{transparency}{A value (between 0 and 1) that
  indicates the level of transparency to use for plotting
  the raw data points.  If expressed as a fraction of 1/x
  then x points plotted on top of each other will represent
  the color in \code{col.pts}.}

  \item{col.rng}{A string or value that indicates the color
  to be used for the interval representing the range of the
  data.}

  \item{lwd.rng}{A value that indicates the line width for
  the interval representing the range of the data.}

  \item{col.ref}{A value or string that indicates the color
  for the 1:1 or zero (if difference) reference line.}

  \item{lwd.ref}{A value that indicates the line width for
  the 1:1 or zero (if difference) reference line.}

  \item{lty.ref}{A value that indicates the line type for
  the 1:1 or zero (if difference) reference line.}

  \item{xlim}{A numeric vector of the limits of the
  x-axis.}

  \item{ylim}{A numeric vector of the limits of the
  y-axis.}

  \item{yaxt}{A string which specifies the x-axis type.
  Specifying \dQuote{n} suppresses plotting of the axis.
  See \sQuote{?par}.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
\code{ageBias} returns a list with the following items:
\itemize{ \item data A data frame with the original two age
assignments and the difference between those two age
assignements.  \item agree The age-agreement table.  \item
bias A data.frame that contains the bias statistics.  \item
bias.diff A data.frame that contains the bias statistics
for the differences.  \item col.lab A string that contains
an optional label for the column structure or readings.
\item row.lab A string that contains an optional label for
the row structure or readings. }

The \code{summary} function will return a data frame that
contains the symmetry test results if
\code{what="symmetry"}, \code{what="Bowkers"},
\code{what="McNemars"}, or \code{what="EvansHoenig"}.  The
\code{plot} function do not return anything.
}
\description{
Computes overall measures of bias and creates plots for
visualizing bias in paired age assignments.  The paired age
assignments can consist of age measurements recorded for
two structures (e.g., otoliths and scales), for two readers
of the same structure, or for one reader at two times.
}
\details{
The main function, \code{ageBias}, requires a formula of
the form \code{col~row}, where \code{col} and \code{row}
generically represent the variables that contain the ages
that will form the columns and rows, respectively, of the
age- agreement table.  If one age assignment is thought to
be more accurate than the other, then it should form the
columns and should be on the left-hand-side of the formula.
The variable that forms the columns in the age-agreement
table will be the \dQuote{constant} age used in the t-tests
(see \code{summary}) and age-bias plots (i.e,. the x-axis
in \code{plot}).

The results can be shown as an age-bias plot, as defined by
Campana et al. (1995), by using \code{what="bias"} (the
default) in \code{plot}.  The variable that formed the
columns in the original \code{ageBias} call is plotted on
the x-axis.  Confidence intervals plotted in red are
computed for the mean of the y-axis data at each
\dQuote{level} (i.e., age) of the x-axis data.  The level
of confidence interval is controlled by \code{sig.level}
given in the original \code{ageBias} call. Vertical lines
that connect the minimum to the maximum value of the y-axis
data at each \dQuote{level} of the x-axis data are plotted
in grey if \code{show.range=TRUE}. A 1:1 (45 degree)
agreement line is shown for comparative purposes.  The
sample sizes at each \dQuote{level} of the x-axis data is
shown if \code{show.n=TRUE} (the default).  The age-bias
plot can be modified, following Muir et al. (2008), with
\code{difference=TRUE} so that the y-axis data is the
difference in the paired ages in the rows and columns from
the original \code{ageBias} call (specifically,
columns-rows).

The data can also be viewed as a sunflower plot by using
\code{what="sunflower"} in \code{plot}.  A sunflower plot
contains a point for each unique (x,y) coordinate with a
\dQuote{petal} added to each point for each observation
that has the same (x,y) coordinate.  Thus, points with more
petals have more observations at that point.  The
differences between the two structures can be shown by
including \code{difference=TRUE}.

The data can also be viewed as a \dQuote{numbers} plot by
using \code{what="numbers"} in \code{plot}.  A
\dQuote{numbers} plot shows the number of observations that
occur at each unique (x,y) coordinate.

The age-agreement table can be seen with
\code{what="table"} in \code{summary}. The agreement table
can be flipped by including \code{flip.table=TRUE}.  The
results from one of three tests of symmetry (along with the
age-agreement table) are given with \code{what="Bowkers"}
for Bowker's test (described in Hoenig et al. (1995)),
\code{what="EvansHoenig"} for Evans-Hoenig test (described
in Evans and Hoenig (1998)), or \code{what="McNemars"} for
McNemar's test (described in Evans and Hoenig (1998)) in
\code{summary}.  The null hypothesis for these tests is
that the agreement table is symmetric.  The results from
all three tests of symmetry can be seen with
\code{what="symmetry"}.

Individual t-tests to determine if the mean row-variables
ages at a particular age of the column-variable (e.g., is
the mean row-variable age at column-variable age-3
statistically equal to 3?) are constructed with
\code{what="bias"} in \code{summary}.  The results provide
a column that indicates whether the difference is
significant or not as determined by adjusted p-value from
the t-test and using the signficance level provided in
\code{sig.level} (defaults to 0.05).  Similar results for
the difference in ages (e.g., is the mean row-variable age
minus column variable age at column-variable age-3 equal to
0?) can be constructed by including \code{what="diff.bias"}
in \code{summary}.

The sample size present in the age-agreement table is found
with \code{what="n"}.

See \code{\link{agePrecision}} for measures of precision
between pairs of age assignments.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/AgeComparisons.pdf}
}
\examples{
data(WhitefishLC)
ab1 <- ageBias(otolithC~scaleC,data=WhitefishLC,col.lab="Otolith Age",row.lab="Scale Age")
summary(ab1)
summary(ab1,what="symmetry")
summary(ab1,what="Bowkers")
summary(ab1,what="EvansHoenig")
summary(ab1,what="McNemars")
summary(ab1,what="McNemars",cont.corr="Yates")
summary(ab1,what="bias")
summary(ab1,what="diff.bias")
summary(ab1,what="n")
summary(ab1,what=c("n","symmetry","table"))
# show the zeroes (rather than dashes)
summary(ab1,what="table",zero.print="0")
# flip the table -- ease of comparison to age-bias plot
summary(ab1,what="table",flip.table=TRUE)

## default plot
plot(ab1)
## demonstrates controlling the y-axis limits
plot(ab1,ylim=c(0,10))
## plot with the data points shown
plot(ab1,show.pts=TRUE,transparency=1/3)
## plot with the range shown
plot(ab1,show.rng=TRUE)
## plot with no difference in significance bar colors
plot(ab1,col.err="black",col.err.sig="black")
## plot of differences (note could use same modifications as shown above)
plot(ab1,difference=TRUE)
## sunflower plot
plot(ab1,what="sunflower")
plot(ab1,what="sunflower",difference=TRUE)
## "Numbers" plot
plot(ab1,what="number",col.ref="gray50")

## Unit tests for what="symmetry"
######## Create Evans & Hoenig (2008) X matrix as a check (table 1)
X.dat <- data.frame(ageR=c(2,2,2,2,2,2,2,2),
                   ageC=c(1,1,1,1,3,3,3,3))
X <- ageBias(ageC~ageR,data=X.dat)
summary(X,what="table",zero.print=0)
summary(X,what="symmetry")   # check

######## Create Evans & Hoenig (2008) Y matrix as a check (table 1)
Y.dat <- data.frame(ageR=c(1,1,1,2,2,2),
                   ageC=c(2,2,3,3,3,3))
Y <- ageBias(ageC~ageR,data=Y.dat)
summary(Y,what="table",zero.print=0)
summary(Y,what="symmetry")   # check

######## Create Evans & Hoenig (2008) Z matrix (McBride's matrix) as a check
Z.dat <- data.frame(ageR=c(1,1,1,2,2,2,3),
                   ageC=c(2,2,2,3,3,3,1))
Z <- ageBias(ageC~ageR,data=Z.dat)
summary(Z,what="table",zero.print=0)
summary(Z,what="symmetry")  #check
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\references{
Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.
Graphical and statistical methods for determining the
consistency of age determinations. Transactions of the
American Fisheries Society, 124:131-138.
\url{http://www.bio.gc.ca/otoliths/publication-eng.php}

Evans, G.T. and J.M. Hoenig.  1998.  Testing and viewing
symmetry in contingency tables, with apprlication to
readers of fish ages.  Biometrics 54:620-629.
\url{http://www.fisheries.vims.edu/hoenig/pdfs/Viewing.pdf}.

Hoenig, J.M., M.J. Morgan, and C.A. Brown. 1995.  Analysing
differences between two age determination methods by tests
of symmetry. Canadian Journal of Fisheries And Aquatic
Systems, 52:364-368.
\url{http://www.fisheries.vims.edu/hoenig/pdfs/Hoenig_Morgan_Brown_AgeDeterminationSymmetry.pdf}

Muir, A.M., M.P. Ebener, J.X. He, and J.E. Johnson.  2008.
A comparison of the scale and otolith methods of age
estimation for lake whitefish in Lake Huron.  North
American Journal of Fisheries Management, 28:625-635.
}
\seealso{
\code{\link{agePrecision}} and \code{compare2} in
\pkg{fishmethods}.
}
\keyword{htest}
\keyword{manip}

