\name{ageComp}
\alias{ageComp}
\alias{plot.ageComp}
\alias{summary.ageComp}
\title{Results for comparing paired sets of ages.}
\usage{
ageComp(formula, data, col.lab = "First", row.lab = "Second",
  method = p.adjust.methods, sig.level = 0.05)

\method{plot}{ageComp}(x, what = c("bias", "difference", "sunflower"),
  xlab = x$col.lab, ylab = x$row.lab, show.n = TRUE, show.pts = FALSE,
  show.rng = FALSE, pch.mean = 3, col.err = "blue", col.err.sig = "red",
  lwd.err = 2, pch.pts = 19, col.pts = rgb(0, 0, 0, transparency),
  transparency = 1/20, col.rng = "gray", lwd.rng = 2, col.ref = "black",
  lwd.ref = 1, lty.ref = 2, xlim = NULL, ylim = NULL,
  yaxt = par("yaxt"), ...)

\method{summary}{ageComp}(object, what = c("precision", "agreement",
  "differences", "prec.stats", "symmetry", "bias", "bias.diff", "detail"),
  zero.print = "-", flip.table = FALSE, ...)
}
\arguments{
  \item{formula}{A formula of the form \code{col~row},
  where \code{col} and \code{row} generically represent the
  variables that contain the ages that will form the
  columns and rows, respectively, of the age-agreement
  table.  See details.}

  \item{data}{A data.frame that minimally contains the
  paired age assignments. See description for
  \code{formula} and etails.}

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

  \item{object}{An object saved from the \code{ageComp}
  call (i.e., of class \code{ageComp}).}

  \item{x}{An object saved from the \code{ageComp} call
  (i.e., of class \code{ageComp}).}

  \item{what}{A string that indicates what type of summary
  to print or plot to construct.  See details.}

  \item{zero.print}{A string that indicates what should be
  printed in place of the zeroes on an age-agreement table.
  The default is to print a single dash.}

  \item{flip.table}{A logical that indicates whether the
  age-agreement table should be \sQuote{flipped} (i.e.,
  rows are reversed so that the younger ages are at the
  bottom of the table).  This makes the table more directly
  comparable to the age-bias plot.}

  \item{xlab}{A string that contains a label for the x-axis
  age assignments.}

  \item{ylab}{A string that contains a label for the y-axis
  age assignments.}

  \item{show.n}{A logical that indicates whether the sample
  sizes for each level of the x-axis variable is shown
  (\code{=TRUE}, default) or not (\code{=FALSE}).}

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
\code{ageComp} returns a list with the following items:
\itemize{ \item detail A data frame will all given and
computed information about each fish. \item absdiff A table
of percentage by absolute differences in age. \item agree
The age-agreement table. \item stats The summary statistics
of percent agreement, AVE, and CV. \item bias A data.frame
that contains the bias statistics. \item bias.diff A
data.frame that contains the bias statistics for the
differences. \item col.lab A string that contains an
optional label for the column structure or readings. \item
row.lab A string that contains an optional label for the
row structure or readings. }

The \code{summary} and \code{plot} function do not return
anything.
}
\description{
Computes overall measures of precision and bias for paired
age assignments. The paired age assignments can consist of
age measurements recorded for two structures (e.g.,
otoliths and scales), for two readers of the same
structure, or for one reader at two times.
}
\details{
The main function, \code{ageComp}, requires a formula of
the form \code{col~row}, where \code{col} and \code{row}
generically represent the variables that contain the ages
that will form the columns and rows, respectively, of the
age- agreement table.  If one age assignment is thought to
be more accurate than the other, then it should form the
columns and should be on the left-hand-side of the formula.
It should also be noted that the variable that forms the
columns in the age-agreement table will also form the
x-axis in the various plots produced by \code{plot}.

If \code{what="agreement"} in \code{summary} then only the
age-agreement table is printed.  If
\code{what="differences"} then only a table of the
percentage of individuals by absolute difference in age is
printed.  If \code{what="prec.stats"} then summary
statistics of overall percentage agreement, overall
absolute percentage error (APE), and overall coefficien to
variation (CV) are printed.  All of the previous items are
printed if \code{what="precision"} (the default).

If \code{what="detail"} in \code{summary} then a data frame
of the original data plus the intermediate caculations of
the differences in assigned ages, average age, standard
deviation of age, APE, and CV for each individual fish will
be printed .  If \code{what="symmetry"} then Bowker's test
of symmetry, as proposed by Hoenig (1995), to determine if
systematic differences exist between paired age assignments
is printed.

If \code{what="bias"} (the default) in \code{plot} then an
age-bias plot as described by Campana et al. (1995) is
produced.  The variable that formed the columns in the
original \code{ageComp} call is plotted on the x-axis.
Confidence intervals plotted in red are computed for the
mean of the y-axis data at each \dQuote{level} (i.e., age)
of the x-axis data.  The level of confidence interval is
controlled by \code{sig.level} given in the original
\code{ageComp} call. Vertical lines that connect the
minimum to the maximum value of the y-axis data at each
\dQuote{level} of the x-axis data are plotted in grey.  A
1:1 (45 degree) agreement line is shown for comparative
purposes.  The sample sizes at each \dQuote{level} of the
x-axis data is shown if \code{show.n=TRUE} (the default).

If \code{what="difference"} in \code{plot} then an
age-difference plot as used by Muir et al. (2008) is
produced.  The plot is similar to the age-bias plot
described above except that the y-axis data is the
difference in the paired ages in the rows and columns from
the original \code{ageComp} call (specifically,
columns-rows).

If \code{what="sunflower"} in \code{plot} then a sunflower
plot is constructed. A sunflower plot contains a point for
each unique (x,y) coordinate with a \dQuote{petal} added to
each point for each observation that has the same (x,y)
coordinate.  Thus, points with more petals have more
observations at that point.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/AgeComparisons.pdf}
}
\examples{
data(WhitefishLC)
ac1 <- ageComp(otolithC~scaleC,data=WhitefishLC,col.lab="Otolith Age",row.lab="Scale Age")
summary(ac1)
summary(ac1,what="agreement")
summary(ac1,what="prec.stats")
summary(ac1,what="detail")
summary(ac1,what="symmetry")
summary(ac1,what="bias")
summary(ac1,what="bias.diff")
# show the zeroes (rather than dashes)
summary(ac1,what="agreement",zero.print="0")
# flip the table -- ease of comparison to age-bias plot
summary(ac1,what="agreement",flip.table=TRUE)

## default plot
plot(ac1)
## plot with the data points shown
plot(ac1,show.pts=TRUE)
## plot with the range shown
plot(ac1,show.rng=TRUE)
## plot with no difference in significance bar colors
plot(ac1,col.err="red",col.err.sig="red")
## plot of differences (not could use same modifications as shown above)
plot(ac1,what="difference")
## sunflower plot
plot(ac1,what="sunflower")
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

Hoenig, J.M., M.J. Morgan, and C.A. Brown. 1995.  Analysing
differences between two age determination methods by tests
of symmetry. Canadian Journal of Fisheries And Aquatic
Systems, 52:364-368.

Muir, A.M., M.P. Ebener, J.X. He, and J.E. Johnson.  2008.
A comparison of the scale and otolith methods of age
estimation for lake whitefish in Lake Huron.  North
American Journal of Fisheries Management, 28:625-635.
}
\keyword{htest}
\keyword{manip}

