\name{growthModelSim}
\alias{growthModelSim}
\title{Dynamics plots to explore typical fisheries growth models.}
\usage{
growthModelSim(type = c("vbTypical", "vbOriginal", "vbGalucciQuinn",
  "vbMooij", "vbSchnute", "vbTypicalW", "vbOriginalW", "Gompertz1", "Gompertz2",
  "Gompertz3", "Schnute"), x = NULL, y = NULL, data = NULL,
  max.len = 500, max.wt = 500)
}
\arguments{
  \item{type}{A single character string identifying which
  growth model to use.  See details.}

  \item{x}{An optional vector containing observed ages or a
  formula.  See details.}

  \item{y}{An optional vector containing observed lengths.
  See details.}

  \item{data}{A data frame from which the variables in the
  formula should be drawn.}

  \item{max.len}{A single numeric indicating the maximum
  length to use in the simulations.}

  \item{max.wt}{A single numeric indicating the maximum
  weight to use in the simulations (only used of
  \code{type=} \code{"vbTypicalW"} or
  \code{"vbOriginalW"}.}
}
\value{
None.  However a dynamic graphic connected to slider bar
controls in which the user can change the maximum age over
which the growth model is evaluated and change the
parameters specific to the chosen growth model.
}
\description{
Plots hypothetical size-at-age for one of seven possible
parameterizations of the von Bertalanffy, three possible
parameterizations of the Gompertz, and the Schnute growth
models.  Slider bars are used to dynamically alter the
parameters of each model.
}
\details{
This function can be used to explore the \dQuote{shape} of
the growth models for various choices of the parameters.
In this usage the \code{x} and \code{y} arguments should be
(left) set at \code{NULL}.  This function can also be used
to visually \dQuote{fit} a growth model to a set of
observed lengths and ages. This usage may be used to
provide reasonable starting values for the parameters when
fitting the growth model to the data with non-linear
least-squares.  The observed data are plotted by including
a formula of the form \code{length~age} in \code{x} and a
data frame from which to draw the variables in the formula
in the \code{data} arguments.

The \code{type} argument is used to choose a type of growth
model and must be one of the following (the models can be
seen with \code{\link{growthModels}} and
\code{\link{vbModels}}):

\tabular{ll}{ \code{"vbTypical"} \tab The "typical"
Beverton-Holt parameterized von Bertalanffy model.\cr
\code{"vbOriginal"} \tab The original parameterization from
von Bertalanffy.\cr \code{"vbMooij"} \tab The Mooij et al
(1999) paramaterization of the von Bertalanffy model.\cr
\code{"vbGalucciQuinn"} \tab The Galucci & Quinn (1979)
parameterization of the von Bertalanffy model.\cr
\code{"vbSchnute"} \tab The Schnute-like paramaterization
of the von Bertalanffy model.\cr \code{"vbTypicalW"} \tab
The "typical" Beverton-Holt parameterized von Bertalanffy
model, but for weights rather than lengths (thus, includes
one more parameter).\cr \code{"vbOriginalW"} \tab The
original parameterization from von Bertalanffy, but for
weights rather than lengths (thus, includes one more
parameter).\cr \code{"Gompertz1"} \tab The "first"
parameterization of the Gompertz model.\cr
\code{"Gompertz2"} \tab The "second" parameterization of
the Gompertz model.\cr \code{"Gompertz3"} \tab The "third"
parameterization of the Gompertz model.\cr \code{"schnute"}
\tab The Schnute(1981) four-parameter general growth model.
}
}
\examples{
if (interactive()) {

# Explore growth models (no data) -- use the defaults
growthModelSim()

# Schnute parameterization of the von Bertalanffy model
growthModelSim(type="vbSchnute")

## Explore growth models superimposed on length-at-age data
# get Smallmouth Bass data from FSA package
library(FSA)
data(SMBassWB)

# interactively "fit" the second paramaterization of the Gompertz model to the data
growthModelSim(type="Gompertz2",lencap~agecap,data=SMBassWB,max.len=500)

} # end if interactive
}
\references{
Francis, R.I.C.C.  1988.  Are growth parameters estimated
from tagging and age-length data comparable?  Canadian
Journal of Fisheries and Aquatic Sciences, 45:936-942.

Galucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing,
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

Schnute, J. and D. Fournier. 1980.  A new approach to
length-frequency analysis: Growth structure.  Canadian
Journal of Fisheries and Aquatic Sciences, 37:1337-1351.
}
\seealso{
\code{\link{growthModels}}, \code{\link{vbModels}}, and
\code{\link{vbStarts}}.
}
\keyword{iplot}

