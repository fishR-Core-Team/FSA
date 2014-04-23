\name{srCobWeb}
\alias{srCobWeb}
\title{Plots typical fisheries stock-recruitment models with cobwebbing traces.}
\usage{
srCobWeb(type = c("BevertonHolt", "Ricker"), param = 1, N = 100,
  initS = 50, N2ignore = 0, ...)
}
\arguments{
  \item{type}{A single string that indicates which
  stock-recruitment model to use. See
  \code{\link{srModels}}.}

  \item{param}{A single numeric that indicates which
  parameterization of the \code{type} to use.  See
  \code{\link{srModels}}.}

  \item{N}{A single numeric that indicates the number of
  iterations to run for the cobwebbing.}

  \item{initS}{A single numeric that indicates the starting
  value of spawning stock size.}

  \item{N2ignore}{A single numeric that indicates the
  number of initial iterattions to ignore when plotting the
  results.  This allows the function to \sQuote{burn-in}
  before showing results.}

  \item{\dots}{Optional parameters that are used in
  stock-recruitment model indicated with \code{type} and
  \code{param}.  See \code{\link{srModels}} and
  \code{\link{srFuns}}\ for the list and meanings of the
  parameters.  Also see the examples..}
}
\value{
None.  However a two-paned graphic is printed.  The top
pane represents the stock-recruit function as a blue line
and the 1:1 replacement line as a red line.  In addition,
the \sQuote{cob-webbing} procedure is shown by a gray line.
The gray line of the cobwebbing will become \sQuote{darker}
as more cobwebbing lines are plotted on top of each other.
This makes it easer to see periodicity if it exists.  The
\sQuote{stopping} point in the cobwebbing is shown as a red
dot and the \sQuote{starting} point is shown, as long as
\code{N2Ignore=0}, a green dot.  The bottom panel is the
stock size plotted against time.
}
\description{
Plots typical fisheries stock-recruitment models with
cobwebbing traces for four parameterizations of the
Beverton-Holt and three parameterizations of the Ricker
stock-recruit models.
}
\details{
This function can be used to explore the dynamics of
stock-recruitment models for various parameter choices.

The \code{type=} argument is used to choose either the
\code{"BevertonHolt"} or \code{"Ricker"} models.  Different
parameterizations of these two models are chosen with the
\code{param=} argument.  Four paramaterizations of the
Beverton-Holt model and three parameterizations of the
Ricker model are allowed.  See \code{\link{srModels}} and
\code{\link{srFuns}} for a representation of each
parameterization.
}
\examples{
# First parametrization of the Ricker model
#   exhibiting various dynamics
srCobWeb(type="Ricker",param=1,a=5,b=0.01)
srCobWeb(type="Ricker",param=1,a=7,b=0.01)
srCobWeb(type="Ricker",param=1,a=10,b=0.01)
srCobWeb(type="Ricker",param=1,a=13,b=0.01)
srCobWeb(type="Ricker",param=1,a=14.5,b=0.01)
srCobWeb(type="Ricker",param=1,a=15,b=0.01)
srCobWeb(type="Ricker",param=1,a=16,b=0.01)
srCobWeb(type="Ricker",param=1,a=17,b=0.01)

# demonstration N2ignore
srCobWeb(type="Ricker",param=1,a=13,b=0.01,N2ignore=30)

# Third parametrization of the Ricker model
srCobWeb(type="Ricker",param=3,a=13,Rp=300)

# First parametrization of the Beverton-Holt model
srCobWeb(type="BevertonHolt",param=1,a=20,b=0.01)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\seealso{
\code{\link{srModels}}, \code{\link{srFuns}}, and
\code{\link{srSim}}.
}
\keyword{iplot}

