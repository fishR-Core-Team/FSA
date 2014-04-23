\name{lwModelSim}
\alias{lwModelSim}
\title{Dynamics plots to explore length-weight power function models.}
\usage{
lwModelSim(x = NULL, y = NULL, wt.max = 50)
}
\arguments{
  \item{x}{A numeric vector of observed ages.  See
  details.}

  \item{y}{A numeric vector of observed lengths.  See
  details.}

  \item{wt.max}{A single numeric that indicates the maximum
  weight for the y-axis scale.}
}
\value{
None.  An interactive graphic is produced.
}
\description{
Plots a length-weight power function evaluated at a vector
of lengths with or without observed data.  A set of slider
bars is included so that the parameter values can be
adjusted by the user to explore the effects of a parameter
on the model or for visually fitting a length-weight model
to a set of data.
}
\details{
If the \code{x} and \code{y} arguments are used then the
observed data in these vectors is plotted and the
length-weight model is superimposed.  If these arguments
are \code{NULL} then the length-weight model is shown
without any observed data.  The former option is useful for
visually fitting a model to actual data.  The latter option
is useful for exploring the effects of different parameter
values on the model dynamics.

A list slider bars is created from which the parameters of
the model -- W=a*L^b -- can be dynamically changed.  In
addition, the minimum and maximum length over which to
evaluate the length-weight model are controlled by the
slider.  The maximum weight plotted is controlled by an
argument to the function.

The plot object should be \dQuote{exit}ed when finished
exploring the model.
}
\note{
The range of values allowed for each of the parameters was
chosen to allow a wide variety of modeling opportunities.
However, it is highly likely that these ranges do not
encompass every possible set of values that a user may wish
to view.  Thus, this function is more useful as a learning
tool rather than a research simulation tool.
}
\examples{
if (interactive()) {

# Exploration of model dynamics
lwModelSim()

# Visual fit of length-weight model to one set of ruffe length-weight data from the FSA package
library(FSA)
data(RuffeWs)
r1 <- Subset(RuffeWs,regrnum==220)
lwModelSim(x=r1$fl,y=r1$wt)

} # end if interactive
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\keyword{iplot}

