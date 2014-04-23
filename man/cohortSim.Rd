\name{cohortSim}
\alias{cohortSim}
\title{Dynamics plots to explore numbers in a cohort over time.}
\usage{
cohortSim(age.max = 15)
}
\arguments{
  \item{age.max}{A single numeric that indicates the
  maximum age to use in the simulations.}
}
\value{
None.  An interactive graphic connected to slider controls
is produced.
}
\description{
Constructs a plot of the hypothetical number of individuals
in a cohort over time.  The initial size of the cohort and
the instantaneous mortality rate are controlled by slider
bars.
}
\examples{
if (interactive()) {

cohortSim()

} # end if interactive
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\keyword{iplot}

