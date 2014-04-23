\name{vbComp}
\alias{vbComp}
\title{Evaluates and plots the traditional von Bertalanffy growth model for several
sets of parameter choices.}
\usage{
vbComp(df, ages = 20, ...)
}
\arguments{
  \item{df}{A data.frame with rows of different parameter
  choices.  See details.}

  \item{ages}{A numeric or vector of the maximum age over
  which to evaluate the models.  See details.}

  \item{\dots}{Additional arguments to pass to
  \code{plot}.}
}
\value{
None.  However a plot of the hypothetical lengths-at-age
for each species shown in the rows of \code{df} is
produced.
}
\description{
Evaluates and plots the traditional von Bertalanffy growth
model for several parameter choices.  Typical use will be
for comparing the shape of the model among parameter
estimates for multiple species.
}
\details{
The data frame, \code{df}, must have a column that contains
the species name and columns for the Linf, K, and to
parameters (with the columns having those names).  Each row
contains values for a seperate model evaluation.  Typical
exercises will select rows from the \code{data(VBGMlit)}
data frame.

All evaluations will be over ages that begin at zero and
continue to the value or values provided in \code{ages}.
If \code{ages} has a length of 1 then all evaluations will
extend from zero to this age.  However, if \code{ages} is a
vector of the same length as the number of rows in
\code{df} then the extent of the evaluations for each row
in \code{df} will be from 0 to the corresponding value in
\code{ages}.  Thus, \code{ages} can be used to provide
model evaluations over different ages.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffy.pdf},
  \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffyExtra.pdf}
}
\examples{
# load VBGMlit data frame
data(VBGMlit)
# select three example species from VBGMlit
v <- VBGMlit[c(1,7,57),]
# default behavior for three example species
vbComp(v)
# same three species but with varying maximum ages
vbComp(v,ages=c(10,20,25))
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\seealso{
\code{\link{VBGMlit}}.
}
\keyword{hplot}
\keyword{manip}

