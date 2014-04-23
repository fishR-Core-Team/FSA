\name{vbDataGen}
\alias{vbDataGen}
\title{Randomly create a von Bertalanffy data set.}
\usage{
vbDataGen(n, Linf, K, t0, minAge = 1, maxAge = 9, CV = 0.1, undf = 5,
  dataType = c("wide", "long", "groupedData", "atCapture"))
}
\arguments{
  \item{n}{Number of individuals to simulate data for.}

  \item{Linf}{The \sQuote{true} value of the Linf
  parameter.}

  \item{K}{The \sQuote{true} value of the K parameter.}

  \item{t0}{The \sQuote{true} value of the t0 parameter.}

  \item{minAge}{The minimum possible age for an individual
  in the data set.}

  \item{maxAge}{The maximum possible age for an individual
  in the data set.}

  \item{CV}{The coefficient of variation to be used when
  adding random error.}

  \item{undf}{The uniform noise division factor for adding
  random noise to the data.  See Box 2 in Vigliola and
  Meekan (2009).}

  \item{dataType}{The type of data frame that should be
  returned.  See details.}
}
\value{
A data frame.  See details.
}
\description{
Randomly creates a data set according to a typical von
Bertalanffy model following the methodology of Vigliola and
Meekan (2009).
}
\details{
This function follows the simulation methodology shown in
Box 2 of Vigliola and Meekan (2009) modified for the
typical von Bertalanffy model (rather than the exponential
model illustrated in that Box).

The results can be returned in one of four formats.  In
\sQuote{wide} format the data frame will contain a column
for the fish's ID (\code{id}), simulated age-at-capture
(\code{agecap}), and simulated lengths at previous ages
(\code{anuX}).  In \sQuote{long} format the same data will
be returned in a data frame with a column for fish's ID,
age-at-capture, age (\code{age}) and length (\code{anu}) in
the fish's history.  In this format the data for an
individual fish will be found in as many rows as the age of
the fish.  Thus, the data for a five-year-old fish will
appear in five columns corresponding to the ages and
lengths at each of the fish's five years of life.  The
\sQuote{groupedData} format will look exactly like the long
format but is a \code{groupedData()} object (from the
\pkg{nlme} package).  Finally, the \sQuote{atCapture}
format removes the longitudinal nature of the previous
three formats and returns just the fish's ID,
age-at-capture, and final length-at-capture.  The final
length-at-capture is simply the last \code{anuX} data in
the three previous formats.
}
\note{
This function will produce warnings about the random number
generators producing \code{NA}s.  This is a byproduct of
the vectorization used in the algorithm but does not cause
any problems with the output.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffy.pdf},
  \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffyExtra.pdf}
}
\examples{
vbwide <- vbDataGen(10,Linf=30,K=0.2,t0=-0.2)
head(vbwide)
vblong <- vbDataGen(10,Linf=30,K=0.2,t0=-0.2,dataType="long")
head(vblong)
vbgrp <- vbDataGen(10,Linf=30,K=0.2,t0=-0.2,dataType="groupedData")
head(vbgrp)
\dontrun{
library(lattice)
plot(vbgrp)
}
vbcap <- vbDataGen(10,Linf=30,K=0.2,t0=-0.2,dataType="atCapture")
head(vbcap)
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\references{
Vigliola, L. and M.G. Meekan.  2009.  The back-calculation
of fish growth from otoliths.  Chapter 6 (pp. 174-211) in
B.S. Green et al. (eds.), Tropical Fish Otoliths:
Information for Assessment, Management and Ecology,
Reviews: Methods and Technologies in Fish Biology and
Fisheries 11.
}
\seealso{
\code{\link{vbComp}}, \code{\link{growthModelSim}}, and
\code{groupedData} in \pkg{nlme}
}
\keyword{manip}

