\docType{data}
\name{VBGMlit}
\alias{VBGMlit}
\title{Von Bertalanffy parameter estimates from the literature.}
\format{A data frame with 87 observations on the following 4 variables:
\describe{
\item{Species}{Species names.}
\item{Linf}{Values of the Linf parameter.}
\item{K}{Values of the K parameter.}
\item{t0}{Values of the t0 parameter.}
}}
\source{
Most of the data presented comes from -- He, Ji X.  and
D.J. Stewart. 2001.  Age and size at first reproduction of
fishes: predictive models based only on growth
trajectories. Ecology, 82:784-792.
\url{esapubs.org/archive/ecol/E082/006/appendix-A.htm}.
}
\description{
A list of parameter estimates for the traditional von
Bertalanffy growth model from published results in the
literature.
}
\details{
Several species have \code{NA} for the \code{t0} parameter.
}
\section{Topic(s)}{
  \itemize{ \item Growth \item von Bertalanffy }
}
\examples{
data(VBGMlit)
str(VBGMlit)
head(VBGMlit)
}
\concept{
Growth 'von Bertalanffy'
}
\keyword{datasets}

