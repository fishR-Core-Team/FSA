\docType{data}
\name{BluegillJL}
\alias{BluegillJL}
\title{Capture histories (2 samples) of bluegill from Jackson Lake.}
\format{A data frame with 277 observations on the following 2 variables.
\describe{
\item{first}{a numeric vector of indicator variables for the first sample (1=captured).}
\item{second}{a numeric vector of indicator variables for the second sample (1=captured).}
}}
\source{
Unsure.
}
\description{
Each line consists of the capture history over two samples
of bluegill (\emph{Lepomis macrochirus}) in Jackson Lake
(MI).  This file contains the capture histories for only 6"
bluegill.
}
\section{Topic(s)}{
  \itemize{ \item Population size \item Abundance \item
  Mark-recapture \item Petersen method \item Capture
  history }
}
\examples{
data(BluegillJL)
str(BluegillJL)
head(BluegillJL)
}
\concept{
Abundance 'Population Size' 'Mark-Recapture' Petersen
'Capture History'
}
\keyword{datasets}

