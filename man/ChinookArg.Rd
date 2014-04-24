\docType{data}
\name{ChinookArg}
\alias{ChinookArg}
\title{Lengths and weights of Chinook Salmon from three locations in Argentina.}
\format{A data frame with 112 observations on the following 3 variables:
\describe{
\item{tl}{Total length (cm)}
 \item{w}{Weight (kg)}
 \item{loc}{Capture location (Argentina, Petrohue, Puyehue)}
}}
\description{
Lengths and weights of Chinook Salmon from three locations
in Argentina.
}
\section{Topic(s)}{
  \itemize{ \item Length-weight }
}
\examples{
data(ChinookArg)
str(ChinookArg)
head(ChinookArg)
op <- par(mfrow=c(2,2),pch=19)
plot(w~tl,data=ChinookArg,subset=loc=="Argentina")
plot(w~tl,data=ChinookArg,subset=loc=="Petrohue")
plot(w~tl,data=ChinookArg,subset=loc=="Puyehue")
par(op)
}
\concept{
'Length-Weight'
}
\keyword{datasets}

