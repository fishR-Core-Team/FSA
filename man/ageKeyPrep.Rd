\name{ageKeyPrep}
\alias{ageKeyPrep}
\title{Prepares problematic age-length key matrics for use.}
\usage{
ageKeyPrep(key, show.msgs = TRUE)
}
\arguments{
  \item{key}{A numeric matrix that contains the problematic
  age-length key.}

  \item{show.msgs}{A logicical indicating if process
  messages should be displayed (\code{TRUE}; default).}
}
\value{
A matrix that contains the original age-length key matrix
sent in \code{key} with missing rows either deleted if they
were at the beginning or end of the matrix or replaced with
interpolated values (see details) if in the middle of the
matrix.
}
\description{
This function can be used to prepare problematic age-length
key matrices for use in \code{ageKey}.  Problems that can
be fixed are the deletion of empty rows at the beginning or
at the end of the age-length key matrix and the
\sQuote{interpolation} of values in missing rows in the
\sQuote{middle} of the age-length key.
}
\details{
This function can be used to prepare problematic age-length
key matrices for use in \code{ageKey}.  Problems that can
be fixed are the deletion of empty rows at the beginning or
at the end of the age-length key matrix and the
\sQuote{interpolation} of values in missing rows in the
\sQuote{middle} of the age-length key.  In the latter
example, a cell in the missing row is computed by assuming
a linear trend between the immediately adjacent rows with
data both above and below the missing row.  This results in
simply averaging the values in the adjacent rows if only
one row is missing in the age-length key matrix.

THIS FUNCTION IS EXPERIMENTAL AT THIS POINT
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/AgeLengthKey.pdf}
}
\examples{
# create a "good" small ALK matrix
alk <- matrix(c(0.4,0.3,0.3,0.0,
0.2,0.4,0.3,0.1,
0.1,0.2,0.4,0.3,
0.0,0.1,0.4,0.5,
0.0,0.0,0.2,0.8),
nrow=5,byrow=TRUE)
rownames(alk) <- c(10,20,30,40,50)
colnames(alk) <- c(2,3,4,5)
addmargins(alk,margin=2)

# Create various "problematic" ALK matrics
alk1 <- alk2 <- alk5 <- alk24 <- alk23 <- alk12 <- alk45 <- alk135 <- alk
alk1[1,] <- NA
alk2[2,] <- NA
alk5[5,] <- NA
alk24[c(2,4),] <- NA
alk23[c(2,3),] <- NA
alk12[1:2,] <- NA
alk45[4:5,] <- NA
alk135[c(1,3,5),] <- NA

# Show how the "problematic" matrices are "fixed"
alk1
ageKeyPrep(alk1)
alk5
ageKeyPrep(alk5)
alk2
ageKeyPrep(alk2)
alk24
ageKeyPrep(alk24)
alk23
ageKeyPrep(alk23)
alk12
ageKeyPrep(alk12)
alk45
ageKeyPrep(alk45)
alk135
ageKeyPrep(alk135)
}
\seealso{
\code{\link{ageKey}}.
}
\keyword{manip}

