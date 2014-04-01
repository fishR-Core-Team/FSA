\name{rcumsum}
\alias{rcumsum}
\title{Computes the reverse cumulative sum of a vector.}
\usage{
rcumsum(x)
}
\arguments{
  \item{x}{a numeric object.}
}
\value{
A numeric vector containing the reverse cumulative sums.
}
\description{
Computes the reverse cumulative sum (i.e., the number that
large or larger) of a vector.
}
\details{
An \code{NA} in the vector causes all returned values to be
\code{NA}.
}
\examples{
cumsum(1:10)
 rcumsum(1:10)
}
\seealso{
\code{cumsum}.
}
\keyword{misc}

