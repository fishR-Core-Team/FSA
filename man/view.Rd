\name{view}
\alias{view}
\title{Shows a random selection of rows from a data frame or matrix.}
\usage{
view(x, n = 6L, which = NULL)
}
\arguments{
  \item{x}{A data frame or matrix.}

  \item{which}{A numeric or string vector containing the
  column numbers or names to display.  Defaults to showing
  all columns.}

  \item{n}{A single numeric indicating the number of rows
  to display.}
}
\value{
No value is returned but a random (but sorted) selection of
rows from the data frame is displayed.
}
\description{
Shows a random selection of rows from a data frame or
matrix.
}
\note{
If \code{n} is larger than the number of rows in \code{x}
then \code{x} is displayed without randomizing the rows.
}
\examples{
data(iris)
view(iris)
view(iris,10)
view(iris,which=c("Sepal.Length","Sepal.Width","Species"))
view(iris,which=grep("Sepal",names(iris)))

## Make a matrix for demonstration purposes only
miris <- as.matrix(iris[1:4,])
view(miris)
view(miris,10)
view(miris,10,which=2:4)
}
\keyword{manip}

