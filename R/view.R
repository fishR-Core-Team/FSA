#'Shows a random selection of rows from a data frame or matrix.
#'
#'Shows a random selection of rows from a data frame or matrix.
#'
#' @param x A data frame or matrix.
#' @param which A numeric or string vector that contains the column numbers or names to display.  Defaults to showing all columns.
#' @param n A single numeric that indicates the number of rows to display.
#'
#' @return No value is returned but a random (but sorted) selection of rows from the data frame is displayed. 
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @note If \code{n} is larger than the number of rows in \code{x} then \code{x} is displayed without randomizing the rows.
#'
#' @keywords manip
#'
#' @examples
#'data(iris)
#'view(iris)
#'view(iris,10)
#'view(iris,which=c("Sepal.Length","Sepal.Width","Species"))
#'view(iris,which=grep("Sepal",names(iris)))
#'
#'## Make a matrix for demonstration purposes only
#'miris <- as.matrix(iris[1:4,])
#'view(miris)
#'view(miris,10)
#'view(miris,10,which=2:4)
#'
#' @export
view <- function(x,n=6L,which=NULL) {
  if (!(is.matrix(x) | is.data.frame(x))) stop("'x' must be a matrix or data.frame.",call.=FALSE)
  stopifnot(length(n) == 1L)
  N <- nrow(x)
  n <- ifelse(n<0L,max(N+n,0L),min(n,N))
  if (is.null(which)) {
    if (is.matrix(x)) x[sort(sample(1:N,n)),]
      else x[sort(sample(1:N,n)),names(x)]
  } else x[sort(sample(1:N,n)),which]
}
