#' Shows rows from the head and tail of a data frame or matrix.
#'
#' Shows rows from the head and tail of a data frame or matrix.
#'
#' @param x A data frame or matrix.
#' @param which A numeric or string vector that contains the column numbers or names to display.  Defaults to showing all columns.
#' @param n A single numeric that indicates the number of rows to display from each of the head and tail of structure.
#' @param addrownums If there are no row names for the MATRIX, then create them from the row numbers.
#' @param \dots Arguments to be passed to or from other methods.
#'
#' @return A matrix or data.frame with 2*n rows.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @note If \code{n} is larger than the number of rows in \code{x} then all of \code{x} is displayed.
#'
#' @keywords manip
#'
#' @examples
#' data(iris)
#' headtail(iris)
#' headtail(iris,10)
#' headtail(iris,which=c("Sepal.Length","Sepal.Width","Species"))
#' headtail(iris,which=grep("Sepal",names(iris)))
#' headtail(iris,n=200)
#'
#'## Make a matrix for demonstration purposes only
#' miris <- as.matrix(iris[,1:4])
#' headtail(miris)
#' headtail(miris,10)
#' headtail(miris,addrownums=FALSE)
#' headtail(miris,10,which=2:4)
#'
#' @export
headtail <- function(x,n=3L,which=NULL,addrownums=TRUE,...) {
  if (!(is.matrix(x) | is.data.frame(x))) stop("'x' must be a matrix or data.frame.",call.=FALSE)
  stopifnot(length(n) == 1L)
  N <- nrow(x)
  n <- ifelse(n<0L,max(N+n,0L),min(n,N))
  if (n>=N) tmp <- x
  else {
    h <- head(x,n,...)
    if (addrownums) {
      if (is.null(rownames(x))) rownames(h) <- paste0("[",1:n,",]")
    } else rownames(h) <- NULL
    t <- tail(x,n,addrownums,...)
    tmp <- rbind(h,t)
  }
  if (!is.null(which)) tmp <- tmp[,which]
  tmp
}
