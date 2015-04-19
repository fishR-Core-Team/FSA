#' @title Computes standard error of the mean.
#'
#' @description Computes the standard error of the mean (i.e., standard deviation divided by the square root of the sample size).
#'
#' @param x A numeric vector.
#' @param na.rm A logical that indicates whether missing values should be removed before computing the standard error.
#' 
#' @return A single numeric that is the standard error of the mean of \code{x}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link[sciplot]{se}} in \pkg{sciplot} for similar functionality.
#'
#' @keywords manip
#'
#' @examples
#' x <- 1:20
#' sd(x)/sqrt(length(x))
#' se(x)
#' 
#' # all return NA if missing values are not removed
#' x2 <- c(x,NA)
#' sd(x2)/sqrt(length(x2))
#' 
#' # Better if missing values are removed
#' se(x2,na.rm=FALSE)
#' sd(x2,na.rm=TRUE)/sqrt(length(x2[complete.cases(x2)]))
#' se(x2)
#' 
#' @export
se <- function (x,na.rm=TRUE) {
  if (na.rm) x <- x[complete.cases(x)]
  sqrt(var(x)/length(x))
}