#' @title Find positions in a vector that have a different value from the previous position.
#'
#' @description Find positions in a vector that have a different value from the previous position.
#'
#' @param x A vector of sorted (generally) values.
#' @param include.first A logical that indicates whether the returned vector of positions should have a ``1'' at the beginning (i.e., by definition the first position is the first position that is different than the previous value).
#'
#' @return A vector of positions in \code{x} where the value differs from the value in the previous position.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords manip
#'
#' @examples
#' x <- rep(LETTERS[1:6],times=c(5,4,1,1,2,4))
#' data.frame(pos=1:length(x),x)  # for demonstration only
#' changesPos(x)
#' v <- rep(10:17,times=c(4,1,1,2,4,2,1,1))
#' data.frame(pos=1:length(v),v)  # for demonstration only
#' changesPos(v)
#' ## An uninteresting example -- i.e., vector not sorted
#' z <- sample(10:17,20,replace=TRUE)
#' data.frame(pos=1:length(z),z)  # for demonstration only
#' changesPos(z)
#' z1 <- z[order(z)]
#' data.frame(pos=1:length(z1),z1)  # for demonstration only
#' changesPos(z1)
#'
#' @export
changesPos <- function(x,include.first=TRUE) {
  if (!is.vector(x)) stop("Only works if 'x' is a vector.",call.=FALSE)
  if (length(x)==0) stop("Only works if length of 'x'>0.",call.=FALSE)
  if (length(x)==1 & !include.first) stop("Only works if length of 'x'>1 when 'include.first=FALSE'.",call.=FALSE)
  if (length(x)==1) tmp <- 1
  else {
    tmp <- which(x[1:(length(x)-1)]!=x[2:length(x)])+1
    if (include.first) tmp <- c(1,tmp)
  }
  tmp
}
