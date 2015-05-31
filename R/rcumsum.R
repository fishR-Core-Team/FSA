#' @name rcumsum
#' 
#' @title Computes the prior to or reverse cumulative sum of a vector.
#'
#' @description Computes the prior-to (i.e., the cumulative sum prior to but not including the current value) or the reverse (i.e., the number that large or larger) cumulative sum of a vector.
#'
#' @note An \code{NA} in the vector causes all returned values at and after the first \code{NA} for \code{pcumsum} and at and before the last \code{NA} for \code{rcumsum} to be \code{NA}.  See the examples.
#'
#' @param x a numeric object.
#'
#' @return A numeric vector that contains the prior-to or reverse cumulative sums.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{cumsum}}.
#'
#' @keywords misc
#'
#' @examples
#' ## Simple example
#' cbind(vals=1:10,
#'       cum=cumsum(1:10),
#'       pcum=pcumsum(1:10),
#'       rcum=rcumsum(1:10))
#'
#' ## Example with NA
#' vals <- c(1,2,NA,3)
#' cbind(vals,
#'       cum=cumsum(vals),
#'       pcum=pcumsum(vals),
#'       rcum=rcumsum(vals))
#'
#' ## Example with NA
#' vals <- c(1,2,NA,3,NA,4)
#' cbind(vals,
#'       cum=cumsum(vals),
#'       pcum=pcumsum(vals),
#'       rcum=rcumsum(vals))
NULL

#' @rdname rcumsum
#' @export
rcumsum <- function(x) {
  if (!is.vector(x)) stop("'x' must be a vector.",call.=FALSE)
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  rev(cumsum(rev(x)))
}

#' @rdname rcumsum
#' @export
pcumsum <- function(x) {
  if (!is.vector(x)) stop("'x' must be a vector.",call.=FALSE)
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  cumsum(x)-x
}
