#' @name oddeven
#' 
#' @title Determine if a number is odd or even.
#' 
#' @description Determine if a number is odd or even.
#' 
#' @param x A numeric vector.
#' 
#' @return A logical vector of the same length as x.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords manip
#' 
#' @examples
#' ## Individual values
#' is.odd(1)
#' is.odd(2)
#' is.even(3)
#' is.even(4)
#' 
#' ## Vector of values
#' d <- 1:8
#' data.frame(d,odd=is.odd(d),even=is.even(d))
NULL

#' @rdname oddeven
#' @export
is.odd <- function (x) iOddEven(x,1)

#' @rdname oddeven
#' @export
is.even <- function(x) iOddEven(x,0)


## Internal function
iOddEven <- function(x,checkval) {
  if (!is.vector(x)) stop("'x' must be a vector.",call.=FALSE)
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  x%%2 == checkval
}
