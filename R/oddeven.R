#' @title Determines if a number is odd or even.
#' 
#' @description Determines if a number is odd or even.
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
#' d <- 1:8
#' is.odd(d)
#' is.even(d)
#' 
#' @rdname oddeven
#' @export
is.odd <- function (x) x%%2 == 1

#' @rdname oddeven
#' @export
is.even <- function(x) x%%2 == 0
