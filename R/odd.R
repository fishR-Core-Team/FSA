#' Determines if a number is odd.
#' 
#' Determines if a number is odd.
#' 
#'@param x A numeric vector.
#'@return A logical vector of the same length as x.
#'@keywords manip
#'@export
#'@examples
#'d <- 1:8
#'odd(d)
#'
odd <- function (x) x%%2 == 1
