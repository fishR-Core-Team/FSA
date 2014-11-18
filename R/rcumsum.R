#' @title Computes the prior to or reverse cumulative sum of a vector.
#'
#' @description Computes the prior-to (i.e., the cumulative sum priot to but not including the current value) or the reverse cumulative sum (i.e., the number that large or larger) of a vector.
#'
#' @details An \code{NA} in the vector causes all returned values to be \code{NA}.
#'
#' @param x a numeric object.
#'
#' @return A numeric vector that contains the prior-to or reverse cumulative sums.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{cumsum}.
#'
#' @keywords misc
#'
#' @examples
#' cbind(vals=1:10,
#'       cum=cumsum(1:10),
#'       pcum=pcumsum(1:10),
#'       rcum=rcumsum(1:10))
#'
#' @rdname rcumsum
#' @export
rcumsum <- function(x) {
  rev(cumsum(rev(x)))
}

#' @rdname rcumsum
#' @export
pcumsum <- function(x) {
  cumsum(x)-x
}
