#'Computes the reverse cumulative sum of a vector.
#'
#'Computes the reverse cumulative sum (i.e., the number that large or larger) of a vector.
#'
#'An \code{NA} in the vector causes all returned values to be \code{NA}.
#'
#' @param x a numeric object.
#'
#' @return A numeric vector that contains the reverse cumulative sums.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{cumsum}.
#'
#' @keywords misc
#'
#' @examples
#'cumsum(1:10)
#'rcumsum(1:10)
#'
#' @export
rcumsum <- function(x) {
  # cumulative sum
  cs <- cumsum(x)
  # total number repeated through vector
  ttl <- rep(cs[length(cs)],length(cs))
  # reverse cumulative sum
  x+ttl-cs
}
