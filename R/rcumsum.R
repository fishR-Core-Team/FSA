#'Computes the reverse cumulative sum of a vector.
#'
#'Computes the reverse cumulative sum (i.e., the number that large or larger)
#'of a vector.
#'
#'An \code{NA} in the vector causes all returned values to be \code{NA}.
#'
#'@param x a numeric object.
#'@return A numeric vector that contains the reverse cumulative sums.
#'@seealso \code{cumsum}.
#'@export
#'@keywords misc
#'@examples
#'  cumsum(1:10)
#'  rcumsum(1:10)
#'
rcumsum <- function(x) {
  cs <- cumsum(x)                        # cumulative sum
  ttl <- rep(cs[length(cs)],length(cs))  # total number repeated through vector
  x+ttl-cs                               # reverse cumulative sum
}
