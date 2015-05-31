#' @title Ratio of lagged observations.
#'
#' @description Computes the ratio of lagged observations in a vector.
#'
#' @details This function behaves similarly to \code{diff()} except that it returns a vector or matrix of ratios rather than differences.
#'
#' @param x A numeric vector or matrix.
#' @param lag An integer representing the lag \sQuote{distance}.
#' @param direction A string that indicates the direction of calculation.  A \code{"backward"} induicates that \sQuote{latter} values are divided by \sQuote{former} values.  A \code{"forward"} induicates that \sQuote{former} values are divided by \sQuote{latter} values.  See examples.
#' @param recursion An integeer that indicates the level of recursion for the calculations.  A \code{1} will simply compute the ratios.  A \code{2}, for example, will compute the ratios, save the result, and then compute the ratios of the results using the same \code{lag}.  See examples.
#' @param differences Same as \code{recursion}.  Used for symmetry with \code{\link[base]{diff}}.
#' @param \dots Additional arguments to \code{diff()}.
#'
#' @return A vector or matrix of lagged ratios.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{diff}
#'
#' @keywords manip
#'
#' @examples
#' ## Backward lagged ratios
#' # no recursion
#' lagratio(1:10,1)
#' lagratio(1:10,2)
#' # with recursion
#' lagratio(1:10,1,2)
#' lagratio(1:10,2,2)
#' 
#' ## Forward lagged ratios
#' # no recursion
#' lagratio(10:1,1,direction="forward")
#' lagratio(10:1,2,direction="forward")
#' # with recursion
#' lagratio(10:1,1,2,direction="forward")
#' lagratio(10:1,2,2,direction="forward")
#'
#' @export
#'
lagratio <- function(x,lag=1L,recursion=1L,differences=recursion,direction=c("backward","forward"),...) {
  ## Some checks
  direction <- match.arg(direction)
  if(any(x==0)) stop("Will not work with zeroes in 'x'.",call.=FALSE)
  if(any(class(x) %in% c("POSIXt","POSIXct"))) stop("Function does not work for 'POSIXt' objects.",call.=FALSE)
  if (!recursion>0) stop("'recursion' value must be >0.",call.=FALSE)
  ## Flip vector if ratio direction is forward
  if (direction=="forward") x <- rev(x)
  ## Compute lagged ratio
  res <- exp(diff(log(x),lag=lag,differences=differences,...))
  ## Flip the resulting vector if direction is forward
  if (direction=="forward") res <- rev(res)
  ## Return the result
  res
}
