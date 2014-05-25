#' Ratio of lagged observations.
#'
#' Computes the ratio of lagged observations in a vector.
#'
#' This function behaves similarly to \code{diff()} except that it returns a vector or matrix of ratios rather than differences.
#'
#' @param x A numeric vector or matrix.
#' @param lag An integer representing the lag \sQuote{distance}.
#' @param differences An integer describing the direction of calculation.  A \code{1} indicates that \sQuote{future} values are divided by \sQuote{past} values.  A \code{2} is the opposite; \sQuote{past} values are divided by \sQuote{future} values.  See examples.
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
#'## same examples as in diff()
#'lagratio(1:10, 2)
#'lagratio(1:10, 2, 2)
#'x <- cumsum(cumsum(1:10))
#'lagratio(x, lag = 2)
#'lagratio(x, differences = 2)
#'
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#'
#' ## This will result in an error, POSIXt objects don't support logs which lagratio uses
#' lagratio(.leap.seconds)
#' 
#' } ## END IF INTERACTIVE MODE
#'
#' @export
#'
lagratio <- function(x,lag=1L,differences=1L,...) {
  if(any(x==0)) stop("Will not work with zeroes in x",call.=FALSE)
  if(any(class(x) %in% c("POSIXt","POSIXct"))) stop("Function does not work for 'POSIXt' objects.",call.=FALSE)
  exp(diff(log(x),lag=lag,differences=differences,...))
}
