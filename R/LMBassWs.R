#'Length-weight regression results for computing Ws for largemouth bass.
#'
#'Length-weight regression results from a variety of populations used for
#'computing the standard weight (Ws) equation for largemouth bass
#'(\emph{Micropterus dolomieu}).
#'
#'Each row contains the intercept (\code{log.a}) and slope (\code{b}) results
#'from fitting the \eqn{log_{10}(W) = log_{10}(a) + b log_{10}(L)} linear
#'regression model to a population of \code{n} fish from the given location and
#'state.  Note that \eqn{W} is weight in grams and \eqn{L} is length in mm.
#'
#'@name LMBassWs
#'@docType data
#'@format A data frame with 16 observations on the following 5 variables:
#'\describe{ 
#'  \item{site}{Location of sample.}
#'  \item{state}{State where location is located.}
#'  \item{n}{Sample size in regression.}
#'  \item{log.a}{Intercept of regression.} 
#'  \item{b}{Slope of regression.} 
#'}
#'@section Topic(s): \itemize{ 
#' \item Relative weight
#' \item Standard weight
#' \item Length-weight
#'}
#'@concept Condition 'Relative Weight' 'Standard Weight' 'Length-Weight'
#'@seealso \code{\link{WSlit}} and \code{\link{wsVal}} and \code{\link{rlp}}.
#'@source From Table 1 in Murphy, B.R., M.L. Brown, and T.A. Springer.  1990.
#'Evaluation of the relative weight (Wr) index, with new applications to
#'walleye.  North American Journal of Fisheries Management, 10:85-97.
#'@keywords datasets
#'@examples
#'data(LMBassWs)
#'str(LMBassWs)
#'head(LMBassWs)
#'## See example for rlp() function
#'
NULL
