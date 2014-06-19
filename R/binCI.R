#' @title Confidence intervals for binomial probability of success.
#'
#' @description Uses one of three methods to compute a confidence interval for the probability of success (p) in a binomial distribution.
#'
#' @details This function will compute confidence interval for three possible methods chosen with the \code{type} argument.
#'
#' \tabular{ll}{
#'  \code{type="wilson"} \tab Wilson's (Journal of the American Statistical Association, 1927) confidence interval for a proportion.  This is the score CI, based on inverting the asymptotic normal test using the null standard error. \cr
#'  \code{type="exact"} \tab Computes the Clopper/Pearson exact CI for a binomial success probability. \cr
#'  \code{type="asymptotic"} \tab This uses the normal distribution approximation. \cr
#' }
#'
#' Note that Agresti and Coull (2000) suggest that the Wilson interval is the preferred method and is, thus, the default \code{type}.
#'
#' @note This is primarily a wrapper function for \code{binconf} in the \pkg{Hmisc} package (this implementation uses arguments, specificially \code{conf.level}, that more closely match other functions).
#'
#' @param x A single or vector of numbers that contains the number of observed successes.
#' @param n A single or vector of numbers that contains the sample size.
#' @param conf.level A single number that indicates the level of confidence (default is \code{0.95}).
#' @param type A string that identifies the type of method to use for the calculations.  See details.
#' 
#' @return A #x2 matrix that contains the lower and upper confidence interval bounds as columns.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{binconf} in \pkg{Hmisc}, \code{bin.conf.int} in \pkg{epitools}, and \code{binom.conf.interval} in \pkg{UCS}.
#'
#' @references Agresti, A. and B.A. Coull.  1998.  Approximate is better than \dQuote{exact} for interval estimation of binomial proportions.  American Statistician, 52:119-126.
#'
#' @keywords htest
#'
#' @examples
#' binCI(7,20,type="wilson")
#' binCI(7,20,type="exact")
#' binCI(7,20,type="asymptotic")
#'
#' ## Demonstrates using all methods at once
#' binCI(7,20,type="all")
#'
#' ## Demonstrates use with multiple inputs
#' binCI(c(7,10),c(20,30))
#'
#' @export
binCI <- function(x,n,conf.level=0.95,type=c("wilson","exact","asymptotic","all")) {
  type <- match.arg(type)
  if (!is.vector(x)) stop("First argument must be a single numeric or a vector of numerics.",call.=FALSE)
  if (type=="all" & length(x)>1) {
    type <- "wilson"
    warning("method=all will not work with vectors...setting method to wilson",call.=FALSE)
  }
  res <- Hmisc::binconf(x,n,alpha=1-conf.level,method=type)[,-1]     # deletes point estimate value
  if (is.vector(res)) {
    res <- rbind(res)                                                # convert to 1x2 matrix if only one set of CIs
    rownames(res) <- ""
  }
  colnames(res) <- iCILabel(conf.level)
  res
}
