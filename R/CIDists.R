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
#' @note This is primarily a wrapper function for \code{\link[Hmisc]{binconf}} in the \pkg{Hmisc} package (this implementation uses arguments, specificially \code{conf.level}, that more closely match other functions).
#'
#' @param x A single or vector of numbers that contains the number of observed successes.
#' @param n A single or vector of numbers that contains the sample size.
#' @param conf.level A single number that indicates the level of confidence (default is \code{0.95}).
#' @param type A string that identifies the type of method to use for the calculations.  See details.
#' 
#' @return A #x2 matrix that contains the lower and upper confidence interval bounds as columns.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link{binom.test}}, \code{\link[Hmisc]{binconf}} in \pkg{Hmisc}, \code{\link[epitools]{binom.conf.int}} in \pkg{epitools}, and functions in \pkg{binom}.
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
#' ## Demonstrates using all types at once
#' binCI(7,20,type="all")
#'
#' ## Demonstrates use with multiple inputs
#' binCI(c(7,10),c(20,30))
#'
#' @export
binCI <- function(x,n,conf.level=0.95,type=c("wilson","exact","asymptotic","all")) {
  type <- match.arg(type)
  if (!is.vector(x)) STOP("'x' must be a single numeric or a vector of numerics.")
  if (type=="all" & length(x)>1) {
    WARN("'type=all' will not work with vectors; 'type' re-set to 'wilson'.")
    type <- "wilson"
  }
  # deletes point estimate value
  res <- Hmisc::binconf(x,n,alpha=1-conf.level,method=type)[,-1]
  if (is.vector(res)) {
    # convert to 1x2 matrix if only one set of CIs
    res <- rbind(res)
    rownames(res) <- ""
  }
  colnames(res) <- iCILabel(conf.level)
  res
}




#' @title Confidence interval for population size (N) in hypergeometric distribution.
#'
#' @description Computes a confidence interval for population size (N) in hypergeometric distribution.
#'
#' @details This is an inefficient brute-force algorithm.  The algorithm computes the \code{conf.level} range of possible values for \code{m}, as if it was unknown, for a large range of values of N.  It then finds all possible values of N for which \code{m} was in the \code{conf.level} range.  The smallest and largest values of N for which \code{m} was in the \code{conf.level} range are the CI endpoints.
#'
#' @note This algorithm is experimental at this point.
#'
#' @param M Number of successes in the population.
#' @param n Number of observations in the sample.
#' @param m Number of observed successes in the sample.
#' @param conf.level Level of confidence to use for constructing confidence intervals (default is \code{0.95}).
#'
#' @return A 1x2 matrix that contains the lower and upper confidence interval bounds.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @keywords htest
#'
#' @examples
#' hyperCI(50,25,10)
#'
#' @export
hyperCI <- function(M,n,m,conf.level=0.95) {
  N.low <- (n+(M-m))
  while (stats::qhyper((1-conf.level)/2,n,N.low-n,M) > m) { N.low <- N.low + 1 }
  N.hi <- (n*M)/m
  while (stats::qhyper(1-((1-conf.level)/2),n,N.hi-n,M) >= m) { N.hi <- N.hi + 1 }
  res <- round(cbind(N.low,N.hi),0)
  colnames(res) <- iCILabel(conf.level)
  res
}




#' @title Confidence interval for Poisson rate parameter.
#'
#' @description Computes a confidence interval for the Poisson mean rate parameter.
#'
#' @details Computes a CI for the Poisson mean using the method described in Ulm (1990), though this method was earlier described by Liddell (1984) and possibly Garwood (1936) as noted in van der Gulden and Verbeck (1992).  Thank you to Jerry Lewis for clarifications to the historical citations of this method.
#'
#' @param x A number representing the number of observed successes.
#' @param conf.level A number that indicates the level of confidence to use for constructing confidence intervals (default is \code{0.95}).
#'
#' @return A 1x2 matrix that contains the lower and upper confidence interval bounds.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link[epitools]{pois.exact}} in \pkg{epitools} for similar functionality.
#'
#' @references Garwood, F.  1936.  Fiducial limits for the Poisson distribution.  Biometrika.  28(3/4):437-442.
#'
#' Liddell, F.D.  1984.  Simple exact analysis of the standardised mortality ratio.  Journal of Epidemiology and Community Health. 38(1):85-88.
#'
#' Ulm, K.  1990.  A simple method to calculate the confidence interval of a standardized mortality ratio.  American Journal of Epidemiology 131(2):373-375.
#'
#' vand der Gulden, J.W.J. and A.L.M. Verbeck.  1992.  Re: \dQuote{A simple method to calculate the confidence interval of a standardized mortality ratio (SMR)}.  American Journal of Epidemiology 136(9):1170-1171.
#'
#' @keywords htest
#'
#' @examples
#' poiCI(12)
#'
#' @export
poiCI <- function(x,conf.level=0.95) {
  LCI <- stats::qchisq((1-conf.level)/2,2*x)/2
  UCI <- stats::qchisq(1-(1-conf.level)/2,2*(x+1))/2
  res <- cbind(LCI,UCI)
  colnames(res) <- iCILabel(conf.level)
  res
}
