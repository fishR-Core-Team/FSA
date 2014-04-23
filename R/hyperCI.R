#'Confidence interval for population size (N) in hypergeometric distribution.
#'
#'Computes a confidence interval for population size (N) in hypergeometric distribution.
#'
#'This is an inefficient brute-force algorithm.  The algorithm computes the
#'\code{conf.level} range of possible values for \code{m}, as if it was
#'unknown, for a large range of values of N.  It then finds all possible values
#'of N for which \code{m} was in the \code{conf.level} range.  The smallest and
#'largest values of N for which \code{m} was in the \code{conf.level} range are
#'the CI endpoints.
#'
#'@note This algorithm is extremely experimental at this point.
#'@param M Number of successes in the population.
#'@param n Number of observations in the sample.
#'@param m Number of observed successes in the sample.
#'@param conf.level Level of confidence to use for constructing confidence
#'intervals (default is \code{0.95}).
#'@return A 1x2 matrix that contains the lower and upper confidence interval bounds.
#'@export
#'@keywords htest
#'@examples
#'hyperCI(50,25,10)
#'
hyperCI <- function(M,n,m,conf.level=0.95) {
  N.low <- (n+(M-m))
  while (qhyper((1-conf.level)/2,n,N.low-n,M) > m) { N.low <- N.low + 1 }
  N.hi <- (n*M)/m
  while (qhyper(1-((1-conf.level)/2),n,N.hi-n,M) >= m) { N.hi <- N.hi + 1 }
  res <- round(cbind(N.low,N.hi),0)
  colnames(res) <- ciLabel(conf.level)
  res
}
