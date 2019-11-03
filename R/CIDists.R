#' @title Confidence intervals for binomial probability of success.
#'
#' @description Uses one of three methods to compute a confidence interval for the probability of success (p) in a binomial distribution.
#'
#' @details This function will compute confidence interval for three possible methods chosen with the \code{type} argument.
#'
#' \tabular{ll}{
#'  \code{type="wilson"} \tab Wilson's (Journal of the American Statistical Association, 1927) confidence interval for a proportion. This is the score CI, based on inverting the asymptotic normal test using the null standard error. \cr
#'  \code{type="exact"} \tab Computes the Clopper/Pearson exact CI for a binomial success probability. \cr
#'  \code{type="asymptotic"} \tab This uses the normal distribution approximation. \cr
#' }
#'
#' Note that Agresti and Coull (2000) suggest that the Wilson interval is the preferred method and is, thus, the default \code{type}.
#'
#' @note This is primarily a wrapper function for \code{binom.exact}, \code{binom.wilson}, and \code{binom.approx} (documented in \code{\link[epitools]{binom.conf.int}}) from the \pkg{epitools} package.
#'
#' @param x A single or vector of numbers that contains the number of observed successes.
#' @param n A single or vector of numbers that contains the sample size.
#' @param conf.level A single number that indicates the level of confidence (default is \code{0.95}).
#' @param type A string that identifies the type of method to use for the calculations. See details.
#' @param verbose A logical that indicates whether \code{x}, \code{n}, and \code{x/n} should be included in the returned matrix (\code{=TRUE}) or not (\code{=FALSE}; DEFAULT).
#' 
#' @return A #x2 matrix that contains the lower and upper confidence interval bounds as columns and, if \code{verbose=TRUE} \code{x}, \code{n}, and \code{x/n} .
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link{binom.test}}; \code{binconf} in \pkg{Hmisc}; \code{binom.exact}, \code{binom.wilson}, and \code{binom.approx} documented in \code{\link[epitools]{binom.conf.int}} in \pkg{epitools}, and functions in \pkg{binom}.
#'
#' @references Agresti, A. and B.A. Coull. 1998. Approximate is better than \dQuote{exact} for interval estimation of binomial proportions. American Statistician, 52:119-126.
#'
#' @keywords htest
#'
#' @examples
#' ## All types at once
#' binCI(7,20)
#' 
#' ## Individual types
#' binCI(7,20,type="wilson")
#' binCI(7,20,type="exact")
#' binCI(7,20,type="asymptotic")
#' binCI(7,20,type="asymptotic",verbose=TRUE)
#' 
#' ## Multiple types
#' binCI(7,20,type=c("exact","asymptotic"))
#' binCI(7,20,type=c("exact","asymptotic"),verbose=TRUE)
#' 
#' ## Use with multiple inputs
#' binCI(c(7,10),c(20,30),type="wilson")
#' binCI(c(7,10),c(20,30),type="wilson",verbose=TRUE)
#'
#' @export
binCI <- function(x,n,conf.level=0.95,type=c("wilson","exact","asymptotic"),
                  verbose=FALSE) {
  type <- match.arg(type,several.ok=TRUE)
  if (!is.vector(x)) STOP("'x' must be a single or vector of whole numbers.")
  if (!is.vector(n)) STOP("'n' must be a single or vector of whole numbers.")
  if (!is.numeric(x)) STOP("'x' must be whole numbers.")
  if (!is.numeric(n)) STOP("'n' must be whole numbers.")
  if (!all(is.wholenumber(x))) STOP("'x' must be whole numbers.")
  if (!all(is.wholenumber(n))) STOP("'n' must be whole numbers.")
  if (any(x<0)) STOP("'x' must be non-negative.")
  if (any(n<0)) STOP("'n' must be non-negative.")
  if (any(x>n)) STOP("'x' must not be greater than 'n'.")
  if (conf.level<=0 | conf.level>=1) STOP("'conf.level' must be between 0 and 1.")
  ## Handle differently depending on number of xs given
  if (length(x)>1) {
    if (length(type)>1) {
      type <- type[1]
      WARN("Can't use multiple 'type's with multiple 'x's. Used only '",type,"'.")
    }
    switch(type,
           exact = { res <- epitools::binom.exact(x,n,conf.level) },
           wilson = { res <- epitools::binom.wilson(x,n,conf.level) },
           asymptotic = { res <- epitools::binom.approx(x,n,conf.level) })
  } else {
    res <- rbind(epitools::binom.exact(x,n,conf.level),
                 epitools::binom.wilson(x,n,conf.level),
                 epitools::binom.approx(x,n,conf.level))
    rownames(res) <- c("Exact","Wilson","Asymptotic")
    # reduce to selected types
    res <- res[rownames(res) %in% capFirst(type),]
  }
  # relabel CI columns, convert to matrix, drop unwanted columns
  names(res)[which(names(res) %in% c("lower","upper"))] <- iCILabel(conf.level)
  res <- as.matrix(res[,-6])
  # return everything if verbose=TRUE, otherwise just CI
  if (!verbose) {
    res <- res[,4:5,drop=FALSE]
    # remove rownname if only one type selected and not verbose
    if (length(type)==1) rownames(res) <- rep("",nrow(res))
  }
  res
}





#' @title Confidence interval for Poisson counts.
#'
#' @description Computes a confidence interval for the Poisson counts.
#'
#' @details Computes a CI for the Poisson counts using the \code{exact}, gamma distribution (\code{daly}`), Byar's (\code{byar}), or normal approximation (\code{asymptotic}) methods. This is largely a wrapper to \code{pois.exact}, \code{pois.daly}, \code{pois.byar}, and \code{pois.approx} functions documented in \code{\link[epitools]{pois.conf.int}}in \pkg{epitools}.
#'
#' @param x A single number or vector that represents the number of observed successes.
#' @param conf.level A number that indicates the level of confidence to use for constructing confidence intervals (default is \code{0.95}).
#' @param type A string that identifies the type of method to use for the calculations. See details.
#' @param verbose A logical that indicates whether \code{x} should be included in the returned matrix (\code{=TRUE}) or not (\code{=FALSE}; DEFAULT).
#'
#' @return A #x2 matrix that contains the lower and upper confidence interval bounds as columns and, if \code{verbose=TRUE} \code{x}.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{pois.exact}, \code{pois.daly}, \code{pois.byar}, and \code{pois.approx} (documented in \code{\link[epitools]{pois.conf.int}}) in \pkg{epitools} for more description and references.
#'
#' @keywords htest
#'
#' @examples
#' ## Demonstrates using all types at once
#' poiCI(12)
#' 
#' ## Selecting types
#' poiCI(12,type="daly")
#' poiCI(12,type="byar")
#' poiCI(12,type="asymptotic")
#' poiCI(12,type="asymptotic",verbose=TRUE)
#' poiCI(12,type=c("exact","daly"))
#' poiCI(12,type=c("exact","daly"),verbose=TRUE)
#' 
#' ## Demonstrates use with multiple inputs
#' poiCI(c(7,10),type="exact")
#' poiCI(c(7,10),type="exact",verbose=TRUE)
#' 
#' @export
poiCI <- function(x,conf.level=0.95,type=c("exact","daly","byar","asymptotic"),
                  verbose=FALSE) {
  type <- match.arg(type,several.ok=TRUE)
  if (!is.vector(x)) STOP("'x' must be a single or vector of whole numbers.")
  if (!is.numeric(x)) STOP("'x' must be a whole number.")
  if (!all(is.wholenumber(x))) STOP("'x' must be a whole number.")
  if (any(x<0)) STOP("'x' must be non-negative.")
  if (conf.level<=0 | conf.level>=1) STOP("'conf.level' must be between 0 and 1.")
  ## Handle differently depending on number of xs given
  if (length(x)>1) {
    if (length(type)>1) {
      type <- type[1]
      WARN("Can't use multiple 'type's with multiple 'x's. Used only '",type,"'.")
    }
    switch(type,
           exact = { res <- epitools::pois.exact(x,1,conf.level) },
           daly  = { res <- epitools::pois.daly(x,1,conf.level) },
           byar  = { res <- epitools::pois.byar(x,1,conf.level) },
           asymptotic = { res <- epitools::pois.approx(x,1,conf.level) })
  } else {
    res <- rbind(epitools::pois.exact(x,1,conf.level),
                 epitools::pois.daly(x,1,conf.level),
                 epitools::pois.byar(x,1,conf.level),
                 epitools::pois.approx(x,1,conf.level))
    rownames(res) <- c("Exact","Daly","Byar","Asymptotic")
    # reduce to selected types
    res <- res[rownames(res) %in% capFirst(type),]
  }
  # relabel CI columns, convert to matrix, drop unwanted columns
  names(res)[which(names(res) %in% c("lower","upper"))] <- iCILabel(conf.level)
  res <- as.matrix(res[,-c(2,3,6)])
  # return everything if verbose=TRUE, otherwise just CI
  if (!verbose) {
    res <- res[,2:3,drop=FALSE]
    # remove rownname if only one type selected and not verbose
    if (length(type)==1) rownames(res) <- rep("",nrow(res))
  }
  res
}




#' @title Confidence interval for population size (N) in hypergeometric distribution.
#'
#' @description Computes a confidence interval for population size (N) in hypergeometric distribution.
#'
#' @details This is an inefficient brute-force algorithm. The algorithm computes the \code{conf.level} range of possible values for \code{m}, as if it was unknown, for a large range of values of N. It then finds all possible values of N for which \code{m} was in the \code{conf.level} range. The smallest and largest values of N for which \code{m} was in the \code{conf.level} range are the CI endpoints.
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
  if (any(length(M)!=1,length(n)!=1,length(m)!=1)) STOP("'M','n', and 'm' must all be a single value.")
  if (!is.numeric(c(M,n,m))) STOP("'M', 'n', and 'm' must all be whole numbers.")
  if (!all(is.wholenumber(M))) STOP("'M' must be a whole number.")
  if (!all(is.wholenumber(n))) STOP("'n' must be a whole number.")
  if (!all(is.wholenumber(m))) STOP("'m' must be a whole number.")
  if (any(c(M,n,m)<1)) STOP("'M', 'n', and 'm' must all be non-negative.")
  if (m>n) STOP("'m' must be less than 'n'.")
  if (m>M) STOP("'m' must be less than 'M'.")
  if (conf.level<=0 | conf.level>=1) STOP("'conf.level' must be between 0 and 1.")
  N.low <- (n+(M-m))
  while (stats::qhyper((1-conf.level)/2,n,N.low-n,M) > m) { N.low <- N.low + 1 }
  N.hi <- (n*M)/m
  while (stats::qhyper(1-((1-conf.level)/2),n,N.hi-n,M) >= m) { N.hi <- N.hi + 1 }
  res <- round(cbind(N.low,N.hi),0)
  colnames(res) <- iCILabel(conf.level)
  res
}
