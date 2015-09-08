#' @title Kolmogorov-Smirnov Tests.
#'
#' @description Performs a one- or two-sample Kolmogorov-Smirnov test.  Includes the option to perform the two-sample test using the formula notation.
#'
#' @details This is exactly \code{\link[stats]{ks.test}} except that a formula may be used for the two-sample situation.  The default version is simply a pass through to \code{\link[stats]{ks.test}}.  See \code{\link[stats]{ks.test}} for more details.
#'
#' @aliases ksTest ksTest.default ksTest.formula
#'
#' @param x A numeric vector of data values or a formula (see details).
#' @param y A numeric vector of data values, a character string naming a cumulative distribution function, or an actual cumulative distribution function.  See \code{\link[stats]{ks.test}}.
#' @param \dots Parameters of the distribution specified (as a character string) by \code{y}.
#' @param alternative A string that indicates the alternative hypothesis.  See \code{\link[stats]{ks.test}}.
#' @param exact \code{NULL} or a logical that indicates whether an exact p-value should be computed. See \code{\link[stats]{ks.test}}.  Not available if ties are present, nor for the one-sided two-sample case.
#' @param data A data frame that contains the variables in the formula for \code{x}.
#'
#' @return See \code{\link[stats]{ks.test}}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso \code{\link[stats]{ks.test}}.
#'
#' @keywords htest
#'
#' @examples
#' ## see ks.test for other examples
#' x <- rnorm(50)
#' y <- runif(30)
#' df <- data.frame(dat=c(x,y),grp=rep(c("X","Y"),c(50,30)))
#' 
#' ## one-sample (from ks.test) still works
#' ksTest(x+2, "pgamma", 3, 2)
#' ks.test(x+2, "pgamma", 3, 2)
#' 
#' ## first two-sample example in ?ks.test
#' ksTest(x,y)
#' ks.test(x,y)
#' 
#' ## same as above but using data.frame and formula
#' ksTest(dat~grp,data=df)
#'
#' @rdname ksTest
#' @export
ksTest <- function (x, ...) {
  UseMethod("ksTest") 
}

#' @rdname ksTest
#' @export
ksTest.default <- function(x,y,...,alternative=c("two.sided","less","greater"),exact=NULL) {
  stats::ks.test(x,y,...,alternative=alternative,exact=exact)
}

#' @rdname ksTest
#' @export
ksTest.formula <- function(x,data=NULL,...,alternative=c("two.sided","less","greater"),exact=NULL) {
  ## Handle formula including checks
  tmp <- iHndlFormula(x,data,expNumR=1,expNumE=1,expNumEFacts=1)
  if (tmp$vnum!=2) stop("Formula for `ksTest' must contain 1 response (LHS) and 1 explanatory (RHS) variable.",call.=FALSE)
  if (!tmp$metExpNumR) stop("LHS of formula must contain only one variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("RHS of formula must contain only one variable.",call.=FALSE)
  if (!tmp$metExpNumEFacts) stop("RHS Variable must be a factor.",call.=FALSE)
  if (length(levels(tmp$mf[,tmp$EFactPos]))!=2) stop("`ksTest` only works if the RHS variable has two levels.",call.=FALSE)
  ## Split the data into the two groups
  DF.split <- split(tmp$mf[,tmp$Rpos],tmp$mf[,tmp$EFactPos])
  ## Send to ksTest.default which ultimately goes to stats::ks.test
  ksTest.default(DF.split[[1]],DF.split[[2]],...,alternative=alternative,exact=exact)
}
