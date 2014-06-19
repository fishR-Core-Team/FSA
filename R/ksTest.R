#' @title Kolmogorov-Smirnov Tests.
#'
#' @description Performs a one- or two-sample Kolmogorov-Smirnov test.  Includes the option to perform a two-sample test using the formula notation.
#'
#' @details This function was created to allow the use of a formula in the two-sample situation.  The default version is simply a pass through to \code{ks.test}.  Thus, see \code{?ks.test} for more details.
#'
#' @aliases ksTest ksTest.default ksTest.formula
#'
#' @param x A numeric vector of data values or a formula (see details).
#' @param y Either a numeric vector of data values, or a character string naming a cumulative distribution function or an actual cumulative distribution function.  See \code{?ks.test}.
#' @param \dots Parameters of the distribution specified (as a character string) by \code{y}.
#' @param alternative A string that indicates the alternative hypothesis.  See \code{?ks.test}.
#' @param exact \code{NULL} or a logical that indicates whether an exact p-value should be computed. See \code{?ks.test}.  Not available if ties are present, nor for the one-sided two-sample case.
#' @param data A data frame that contains the variables in the formula for \code{x}.
#'
#' @return See \code{?ks.test}.
#'
#' @seealso \code{\link{ks.test}}.
#'
#' @keywords htest
#'
#' @examples
#' ## see ?ks.test for other examples
#' ## first two-sample example in ?ks.test
#' x <- rnorm(50)
#' y <- runif(30)
#' ksTest(x,y)
#'
#' ## same as above but using formulas
#' df <- data.frame(dat=c(x,y),grp=rep(c("X","Y"),c(50,30)))
#' str(df)
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
  alternative <- match.arg(alternative)
  ks.test(x,y,...,alternative=alternative,exact=exact)
}

#' @rdname ksTest
#' @export
ksTest.formula <- function(x,data=NULL,...,alternative=c("two.sided","less","greater"),exact=NULL) {
  alternative <- match.arg(alternative)
  DF <- model.frame(x,data=data)
  if (dim(DF)[2]>3) stop("ks.test.formula only works with one quantitative variable on LHS and one factor variable on RHS of formula.",call.=FALSE)
  if (attr(attr(DF, "terms"),"dataClasses")[1]!="numeric") stop("LHS of formula must be a numeric vector.",call.=FALSE)
  if (attr(attr(DF, "terms"),"dataClasses")[2]!="factor") stop("Variable on RHS of formula must be a factor.",call.=FALSE)
  if (length(levels(DF[,2]))!=2) stop("ks.test.formula only works if the factor variable on RHS has two levels.",call.=FALSE)
  DF.split <- split(DF[[1]],DF[[2]])
  ksTest.default(DF.split[[1]],DF.split[[2]],...,alternative=alternative,exact=exact)
}
