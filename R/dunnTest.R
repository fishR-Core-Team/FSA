#' @title Dunn's Kruskal-Wallis Multiple Comparisons.
#'
#' @description Performs Dunn's (1964) test of multiple comparisons following a significant Kruskal-Wallis test, possibly with a correction to control the experimentwise error rate.
#' 
#' @details This function performs \dQuote{Dunn's} test of multiple comparisons following a Kruskal-Wallis test.  Unadjusted two-sided p-values for each pairwise comparison among groups are computed following Dunn's description as implemented in the \code{\link[dunn.test]{dunn.test}} function from \pkg{dunn.test}.  These p-values may be adjusted using the methods in the \code{\link[dunn.test]{p.adjustment.methods}} function in \pkg{dunn.test} as implemented in the \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test}.
#'
#' This function is largely a wrapper for the \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test}.  Changes here are the possible use of formula notation, results that are not printed by the main function (but are printed in a more useful format (in my opinion) by the \code{print} function), the p-values are adjusted by default with the \dQuote{holm} method, and two-sided p-values (two times the p-value produced by the \code{\link[dunn.test]{dunn.test}} function) are returned with \code{two.sided=TRUE}.  This function returns the same results (in a different format) as the \code{\link[dunn.test]{dunn.test}} function from \pkg{dunn.test} when \code{two.sided=FALSE}.  See \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test} for a more details underlying these computations.
#' 
#' @note The data.frame will be reduced to only those rows that are complete cases for \code{x} and \code{g}.  In other words, rows with missing data for either \code{x} or \code{g} are removed from the analysis.
#' 
#' There are a number of functions in other packages that purport to do similar analyses.
#' 
#' The \code{\link[asbio]{pairw.kw}} function from the \pkg{asbio} package performs the Dunn test with the Bonferroni correction.  The p-value results from \code{DunnTest} match the results from \code{\link[asbio]{pairw.kw}}.  The \code{\link[asbio]{pairw.kw}} also provides a confidence interval for the difference in mean ranks between pairs of groups.
#' 
#' The \code{\link[PMCMR]{posthoc.kruskal.nemenyi.test}} function from the \pkg{PMCMR} package uses the \dQuote{Nemenyi} (1963) method of multiple comparisons.
#' 
#' The \code{\link[pgirmess]{kruskalmc}} function from the \pkg{pgirmess} package uses the method described by Siegel and Castellan (1988).
#' 
#' It is not clear which method the \code{\link[agricolae]{kruskal}} function from the \pkg{agricolae} package uses.  It does not seem to output p-values but it does allow for a wide variety of methods to control the experimentwise error rate. 
#'
#' @aliases dunnTest dunnTest.default dunnTest.formula
#' 
#' @param x A numeric vector of data values or a formula of the form x~g.
#' @param g A factor vector or a (non-numeric) vector that can be coerced to a factor vector.
#' @param data A data.frame that minimally contains \code{x} and \code{g}.
#' @param method A single string that identifies the method to use to control the experimentwise error rate.  See the list of methods in \code{\link[dunn.test]{p.adjustment.methods}}.
#' @param two.sided A single logical that indicates whether a two-sided p-value should be returned (\code{TRUE}; default) or not.  See details.
#' @param dunn.test.results A single logical that indicates whether the results that would have been printed by \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test} are shown.
#' @param \dots Not yet used.
#'
#' @return A list with three items -- \code{method} is the long name of the method used to control the experimentwise error rate, \code{dtres} is the strings that would have been printed by \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test}, and a data.frame in \code{res} with the following variables:
#' \itemize{
#'   \item Comparison Labels for each pairwise comparison.
#'   \item Z Values for the Z test statistic for each comparison.
#'   \item P.unadj Unadjusted p-values for each comparison.
#'   \item P.adj Adjusted p-values for each comparison.
#' }
#'
#' @seealso See \code{\link{kruskal.test}}, \code{\link[dunn.test]{dunn.test}} in \pkg{dunn.test}, \code{\link[PMCMR]{posthoc.kruskal.nemenyi.test}} in \pkg{PMCMR}, \code{\link[pgirmess]{kruskalmc}} in \pkg{pgirmess}, and \code{\link[agricolae]{kruskal}} in \pkg{agricolae}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}, but this is largely a wrapper (see details) for \code{\link[dunn.test]{dunn.test}} in \pkg{dunn.test} written by Alexis Dinno.
#'
#' @references 
#' Dunn, O.J. 1964. Multiple comparisons using rank sums. Technometrics 6:241-252.
#' 
#' @examples
#' ## pH in four ponds data from Zar (2010)
#' ponds <- data.frame(pond=as.factor(rep(1:4,each=8)),
#'                     pH=c(7.68,7.69,7.70,7.70,7.72,7.73,7.73,7.76,
#'                          7.71,7.73,7.74,7.74,7.78,7.78,7.80,7.81,
#'                          7.74,7.75,7.77,7.78,7.80,7.81,7.84,NA,
#'                          7.71,7.71,7.74,7.79,7.81,7.85,7.87,7.91))
#' ponds <- ponds[complete.cases(ponds),]
#' 
#' # non-formula usage (default "holm" method)
#' dunnTest(ponds$pH,ponds$pond)
#' 
#' # formula usage (default "holm" method)
#' dunnTest(pH~pond,data=ponds)
#' 
#' # other methods
#' dunnTest(pH~pond,data=ponds,method="bonferroni")
#' dunnTest(pH~pond,data=ponds,method="bh")
#' dunnTest(pH~pond,data=ponds,method="none")
#' 
#' # print dunn.test results
#' tmp <- dunnTest(pH~pond,data=ponds)
#' print(tmp,dunn.test.results=TRUE)
#' 
#' @rdname dunnTest
#' @export
dunnTest <- function (x,...) {
  UseMethod("dunnTest") 
}

#' @rdname dunnTest
#' @export
dunnTest.default <- function(x,g,
                             method=dunn.test::p.adjustment.methods[c(4,2:3,5:8,1)],
                             two.sided=TRUE,...) {
  ## check method type and get long name for the p-value adjustment method
  method <- match.arg(method)
  adjNAMES <- c("Holm","Bonferroni","Sidak","Holm-Sidak","Hochberg","Benjamini-Hochberg","Benjamini-Yekuteili","No Adjustment")
  Name <- adjNAMES[which(dunn.test::p.adjustment.methods[c(4,2:3,5:8,1)]==method)]
  
  ## check variable types
  if (!is.numeric(x)) STOP("'x' must be numeric.")
  if (!is.factor(g)) {
    if (!(is.integer(g)|is.character(g))) STOP("'g' must be coerceable to a factor.")
    g <- as.factor(g)
    WARN("'g' variable was coerced to a factor.")
  }
  ## Find missing values in x and g, and remove
  ok <- stats::complete.cases(x,g)
  if (sum(!ok)>0) {
    WARN("Some rows deleted from 'x' and 'g' because missing data.")
    x <- x[ok]
    g <- g[ok]
  }
  
  ## MAIN CALCULATIONS (using dunn.test() from dunn.test package)
  # Result is in res, capture.output() is used to ignore the cat()ted
  # output from dunn.test(), which is in dtres
  if (!requireNamespace("dunn.test")) STOP("'dunnTest' requires the 'dunn.test' package to be installed!")
  else {
    dtres <- utils::capture.output(res <- dunn.test::dunn.test(x,g,method,TRUE,...))
    # return a list
    tmp <- list(method=Name,
                res=data.frame(Comparison=res$comparisons,Z=res$Z,
                               P.unadj=ifelse(two.sided,2,1)*res$P,
                               P.adj=ifelse(two.sided,2,1)*res$P.adjusted),
                dtres=dtres)
    pgt1 <- which(tmp$res$P.adj>1)
    if (length(pgt1)>0) tmp$res$P.adj[pgt1] <- 1
    class(tmp) <- "dunnTest"
    tmp
  }
}


#' @rdname dunnTest
#' @export
dunnTest.formula <- function(x,data=NULL,
                             method=dunn.test::p.adjustment.methods[c(4,2:3,5:8,1)],
                             two.sided=TRUE,...) {
  ## get the dataframe of just the two variables
  tmp <- iHndlFormula(x,data,expNumR=1,expNumE=1)
  d <- tmp$mf
  ## perform some simple checks on the formula and variables
  if (!tmp$metExpNumR) STOP("'dunnTest' must have only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer")) STOP("LHS variable must be numeric.")
  if (!tmp$metExpNumE) STOP("'dunnTest' must have only one RHS variable.")
  if (tmp$Eclass!="factor") {
    if (tmp$Eclass=="numeric") STOP("RHS variable must be a factor.")
    d[,tmp$Enames] <- as.factor(d[,tmp$Enames])
    WARN(tmp$Enames," was coerced to a factor.")
  }
  ## send the two variables to dunnTest.default
  dunnTest.default(d[,tmp$Rname],d[,tmp$Enames],method=method,two.sided=two.sided,...)
}

#' @rdname dunnTest
#' @export
print.dunnTest <- function(x,dunn.test.results=FALSE,...) { # nocov start
  if (!dunn.test.results) {
    message("Dunn (1964) Kruskal-Wallis multiple comparison")
    if (x$method=="No Adjustment") message("  with no adjustment for p-values.\n")
    else message("  p-values adjusted with the ",x$method," method.\n")
    print(x$res,...)
  } else {
    ## Prints the result as if it came from dunn.test() from dunn.test package
    cat(paste(x$dtres,"\n"))
  }
} # nocov end