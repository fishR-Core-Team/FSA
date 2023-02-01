#' @title Dunn's Kruskal-Wallis Multiple Comparisons.
#'
#' @description Performs Dunn's (1964) test of multiple comparisons following a significant Kruskal-Wallis test, possibly with a correction to control the experimentwise error rate. This is largely a wrapper for the \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test}. Please see and cite that package.
#' 
#' @details This function performs \dQuote{Dunn's} test of multiple comparisons following a Kruskal-Wallis test. Unadjusted one- or two-sided p-values for each pairwise comparison among groups are computed following Dunn's description as implemented in the \code{\link[dunn.test]{dunn.test}} function from \pkg{dunn.test}. These p-values may be adjusted using methods in the \code{p.adjustment.methods} function in \pkg{dunn.test}.
#'
#' This function is largely a wrapper for the \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test}. Changes here are the possible use of formula notation, results not printed by the main function (but are printed in a more useful format (in my opinion) by the \code{print} function), the p-values are adjusted by default with the \dQuote{holm} method, and two-sided p-values are returned by default. See \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test} for more details underlying these computations.
#' 
#' @note The data.frame will be reduced to only those rows that are complete cases for \code{x} and \code{g}. In other words, rows with missing data for either \code{x} or \code{g} are removed from the analysis and a warning will be issued.
#' 
#' There are a number of functions in other packages that do similar analyses.
#' 
#' The results from \code{DunnTest} match the results (in a different format) from the \code{\link[dunn.test]{dunn.test}} function from \pkg{dunn.test}.
#' 
#' The \code{pairw.kw} function from the \pkg{asbio} package performs the Dunn test with the Bonferroni correction. The \code{pairw.kw} also provides a confidence interval for the difference in mean ranks between pairs of groups. The p-value results from \code{DunnTest} match the results from \code{pairw.kw}.
#' 
#' The \code{posthoc.kruskal.nemenyi.test} function from the \pkg{PMCMR} package uses the \dQuote{Nemenyi} (1963) method of multiple comparisons.
#' 
#' The \code{kruskalmc} function from the \pkg{pgirmess} package uses the method described by Siegel and Castellan (1988).
#' 
#' It is not clear which method \code{kruskal} from the \pkg{agricolae} package uses. It does not seem to output p-values but it does allow for a wide variety of methods to control the experimentwise error rate. 
#'
#' @aliases dunnTest dunnTest.default dunnTest.formula
#' 
#' @param x A numeric vector of data values or a formula of the form x~g.
#' @param g A factor vector or a (non-numeric) vector that can be coerced to a factor vector.
#' @param data A data.frame that minimally contains \code{x} and \code{g}.
#' @param method A single string that identifies the method used to control the experimentwise error rate. See the list of methods in \code{p.adjustment.methods} (documented with \code{\link[dunn.test]{dunn.test}}) in \pkg{dunn.test}.
#' @param two.sided A single logical that indicates whether a two-sided p-value should be returned (\code{TRUE}; default) or not. See details.
#' @param altp Same as \code{two.sided}. Allows similar code with the \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test}. \code{two.sided} is maintained because it pre-dates \code{altp}.
#' @param dunn.test.results A single logical that indicates whether the results that would have been printed by \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test} are shown.
#' @param \dots Not yet used.
#'
#' @return A list with three items -- \code{method} is the long name of the method used to control the experimentwise error rate, \code{dtres} is the strings that would have been printed by the \code{\link[dunn.test]{dunn.test}} function in \pkg{dunn.test}, and \code{res} is a data.frame with the following variables:
#' \itemize{
#'   \item Comparison: Labels for each pairwise comparison.
#'   \item Z: Values for the Z test statistic for each comparison.
#'   \item P.unadj: Unadjusted p-values for each comparison.
#'   \item P.adj: Adjusted p-values for each comparison.
#' }
#'
#' @seealso See \code{kruskal.test}, \code{\link[dunn.test]{dunn.test}} in \pkg{dunn.test}, \code{posthoc.kruskal.nemenyi.test} in \pkg{PMCMR}, \code{kruskalmc} in \pkg{pgirmess}, and \code{kruskal} in \pkg{agricolae}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}, but this is largely a wrapper (see details) for \code{\link[dunn.test]{dunn.test}} in \pkg{dunn.test} written by Alexis Dinno.
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
#' ponds2 <- ponds[complete.cases(ponds),]
#' 
#' # non-formula usage (default "holm" method)
#' dunnTest(ponds2$pH,ponds2$pond)
#' 
#' # formula usage (default "holm" method)
#' dunnTest(pH~pond,data=ponds2)
#' 
#' # other methods
#' dunnTest(pH~pond,data=ponds2,method="bonferroni")
#' dunnTest(pH~pond,data=ponds2,method="bh")
#' dunnTest(pH~pond,data=ponds2,method="none")
#' 
#' # one-sided
#' dunnTest(pH~pond,data=ponds2,two.sided=FALSE)
#' 
#' # warning message if incomplete cases were removed
#' dunnTest(pH~pond,data=ponds)
#' 
#' # print dunn.test results
#' tmp <- dunnTest(pH~pond,data=ponds2)
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
                             two.sided=TRUE,altp=two.sided,...) {
  ## check method type and get long name for the p-value adjustment method
  method <- match.arg(method)
  adjNAMES <- c("Holm","Bonferroni","Sidak","Holm-Sidak","Hochberg",
                "Benjamini-Hochberg","Benjamini-Yekuteili","No Adjustment")
  Name <- adjNAMES[which(dunn.test::p.adjustment.methods[c(4,2:3,5:8,1)]==method)]
  
  ## check variable types
  if (!is.numeric(x)) STOP("'x' must be numeric.")
  if (!is.factor(g)) {
    if (!(is.integer(g)|is.character(g)))
      STOP("'g' must be coerceable to a factor.")
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
  if (!requireNamespace("dunn.test"))
    STOP("'dunnTest' requires the 'dunn.test' package to be installed!")
  else {
    dtres <- utils::capture.output(res <- dunn.test::dunn.test(x,g,method,TRUE,
                                                               altp=altp,...))
    # convert returned list to data.frame (without chi-square value)
    # and reorder and rename the columns
    res <- data.frame(res[-which(names(res)=="chi2")])[,c(4,1,2,3)]
    names(res) <- c("Comparison","Z","P.unadj","P.adj")
    # return a list
    tmp <- list(method=Name,res=res,dtres=dtres)
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
                             two.sided=TRUE,altp=two.sided,...) {
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
  dunnTest.default(d[,tmp$Rname],d[,tmp$Enames],method=method,altp=altp,...)
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
