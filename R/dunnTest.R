#' @title Dunn's Kruskal-Wallis Multiple Comparisons.
#'
#' @description Performs Dunn's (1964) test of multiple comparisons following a significant Kruskal-Wallis test, possibly with a correction to control the experimentwise error rate.
#' 
#' @details This function performs \dQuote{Dunn's} test of multiple comparisons following a Kruskal-Wallis test.  Unadjusted two-sided p-values for each pairwise comparison among groups are computed following Dunn's description.  These p-values may be adjusted with the methods of \code{p.adjust}.
#' 
#' @note There are a number of functions in other packages that purport to do similar analyses.
#' 
#' The \code{dunn.test} function in \pkg{dunn.test} performs the same calculations but does not use formula notation, does not produce two-sided p-values, and does provide a few other methods to correct for the experimentwise error rate.  Additionally, the results from \code{dunn.test} are printed automatically and in a format that I consider cumbersome.  The results from \code{dunnTest} match those from \code{dunn.test} if the p-values are adjusted to be two-sided.
#' 
#' The \code{pairw.kw} function from the \pkg{asbio} package performs the Dunn test with the Bonferroni correction.  The p-value results from \code{DunnTest} match the results from \code{pairw.kw}.  The \code{pairw.kw} also provides a confidence interval for the difference in mean ranks between pairs of groups.
#' 
#' The \code{posthoc.kruskal.nemenyi.test} function from the \pkg{PMCMR} package uses the \dQuote{Nemenyi} (1963) method of multiple comparisons.
#' 
#' The \code{kruskalmc} function from the \pkg{pgirmess} package uses the method described by Siegel and Castellan (1988).
#' 
#' It is not clear which method the \code{kruskal} function from the \pkg{agricolae} package uses.  It does not seem to output p-values but it does allow for a wide variety of methods to control the experimentise error rate. 
#'
#' @aliases dunnTest dunnTest.default dunnTest.formula
#' 
#' @param x A numeric vector of data values or a formula of the form x~g.
#' @param g A factor vector or a (non-numeric) vector that can be coerced to a factor vector.
#' @param data A data.frame that minimally contains \code{x} and \code{g}.
#' @param method A single string that identifies the method to use to control the experimentwise error rate.  See details in \code{p.adjust}.
#' @param \dots Not yet used.
#'
#' @return A list with two items -- \code{method} is the long name of the method used to control the experimentwise error rate and a data.frame in \code{res} with the following variables:
#' \itemize{
#'   \item Comparison Labels for each pairwise comparison.
#'   \item Z Values for the Z test statistic for each comparison.
#'   \item P.unadj Unadjusted p-values for each comparison.
#'   \item P.adj Adjusted p-values for each comparison.
#' }
#'
#' @seealso See \code{kruskal.test} and \code{dunn.test} in \pkg{dunn.test}, \code{posthoc.kruskal.nemenyi.test} in \pkg{PMCMR}, \code{kruskalmc} in \pkg{pgirmess}, and \code{kruskal} in \pkg{agricolae}.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, but learned a lot from \code{dunn.test} in \pkg{dunn.test}.
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
#' # non-formula usage (default "Holm" method)
#' dunnTest(ponds$pH,ponds$pond)
#' 
#' # formula usage (default "Holm" method)
#' dunnTest(pH~pond,data=ponds)
#' 
#' # other methods
#' dunnTest(pH~pond,data=ponds,method="bonferroni")
#' dunnTest(pH~pond,data=ponds,method="BH")
#' dunnTest(pH~pond,data=ponds,method="none")
#' 
#' @rdname dunnTest
#' @export
dunnTest <- function (x,...) {
  UseMethod("dunnTest") 
}

#' @rdname dunnTest
#' @export
dunnTest.default <- function(x,g,method=p.adjust.methods,...) {
  ## check method type and get long name for the p-value adjustment method
  method <- match.arg(method)
  adjNAMES <- c("Holm","Hochberg","Hommel","Bonferroni","Benjamini-Hochberg","Benjamini-Yekuteili","False Discovery Rate","No Adjustment")
  Name <- adjNAMES[which(p.adjust.methods==method)]

  ## check variable types
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  if (!is.factor(g)) {
    if (is.numeric(g)) stop("'g' must be coerceable to a factor.",call.=FALSE)
    g <- as.factor(g)
    warning("'g' variable was coerced to a factor.",call.=FALSE)
  }
  ## Find missing values in x and g, and remove
  ok <- complete.cases(x,g)
  if (sum(!ok)>0) {
    warning("Some rows deleted from 'x' and 'g' because missing data.",call.=FALSE)
    x <- x[ok]
    g <- g[ok]
  }

  ## MAIN CALCULATIONS (largely borrowed from dunn.test() in dunn.test package)
  # find some constants
  N <- length(x)
  lvls <- levels(g)
  k <- length(lvls)
  # make a data.frame
  d <- data.frame(x=x,g=g,ranks=rank(x,ties.method="average",na.last=NA) )
  Z <- lbls <- rep(NA,k*(k-1)/2)
  # calculate ties adjustment to be used in pooled variance estimate later
  tiesadj <- iTiesAdj(d$ranks)/(12*(N-1))
  # Generate differences in mean ranks and z statistic
  for (i in 2:k) {
    for (j in 1:(i-1)) {
      ni <- sum(d$g==lvls[i])
      nj <- sum(d$g==lvls[j])
      meanranki <- mean(d$ranks[d$g==lvls[i]])
      meanrankj <- mean(d$ranks[d$g==lvls[j]])
      z <- (meanrankj - meanranki) / sqrt( ((N*(N+1)/12) - tiesadj) * ((1/nj) + (1/ni)) )
      index <- ((i-2)*(i-1)/2) + j
      Z[index] <- z
      lbls[index] <- paste0(lvls[i],"-",lvls[j],"=0")
    }
  }
  # Compute unadjusted p-values (note that 2* is different than in dunn.test)
  P <- 2*pnorm(abs(Z),lower.tail=FALSE)
  # compute adjusted p-values
  P.adjust <- p.adjust(P,method=method)
  # return a list
  tmp <- list(method=Name,res=data.frame(Comparison=lbls,Z=Z,P.unadj=P,P.adj=P.adjust))
  class(tmp) <- "DunnTest"
  tmp
  }

#' @rdname dunnTest
#' @export
dunnTest.formula <- function(x,data=NULL,method=p.adjust.methods,...) {
  ## match the arguments
  method <- match.arg(method)
  ## get the dataframe of just the two variables
  tmp <- iHndlFormula(x,data,expNumR=1,expNumE=1)
  d <- tmp$mf
  ## perform some simple checks on the formula and variables
  if (!tmp$metExpNumR) stop("'dunnTest' must have only one LHS variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'dunnTest' must have only one RHS variable.",call.=FALSE)
  if (tmp$Eclass!="factor") {
    if (tmp$Eclass=="numeric") stop("RHS variable must be a factor.",call.=FALSE)
    d[,tmp$Enames] <- as.factor(d[,tmp$Enames])
    warning(tmp$Enames," was coerced to a factor.",call.=FALSE)
  }
  ## send the two variables to dunnTest.default
  dunnTest.default(d[,tmp$Rname],d[,tmp$Enames],method=method,...)
}

#' @rdname dunnTest
#' @method print DunnTest
#' @export
print.DunnTest <- function(x,...) {
  message("Dunn (1964) Kruskal-Wallis multiple comparison")
  if (x$method=="No Adjustment") message("  with no adjustment for p-values.\n")
    else message("  p-values adjusted with the ",x$method," method.\n")
  print(x$res,...)
}


### INTERNAL FUNCTIONS
iTiesAdj <- function(ranks) {## computes an adjustment for ties
  # finds ranks that are tied and returns those ranks
  tmp <- table(ranks)
  ties <- as.numeric(names(tmp)[which(tmp>1)])
  if (length(ties)==0) ties <- NULL
  # find the adjustment
  r <- length(ties)
  tiesadj <- 0
  if (r > 0) {
    for (s in 1:r) {
      tau <- sum(ranks==ties[s])
      tiesadj <- tiesadj + (tau^{3} - tau)
    }
  }
  tiesadj
}
