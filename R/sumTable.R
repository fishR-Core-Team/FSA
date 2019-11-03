#' @title Creates a one- or two-way table of summary statistics.
#'
#' @description Creates a one- or two-way table of summary statistics for a quantitative variable.
#'
#' @details The formula must be of the form \code{quantitative~factor} or \code{quantitative~factor*factor2} where \code{quantitative} is the quantitative variable to construct the summaries for and \code{factor} and \code{factor2} are factor variables that contain the levels for which separate summaries should be constructed. If the variables on the right-hand-side are not factors, then they will be coerced to be factors and a warning will be issued.
#'
#' This function is largely a wrapper to \code{tapply()}, but only works for one quantitative variable on the left-hand-side and one or two factor variables on the right-hand-side. Consider using \code{\link[base]{tapply}} for situations with more factors on the right-hand-side.
#'
#' @aliases sumTable sumTable.formula
#'
#' @param formula A formula with a quantitative variable on the left-hand-side and one or two factor variables on the right-hand-side. See details.
#' @param data An optional data frame that contains the variables in \code{formula}.
#' @param FUN A scalar function that identifies the summary statistics. Applied to the quantitative variable for all data subsets identified by the combination of the factor(s). Defaults to \code{mean}.
#' @param digits A single numeric that indicates the number of digits to be used for the result.
#' @param \dots Other arguments to pass through to \code{FUN}.
#'
#' @return A one-way array of values if only one factor variable is supplied on the right-hand-side of \code{formula}. A two-way matrix of values if two factor variables are supplied on the right-hand-side of \code{formula}. These are the same classes of objects returned by \code{\link[base]{tapply}}.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link[base]{tapply}} for a more general implementation. See \code{\link{Summarize}} for a similar computation when only one factor variable is given.
#'
#' @keywords hplot
#'
#' @examples
#' ## The same examples as in the old aggregate.table in gdata package
#' ## but data in data.frame to illustrate formula notation
#' d <- data.frame(g1=sample(letters[1:5], 1000, replace=TRUE),
#'                 g2=sample(LETTERS[1:3], 1000, replace=TRUE),
#'                 dat=rnorm(1000))
#'
#' sumTable(dat~g1*g2,data=d,FUN=length)       # get sample size
#' sumTable(dat~g1*g2,data=d,FUN=validn)       # get sample size (better way)
#' sumTable(dat~g1*g2,data=d,FUN=mean)         # get mean
#' sumTable(dat~g1*g2,data=d,FUN=sd)           # get sd
#' sumTable(dat~g1*g2,data=d,FUN=sd,digits=1)  # show digits= argument
#'
#' ## Also demonstrate use in the 1-way example -- but see Summarize()
#' sumTable(dat~g1,data=d,FUN=validn)
#' sumTable(dat~g1,data=d,FUN=mean)
#' 
#' ## Example with a missing value (compare to above)
#' d$dat[1] <- NA
#' sumTable(dat~g1,data=d,FUN=validn)  # note use of validn
#' sumTable(dat~g1,data=d,FUN=mean,na.rm=TRUE)
#'
#' @rdname sumTable
#' @export
sumTable <- function (formula, ...) {
  UseMethod("sumTable") 
}

#' @rdname sumTable
#' @export
sumTable.formula <- function(formula,data=NULL,FUN=mean,
                             digits=getOption("digits"),...) {
  tmp <- iHndlFormula(formula,data,expNumR=1)
  ## Make sure that this function can handle the request
  if (tmp$vnum<2 | tmp$vnum>3)
    STOP("'sumTable' requires one quantitative variable on LHS and\n one or two factor variables on RHS of formula.")
  ## Handle the response variable
  if (!tmp$metExpNumR)
    STOP("'sumTable' only works with quantitative variable on LHS of formula")
  nv <- tmp$mf[,tmp$Rpos]
  ## Handle the explanatory variables
  if (tmp$Enum==1) { ## Only one explanatory variable
    rv <- tmp$mf[,tmp$Enames]
    if (tmp$Eclass!="factor") {
      WARN("RHS variable was converted to a factor.")
      rv <- factor(rv)
    }
    res <- tapply(nv,rv,FUN=FUN,...)
  } else { ## Two explanatory variables
    rv <- tmp$mf[,tmp$Enames[1]]
    if (tmp$Eclass[1]!="factor") {
      WARN("First RHS variable was converted to a factor.")
      rv <- factor(rv)
    }
    cv <- tmp$mf[,tmp$Enames[2]]
    if (tmp$Eclass[2]!="factor") {
      WARN("Second RHS variable was converted to a factor.")
      cv <- factor(cv)
    }
    res <- tapply(nv,list(rv,cv),FUN=FUN,...)
  }
  round(res,digits)
}  
