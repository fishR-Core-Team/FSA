#' @title Creates a one- or two-way table of summary statistics.
#'
#' @description Creates a one- or two-way table of summary statistics for a quantitative variable.
#'
#' @details The formula must be of the form \code{quantitative~factor} or \code{quantitative~factor*factor2} where \code{quantitative} is the quantitative variable to construct the summaries for and \code{factor} or \code{factor2} are factor variables that contain the levels for which separate summaries should be constructed.  If the variables on the right-hand-side are not factors then they will be coerced to be factors and a warning will be issued.
#'
#' This function currently only works for one quantitative variable on the left-hand-side and one or two factor variales on the right-hand-side.  Consider using \code{tapply} for situations with more factors on the right-hand-side.
#'
#' @note This function is largely a wrapper to \code{tapply()}.
#'
#' @aliases sumTable sumTable.formula
#'
#' @param formula A formula with a quantitative variable on the left-hand-side and one or two factor variables on the right-hand-side.
#' @param data An optional data frame that contains the variables in the formula.
#' @param FUN A scalar function to compute the summary statistics which can be applied to the quantitative variable for all data subsets identified by the combination of the factor(s).  Defaults to \code{mean}.
#' @param digits A single numeric that indicates the number of digits to be used for the result.
#' @param \dots Other arguments to pass through to \code{FUN}.
#'
#' @return A one-way array of values if only one factor variable is supplied on the right-hand-side of the formula.  A two-way matrix of values if two factor variables are supplied on the right-hand-side.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{tapply}.
#'
#' @keywords hplot
#'
#' @examples
#' ## The same examples as in the old aggregate.table in gdata package
#' g1 <- sample(letters[1:5], 1000, replace=TRUE)
#' g2 <- sample(LETTERS[1:3], 1000, replace=TRUE )
#' dat <- rnorm(1000)
#' d <- data.frame(g1,g2,dat)   # put data into a data.frame to illustrate formula notation
#'
#' sumTable(dat~g1*g2,data=d,FUN=length)       # get sample size
#' sumTable(dat~g1*g2,data=d,FUN=mean)         # get mean
#' sumTable(dat~g1*g2,data=d,FUN=sd)           # get sd
#' sumTable(dat~g1*g2,data=d,FUN=sd,digits=1)  # show digits= argument
#'
#' ## Also demonstrate use in the 1-way example -- but see Summarize() in FSA package
#' sumTable(dat~g1,data=d,FUN=length)
#' sumTable(dat~g1,data=d,FUN=mean)
#'
#' @rdname sumTable
#' @export
sumTable <- function (formula, ...) {
  UseMethod("sumTable") 
}

#' @rdname sumTable
#' @export
sumTable.formula <- function(formula,data=NULL,FUN=mean,digits=getOption("digits"),...) {
  DF <- stats::model.frame(formula,data=data)
  if (dim(DF)[2]>3) stop("sumTable.formula only works with one quantitative variable on LHS and one or two factor variables on RHS of formula.",call.=FALSE)
  if (attr(attr(DF, "terms"),"dataClasses")[1]!="numeric") stop("Left-hand-side of formula must be a numeric vector.",call.=FALSE)
  nv <- DF[,1]
  rv <- DF[,2]
  if (attr(attr(DF, "terms"),"dataClasses")[2]!="factor") {
    warning("RHS variable was converted to a factor.",call.=FALSE)
    rv <- factor(rv)
  }
  if (dim(DF)[2]==2) {  ## only one factor variable on RHS
    res <- tapply(nv,rv,FUN=FUN,...)
  } else {
    cv <- DF[,3]
    if (attr(attr(DF, "terms"),"dataClasses")[3]!="factor") {
      warning("RHS column variable was converted to a factor.",call.=FALSE)  
      cv <- factor(cv)
    }
    res <- tapply(nv,list(rv,cv),FUN=FUN,...)
  }
  round(res,digits)
}  
