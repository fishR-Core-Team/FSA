#' @title Create a histogram from a frequency table.
#'
#' @description Creates a histogram from values in a frequency table. Primarily used with already summarized length frequency data.
#'
#' @param x A numeric vector of bin/category values, a formula of the form \code{freq~cat} where \code{freq} contains the count/frequency values and \code{cat} contains the bin/category values, an object of class \code{table} from \code{table()} or \code{xtabs()}.
#' @param y A numeric vector of count/frequency values.
#' @param data A data.frame that contains the \code{freq} and \code{cat} variables if a formula is given in \code{x}.
#' @param \dots Additional arguments for \code{\link[graphics]{hist}}.
#'
#' @details Creates a histogram fro values in a frequency table. The frequency table may be constructed from \code{\link[stats]{xtabs}}, \code{\link{table}}, or be in the form of a matrix or a data.frame (as if read in from an external data file).
#'
#' @return None, but a graphic is created.
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @seealso See \code{\link[graphics]{hist}} and \code{\link{hist.formula}} for related functionality.
#'
#' @keywords hplot
#'
#' @aliases histFromSum histFromSum.default histFromSum.formula histFromSum.table
#'
#' @examples
#' ## Make some dummy data with a length category variable
#' set.seed(634434789)
#' df <- data.frame(tl=round(rnorm(100,100,20)))
#' df$lcat10 <- lencat(df$tl,w=10)
#'
#' ## Summarize as tables
#' ( tbl1 <- xtabs(~lcat10,data=df) )
#' ( tbl2 <- table(df$lcat10) )
#'
#' ## Turn the tables into a data.frame for testing (convert
#' ## the categories variables to numeric with fact2num())
#' df2 <- data.frame(tbl1)
#' df2$lcat10 <- fact2num(df2$lcat10)
#'
#' ## Turn the table into a matrix for testing
#' ( mat1 <- cbind(lcat10=as.numeric(rownames(tbl1)),freq=tbl1) )
#'
#' ## Histogram of the raw data ... set breaks and x-axis label
#' brks <- seq(20,160,10)
#' xlbl <- "Total Length (mm)"
#' hist(~tl,data=df,breaks=brks,xlab=xlbl)
#'
#' ## Use this function with various inputs ... changed colors
#' ## on each plot so that it was obvious that a new plot was made.
#' # table from xtabs()
#' histFromSum(tbl1,breaks=brks,xlab=xlbl,col="gray75")
#' # table from table()
#' histFromSum(tbl2,breaks=brks,xlab=xlbl,col="gray70")
#' # vectors from data.frame
#' histFromSum(df2$lcat10,df2$Freq,breaks=brks,xlab=xlbl,col="gray65")
#' # vectors from matrix
#' histFromSum(mat1[,"lcat10"],mat1[,"freq"],breaks=brks,xlab=xlbl,col="gray60")
#' # formula from a data.frame
#' histFromSum(Freq~lcat10,data=df2,breaks=brks,xlab=xlbl,col="gray55")
#'
#' @rdname histFromSum
#' @export
histFromSum <- function (x,...) {
  UseMethod("histFromSum")
}

#' @rdname histFromSum
#' @export
histFromSum.default <- function(x,y,...) {
  if (!is.numeric(x)) STOP("'x' (bin/category values) must be a numeric vector.")
  if (!is.vector(x)) STOP("'x' (bin/category values) must be a vector.")
  if (!is.numeric(y)) STOP("'y' (count/frequency values) must be a numeric vector.")
  if (!is.vector(y)) STOP("'y' (count/frequency values) must be a vector.")
  xs <- rep(x,y)
  graphics::hist(~xs,...) # nocov
}

#' @rdname histFromSum
#' @export
histFromSum.table <- function(x,...) {
  if (length(dim(x))>1) STOP("'x' must be a 1-dimensional table.")
  y <- x
  attributes(y) <- NULL
  names(y) <- "Freq"
  x <- suppressWarnings(as.numeric(names(x)))
  if (any(is.na(x))) STOP("Names in 'x' are not numeric.")
  names(x) <- names(dimnames(x))
  histFromSum.default(x,y,...) # nocov
}

#' @rdname histFromSum
#' @export
histFromSum.formula <- function(x,data=NULL,...) {
  tmp <- iHndlFormula(x,data,expNumR=1,expNumE=1)
  if (tmp$vnum!=2)
    STOP("'histFromSum' only works with 1 response and 1 explanatory variable.")
  if (!tmp$metExpNumR)
    STOP("'histFromSum' must have only 1 left-side variable in the formula.")
  if (!tmp$Rclass %in% c("numeric","integer"))
    STOP("The left-side variable in the formula must be numeric.")
  if (!tmp$metExpNumE)
    STOP("'histFromSum' must have only 1 right-side variable in the formula.")
  if (!tmp$Eclass %in% c("numeric","integer"))
    STOP("The right-side variable in the formula must be numeric.")
  histFromSum.default(tmp$mf[,tmp$Enames],tmp$mf[,tmp$Rname],...) # nocov
}
