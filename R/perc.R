#' @title Computes the percentage of values in a vector less than or greater than (and equal to) some value.
#'
#' @description Computes the percentage of values in a vector less than or greater than (and equal to) a user-supplied value.
#' 
#' @details This function is most useful when used with an apply-type of function.
#'
#' @param x A numeric vector.
#' @param val A single numeric value.
#' @param dir A string that indicates whether the percentage is for values in \code{x} that are \code{"greater"} or \code{"less"} than \code{val}. 
#' @param na.rm A logical that indicates whether \code{NA} values should be removed (DEFAULT) from \code{x} or not.
#' @param digits A single numeric that indicates the number of decimals the percentage should be rounded to.
#'
#' @return A single numeric that is the percentage of values in \code{x} that are \code{"greater"} or \code{"less"} than (and equal to) \code{val}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords misc
#'
#' @examples
#' ( tmp <- c(1:10,NA,NA) )
#' perc(tmp,5)
#' perc(tmp,5,"less")
#' perc(tmp,5,na.rm=FALSE)
#' perc(tmp,c(5,7),"less")
#' 
#' @export
perc <- function(x,val,dir=c("greater","less"),na.rm=TRUE,digits=getOption("digits")) {
  ## Some checks
  dir <- match.arg(dir)
  if (!class(x) %in% c("numeric","integer")) stop("'perc' only works for numeric vectors.",call.=FALSE)
  if (length(val)>1) warning("Only the first value of 'val' was used.",call.=FALSE)
  ## Remove missing values if asked to do so
  if (na.rm) x <- x[!is.na(x)]
  else if (any(is.na(x))) warning("'x' contained 'NA's and 'na.rm=FALSE'; beware of returned values.",call.=FALSE)
  ## Find sample size
  n <- length(x)
  ## Compute percentage in dir(ection) of val(ue), but return
  ##   a NaN if the x has no valid values
  if (n==0) NaN
  else if (dir=="greater") round(length(x[x>=val])/n*100,digits)
  else round(length(x[x<=val])/n*100,digits)
}
