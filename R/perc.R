#' @title Computes the percentage of values in a vector less than or greater than (and equal to) some value.
#'
#' @description Computes the percentage of values in a vector less than or greater than (and equal to) a user-supplied value.
#' 
#' @details This function is most useful when used with an apply-type of function.
#'
#' @param x A numeric vector.
#' @param val A single numeric value.
#' @param dir A string that indicates whether the percentage is for values in \code{x} that are \dQuote{greater than and equal} \code{"geq"}, \dQuote{greater than} \code{"gt"}, \dQuote{less than and equal} \code{"leq"}, \dQuote{less than} \code{"lt"} the value in \code{val}. 
#' @param na.rm A logical that indicates whether \code{NA} values should be removed (DEFAULT) from \code{x} or not.
#' @param digits A single numeric that indicates the number of decimals the percentage should be rounded to.
#'
#' @return A single numeric that is the percentage of values in \code{x} that meet the criterion in \code{dir} relative to \code{val}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords misc
#'
#' @examples
#' ## vector of values
#' ( tmp <- c(1:8,NA,NA) )
#' 
#' ## percentages excluding NA values
#' perc(tmp,5)
#' perc(tmp,5,"gt")
#' perc(tmp,5,"leq")
#' perc(tmp,5,"lt")
#' 
#' ## percentages including NA values
#' perc(tmp,5,na.rm=FALSE)
#' perc(tmp,5,"gt",na.rm=FALSE)
#' perc(tmp,5,"leq",na.rm=FALSE)
#' perc(tmp,5,"lt",na.rm=FALSE)
#' 
#' @export
perc <- function(x,val,dir=c("geq","gt","leq","lt"),na.rm=TRUE,digits=getOption("digits")) {
  ## Some checks
  dir <- match.arg(dir)
  if (!class(x) %in% c("numeric","integer")) stop("'perc' only works for numeric vectors.",call.=FALSE)
  if (length(val)>1) warning("Only the first value of 'val' was used.",call.=FALSE)
  ## Find sample size (don't or do include NA values)
  n <- ifelse(na.rm,length(x[!is.na(x)]),length(x))
  ## Compute percentage in dir(ection) of val(ue), but return
  ##   a NaN if the x has no valid values
  if (n==0) return(NaN)
  else { # find values that fit criterion
    switch(dir,
           geq= {tmp <- x[x>=val]},
           gt = {tmp <- x[x>val]},
           leq= {tmp <- x[x<=val]},
           lt = {tmp <- x[x<val]}
           ) # end switch
    ## must remove NA values (even if asked not to because they
    ## will appear to be less than val ... i.e., NAs were included
    ## in n above if asked for but they should not be included in
    ## the vector of values that fit the criterion) to find
    ## number that match the criterion
    tmp <- length(tmp[!is.na(tmp)])
  }
  round(tmp/n*100,digits)
}
