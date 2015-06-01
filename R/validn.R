#' @title Finds the number of valid (non-NA) values in a vector.
#'
#' @description Finds the number of valid (non-NA) values in a vector.
#'
#' @param object A vector.
#'
#' @return A single numeric value that is the number of non-\code{NA} values in a vector.
#' 
#' @seealso See \code{\link[plotrix]{valid.n}} in \pkg{plotrix} and \code{\link[gdata]{nobs}} in \pkg{gdata} for similar functionality.  See \code{\link{is.na}} for finding the missing values.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{2-Basic Data Manipulations}.
#' 
#' @keywords manip
#' 
#' @examples
#' junk1 <- c(1,7,2,4,3,10,NA)
#' junk2 <- c("Derek","Hugh","Ogle","Santa","Claus","Nick",NA,NA)
#' junk3 <- factor(junk2)
#' junk4 <- c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,NA,NA)
#' 
#' validn(junk1)
#' validn(junk2)
#' validn(junk3)
#' validn(junk4)
#'  
#' @export
validn <- function(object) {
  ## Process easily if it is not a factor
  if (!is.factor(object)) {
    ## Check to make sure that it is a vector
    if (!is.vector(object)) stop("'object' must be a vector.",call.=FALSE)
    else return(sum(!is.na(object)))
  } else { # is a factor
    ## Make sure that it is a factor vector
    if (!is.null(dim(object))) stop("'object' must be a vector.",call.=FALSE)
    else return(sum(!is.na(object)))
  }
}
