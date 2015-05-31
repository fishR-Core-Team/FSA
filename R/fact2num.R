#' @title Converts "numeric" factor levels to numeric values.
#'
#' @description Converts \dQuote{numeric} factor levels to numeric values.
#'
#' @param object A vector with \dQuote{numeric} factor levels to be converted to numeric values.
#'
#' @return A numeric vector.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords manip
#' 
#' @examples
#' junk <- factor(c(1,7,2,4,3,10))
#' str(junk)
#' junk2 <- fact2num(junk)
#' str(junk2)
#'
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#' 
#' bad <- factor(c("A","B","C"))
#' # This will result in an error -- levels are not 'numeric'
#' bad2 <- fact2num(bad)
#' 
#' }  ## END IF INTERACTIVE MODE
#' 
#' @export
fact2num <- function(object) {
  ## Don't continue if object is not a factor or character 
  ## i.e., does not fit the purpose of this function
  if (!class(object) %in% c("factor","character")) stop("'object' is not a factor or character and does not fit the purpose of this function.",call.=FALSE)
  ## Convert factor to character and then numeric
  suppressWarnings(res <- as.numeric(as.character(object)))
  ## If all na's then stop because values were not numeric-like, else return
  if (all(is.na(res))) stop("Conversion aborted because all levels in 'object' are not 'numbers'.",call.=FALSE)
  else as.vector(res)
}
