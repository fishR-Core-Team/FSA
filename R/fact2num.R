#'Converts "numeric" factor levels to numeric values.
#'
#'Converts \dQuote{numeric} factor levels to numeric values.
#'
#'@param object A vector with \dQuote{numeric} factor levels to be converted to
#'numeric values.
#'@return A numeric vector.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@keywords manip
#'@export
#'@examples
#'junk <- factor(c(1,7,2,4,3,10))
#'str(junk)
#'junk2 <- fact2num(junk)
#'str(junk2)
#'
#'## Below not run because it will result in an error -- levels are not 'numeric'
#'\dontrun{
#'bad <- factor(c("A","B","C"))
#'bad2 <- fact2num(bad)
#'}
#'
fact2num <- function(object) {
  res <- as.numeric(as.character(object))
  if (all(is.na(res))) stop("Conversion aborted because all levels in 'object' are not 'numbers.'\n\n",call.=FALSE)
  else as.vector(res)
}
