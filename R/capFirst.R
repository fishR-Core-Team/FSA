#' @title Capitalizes the first letter of first or all words in a string.
#' 
#' @description Capitalizes the first letter of first or all words in a string.
#' 
#' @param x A single string.
#' @param which A single string that indicates whether all (the default) or only the first words should be capitalized.
#'
#' @return A single string with the first letter of the first or all words capitalized.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords manip
#'
#' @examples
#' ## Capitalize first letter of all words (the default)
#' capFirst("Derek Ogle")
#' capFirst("derek ogle")
#' capFirst("derek")
#'
#' ## Capitalize first letter of only the first words
#' capFirst("Derek Ogle",which="first")
#' capFirst("derek ogle",which="first")
#' capFirst("derek",which="first")

#' ## apply to all elements in a vector
#' vec <- c("Derek Ogle","derek ogle","Derek ogle","derek Ogle","DEREK OGLE")
#' capFirst(vec)
#' capFirst(vec,which="first")
#'
#' ## check class types
#' class(vec)
#' vec1 <- capFirst(vec)
#' class(vec1)
#' fvec <- factor(vec)
#' fvec1 <- capFirst(fvec)
#' class(fvec1)
#' @export
capFirst <- function(x,which=c("all","first")) {
  ## Get the class of the object
  cls <- class(x)
  ## Perform a check
  if (!(cls %in% c("character","factor"))) stop("'capFirst' only works with 'character' or 'class' objects.",call.=FALSE)
  ## Capitalize the one word or the words in the vector
  if (length(x)==1) x <- iCapFirst(x,which)
  else x <- apply(matrix(x),MARGIN=1,FUN=iCapFirst,which=which)
  ## Change the case to what the original was
  if (cls=="factor") x <- as.factor(x)
  ## Return the object
  x
}

iCapFirst<- function(x,which=c("all","first")) {
  # See whether all or just the first word should have the first letter capitalized
  which <- match.arg(which)
  # convert entire string to lower case ...
  x <- tolower(x)
  # then split on space if more than one word
  s <- strsplit(x, " ")[[1]]
  if (which=="first") {
    # convert first letters of first word to upper-case    
    s1 <- toupper(substring(s, 1,1)[1])
    # attach capitalized first letter to rest of lower-cased original string
    paste(s1,substring(x,2),sep="",collapse=" ")
  } else {
    # convert first letters of all words to upper-case
    s1 <- toupper(substring(s, 1,1))
    # attach capitalized first letter to rest of lower-cased separated strings
    paste(s1,substring(s,2),sep="",collapse=" ")
  }
}

