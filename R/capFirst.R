#' Capitalizes the first letter of first or all words in a string.
#' 
#' Capitalizes the first letter of first or all words in a string.
#' 
#'@param x A single string.
#'@param words A single string that indicates whether all (the default) or only
#'the first words should be capitalized.
#'
#'@return A single string with the first letter of the first or all words capitalized.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@export
#'@keywords manip
#'@examples
#'## Capitalize first letter of all words (the default)
#'capFirst("Derek Ogle")
#'capFirst("derek ogle")
#'capFirst("derek")
#'# apply to all elements in a vector
#'vec <- c("Derek Ogle","derek ogle","Derek ogle","derek Ogle","DEREK OGLE")
#'apply(matrix(vec),1,capFirst)
#'
#'## Capitalize first letter of only the first words
#'capFirst("Derek Ogle",words="first")
#'capFirst("derek ogle",words="first")
#'capFirst("derek",words="first")
#'# apply to all elements in a vector
#'apply(matrix(vec),1,capFirst,words="first")
#'
capFirst<- function(x,words=c("all","first")) {
  # See whether all or just the first word should have the first letter capitalized
  words <- match.arg(words)
  # convet entire string to lower case ...
  x <- tolower(x)
  # then split on space if more than one word
  s <- strsplit(x, " ")[[1]]
  if (words=="first") {
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
