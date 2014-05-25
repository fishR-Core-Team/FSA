#' @title Read news and changes for the 'FSA' package.
#'
#' @description Opens up the \href{https://github.com/droglenc/FSA/blob/master/NEWS.md}{News.md GitHub file} for the \sQuote{FSA} package in an external browser.
#' 
#' @aliases fsaNews FSANews
#' 
#' @return None.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords manip
#' 
#' @examples
#'## Not run because it will open an external browser.
#'\dontrun{
#'fsaNews()
#'FSANews()
#'}
#' 
#' @rdname fsaNews
#' @export
fsaNews <- function () {
  browseURL("https://github.com/droglenc/FSA/blob/master/NEWS.md")
}

#' @rdname fsaNews
#' @export
FSANews <- function () {
  fsaNews()
}
