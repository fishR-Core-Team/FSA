#'Read news and changes in the 'FSA' package.
#'
#'Read news and changes in the \sQuote{FSA} package.
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
#' @export fsaNews FSANews
fsaNews <- FSANews <- function () {
  browseURL("https://github.com/droglenc/FSA/blob/master/NEWS.md")
}
