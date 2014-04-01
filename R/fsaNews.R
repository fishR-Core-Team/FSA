#'Read news and changes in the 'FSA' package.
#'
#'Read news and changes in the \sQuote{FSA} package.
#'
#'This makes use of \code{file.show()} to display the \sQuote{NEWS} file for this package.
#'
#'@aliases fsaNews FSANews
#'@param \dots Additional arguments passed to \code{file.show()}.
#'@return None.
#'@note Borrowed from Yihui Xie of the \sQuote{animation} package.
#'@keywords manip
#'@export fsaNews FSANews
#'@examples
#'fsaNews()
#'FSANews()
#'
fsaNews <- FSANews <- function (...) {
  newsfile <- file.path(system.file(package="FSA"),"NEWS")
  file.show(newsfile, ...)
}
