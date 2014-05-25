#' @title Opens web pages associated with the fishR website.
#'
#' @description Opens web pages associated with the \href{http://fishr.wordpress.com"}{fishR website} in a browser.  The user can open the main page or choose a specific page to open.
#'
#' @param where A string that indicates a particular page on the fishR website to open.
#' 
#' @return None, but a webpage will be opened in the default browswer.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords misc
#' 
#' @examples
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#' 
#' fishR()            # home page
#' fishR("general")   # general vignettes page
#' fishR("books")     # books vignettes page
#' fishR("AIFFD")     # Analysis & Interpretation of Freshwater Fisheries Data page
#' fishR("posts")     # blog posts (some examples) page
#' 
#' } ## END IF INTERACTIVE MODE
#' 
#' @export
fishR <- function(where=c("home","general","books","AIFFD","posts","news")) {
  where <- match.arg(where)
  switch(where,
    home={browseURL("http://fishr.wordpress.com")},
    general={browseURL("http://fishr.wordpress.com/vignettes/")},
    books={browseURL("http://fishr.wordpress.com/books/")},
    AIFFD={browseURL("http://fishr.wordpress.com/books/aiffd/")},
    posts={browseURL("http://fishr.wordpress.com/news/")}
  )
}
