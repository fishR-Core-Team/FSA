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
#' fishR("IFAR")      # Introduction to Fisheries Analysis with R page
#' fishR("general")   # general vignettes page
#' fishR("books")     # books vignettes page
#' fishR("AIFFD")     # Analysis & Interpretation of Freshwater Fisheries Data page
#' fishR("posts")     # blog posts (some examples) page
#' 
#' } ## END IF INTERACTIVE MODE
#' 
#' @export
fishR <- function(where=c("home","IFAR","general","books","AIFFD","posts","news")) {
  where <- match.arg(where)
  tmp <- "http://fishr.wordpress.com"
  switch(where,
    home=   { tmp <- tmp },
    IFAR=   { tmp <- paste0(tmp,"/ifar/") },
    general={ tmp <- paste0(tmp,"/vignettes/") },
    books=  { tmp <- paste0(tmp,"/books/") },
    AIFFD=  { tmp <- paste0(tmp,"/books/aiffd/") },
    posts=  { tmp <- paste0(tmp,"/news/") }
  )
  browseURL(tmp)
}
