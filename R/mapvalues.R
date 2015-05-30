#' Replace specified values with new values, in a vector or factor.
#'
#' This is the exact same \code{\link[plyr]{mapvalues}} function from \pkg{plyr} but exported from \pkg{FSA} so as to minimize conflicts between same-named functions in \pkg{dplyr}.  In other words, this allows the user to load just \pkg{FSA} and \pkg{dplyr} and still have the functionality of \code{\link[plyr]{mapvalues}} from \pkg{plyr} without the hassles associated with having \pkg{dplyr} and \pkg{plyr} loaded at the same time.
#' 
#' The examples illustrate how the function could be used in a typical fisheries analysis.
#' 
#' @seealso See \code{\link[plyr]{revalue}} in \pkg{plyr} to do the same thing but with a single named vector instead of two separate vectors.
#'
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{2-Basic Data Manipulations}.
#' 
#' @examples
#' ## Examples of typical fisheries uses
#' # only for repeatability
#' set.seed(345234534)
#' dbg <- data.frame(species=factor(rep(c("BLUEGILL"),30)),tl=round(rnorm(30,130,50),0))
#' dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
#' dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
#' dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
#' dbt <- data.frame(species=factor(rep(c("Bluefin Tuna"),30)),tl=round(rnorm(30,1900,300),0))
#' dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
#' df <- rbind(dbg,dlb,dbt)
#' str(df)
#' df$species1 <- mapvalues(df$species,from=c("BLUEGILL","LMB"),to=c("Bluegill","Largemouth Bass")) 
#' xtabs(~species+species1,data=df)
#' 
#' @importFrom plyr mapvalues
#' @name mapvalues
#' @rdname mapvalues
#' @export
NULL