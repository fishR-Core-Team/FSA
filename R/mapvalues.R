#' Replace specified values with new values, in a vector or factor.  EXACT SAME FUNCTION AS IN THE plyr PACKAGE.
#'
#' Item in \code{x} that match items \code{from} will be replaced by items in \code{to}, matched by position. For example, items in \code{x} that match the first element in \code{from} will be replaced by the first element of \code{to}.
#'
#' If \code{x} is a factor, the matching levels of the factor will be replaced with the new values.
#'
#' This is the EXACT SAME FUNCTION AS IN THE \pkg{plyr} PACKAGE.  I included it here to minimize conflicts between same-named functions in \pkg{dplyr} and \pkg{plyr}.
#'
#' @param x the factor or vector to modify
#' @param from a vector of the items to replace
#' @param to a vector of replacement values
#' @param warn_missing print a message if any of the old values are not actually present in \code{x}
#'
#' @seealso See \code{\link[plyr]{mapvalues}} in \pkg{plyr} for the EXACT SAME FUNCTION.  See \code{\link[plyr]{revalue}} in \pkg{plyr} to do the same thing but with a single named vector instead of two separate vectors.
#'
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{2-Basic Data Manipulations}.
#' 
#' @export
#' 
#' @examples
#' ## Examples from plyr
#' x <- c("a", "b", "c")
#' mapvalues(x, c("a", "c"), c("A", "C"))
#'
#' # Works on factors
#' y <- factor(c("a", "b", "c", "a"))
#' mapvalues(y, c("a", "c"), c("A", "C"))
#'
#' # Works on numeric vectors
#' z <- c(1, 4, 5, 9)
#' mapvalues(z, from = c(1, 5, 9), to = c(10, 50, 90))
#' 
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
mapvalues <- function(x, from, to, warn_missing = TRUE) {
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  
  if (is.factor(x)) {
    # If x is a factor, call self but operate on the levels
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  
  mapidx <- match(x, from)
  mapidxNA  <- is.na(mapidx)
  
  # index of items in `from` that were found in `x`
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found) ], collapse = ", "))
  }
  
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}