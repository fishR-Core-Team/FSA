#' @title Filters a data frame and drops the unused levels.
#'
#' @description Filters a data frame and drops the unused levels.
#'
#' @details This function is equivalent to the combined usage of \code{filter} from \pkg{dplyr} and \code{droplevels}.
#'
#' @note Newbie students using R expect that when a data.frame is filtered via a factor variable that any original levels that are no longer used after the filterting will be ignored.  This, however, is not the case and often results in tables with empty cells and figures with empty bars.  One remedy is to use \code{droplevels} immediately following \code{filter}.  This generally becomes a repetitive sequence; thus, \code{filterD} incorporates these two functions into one function.
#'
#' @aliases filterD
#'
#' @param .data A data frame.
#' @param \dots further arguments to be passed to \code{filter} from \pkg{dplyr}.
#'
#' @return A data frame with same variables as \code{.data}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See also \code{subset} and \code{filter} in \pkg{dplyr}.
#'
#' @keywords misc
#'
#' @examples
#' ## The problem -- note use of unused level in the final table.
#' require(dplyr)
#' levels(iris$Species)
#' tmp1 <- dplyr::filter(iris,Species=="setosa" | Species=="versicolor")
#' levels(tmp1$Species)
#' xtabs(~Species,data=tmp1)
#'
#' ## A simpler fix using filterD
#' tmp2 <- filterD(iris,Species=="setosa" | Species=="versicolor")
#' levels(tmp2$Species)
#' xtabs(~Species,data=tmp2)
#' 
#' @export
filterD <- function(.data,...) {
  tmp <- dplyr::filter(.data,...)
  droplevels(tmp)
}
