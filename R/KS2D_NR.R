#' @title Data from Figure 14.7.1 of Numerical Recipes
#'
#' @description Data \sQuote{digitized} from Figure 14.7.1 of Numerical Recipes
#'
#' @note The data in Figure 14.7.1 apparently had 65 triangles and 35 squares.  These data have only 63 triangles and 32 squares.
#'
#' @name KS2D_NR
#'
#' @docType data
#'
#' @format A data frame with 95 observations on the following 4 variables:
#'  \describe{ 
#'    \item{obs}{Observation number.} 
#'    \item{x}{X coordinate.} 
#'    \item{y}{Y coordinate.} 
#'    \item{group}{Which coordinate set (\sQuote{triangles} or \sQuote{squares}).} 
#'  }
#'
#' @source
#' Press, W.H., S.A. Teukolsky, W.T. Vetterling, B.P. Flannery.  2007.  \href{http://www.nr.com/}{Numerical Recipes: The Art of Scientific Computing, 3rd Edition.}  Cambridge University Press.  1286 pages.
#'
#' @keywords datasets
#'
#' @examples
#' data(KS2D_NR)
#'
#' # separate into the two sets of coordinates
#' d1 <- subset(KS2D_NR,group=="triangles")
#' d2 <- subset(KS2D_NR,group=="squares")
#'
NULL
