#' @title Assigned ages from two readers on three structures for Lake Whitefish from Lake Champlain.
#'
#' @description Assigned ages from two readers on three structures for Lake Whitefish (\emph{Coregonus clupeaformis}) from Lake Champlain in 2009.
#'
#' @name WhitefishLC
#'
#' @docType data
#'
#' @format A data frame with 151 observations on the following 11 variables:
#'  \describe{
#'    \item{fishID}{A unique fish identification number}
#'    \item{tl}{Total length (in mm)}
#'    \item{scale1}{Assessed age from scales by first reader}
#'    \item{scale2}{Assessed age from scales by second reader}
#'    \item{scaleC}{Consensus age from scales by both reader}
#'    \item{finray1}{Assessed age from fin rays by first reader}
#'    \item{finray2}{Assessed age from fin rays by second reader}
#'    \item{finrayC}{Consensus age from fin rays by both reader}
#'    \item{otolith1}{Assessed age from otoliths by first reader}
#'    \item{otolith2}{Assessed age from otoliths by second reader}
#'    \item{otolithC}{Consensus age from otoliths by both reader}
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Age 
#'    \item Ageing Error
#'    \item Precision 
#'    \item Bias 
#'    \item Age Comparisons
#'  }
#'
#' @concept Age Precision
#' @concept Age Bias
#' @concept Ageing Error
#' @concept Age Comparisons
#'
#' @source Data from Herbst, S.J. and J.E. Marsden.  2011.  Comparison of precision and bias of scale, fin ray, and otolith age estimates for lake whitefish (\emph{Coregonus clupeaformis}) in Lake Champlain.  Journal of Great Lakes Research.  37:386-389.  Contributed by Seth Herbst.  \bold{Do not use for other than educational purposes without permission from the author.}  [Was (is?) from http://www.uvm.edu/rsenr/emarsden/documents/Herbst\%20and\%20Marsden\%20whitefish\%20age\%20structure\%20comparison.pdf.]
#'
#' @seealso Used in \code{\link{ageBias}} and \code{\link{agePrecision}} examples.
#' 
#' @keywords datasets
#'
#' @examples
#' str(WhitefishLC)
#' head(WhitefishLC)
#'
NULL