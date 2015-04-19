#' @title Age and length of spot.
#'
#' @description Ages (from otoliths) and length of Virginia Spot (\emph{Leiostomus xanthurus}).
#'
#' @details Final length measurements were simulated by adding a uniform error to the value at the beginning of the length category.
#'
#' @name SpotVA1
#'
#' @docType data
#'
#' @format A data frame of 403 observations on the following 2 variables:
#'  \describe{
#'    \item{tl}{Measured total lengths (in inches).} 
#'    \item{age}{Ages assigned from examination of otoliths.} 
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Growth
#'    \item von Bertalanffy
#'  }
#'
#' @concept Growth 'von Bertalanffy'
#'
#' @seealso See \code{\link[FSAdata]{SpotVA2}} in \pkg{FSAdata} for related data.
#'
#' @source Extracted from Table 1 in Chapter 8 (Spot) of the VMRC Final Report on Finfish Ageing, 2002 by the \href{http://ww2.odu.edu/sci/cqfe/}{Center for Quantitative Fisheries Ecology} at Old Dominion University.
#'
#' @keywords datasets
#'
#' @examples
#' data(SpotVA1)
#' str(SpotVA1)
#' head(SpotVA1)
#' plot(tl~age,data=SpotVA1)
#'
NULL
