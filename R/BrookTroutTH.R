#' @title Catch-at-age for Tobin Harbor, Isle Royale Brook Trout.
#'
#' @description Catch-at-age in fyke nets from 1996-1998 for \dQuote{Coaster} Brook Trout (\emph{Salvelinus fontinalis}) in Tobin Harbor, Isle Royale, Lake Superior.
#'
#' @name BrookTroutTH
#' 
#' @docType data
#' 
#' @format A data frame with 7 observations on the following 2 variables.
#'  \describe{
#'    \item{age}{A numeric vector of assigned ages}
#'    \item{catch}{A numeric vector of number of Brook Trout caught}
#'  }
#' 
#' @section Topic(s):
#'  \itemize{
#'    \item Mortality
#'    \item Catch Curve
#'    \item Chapman-Robson 
#'  }
#' 
#' @concept Mortality 'Catch Curve'
#' 
#' @source Quinlan, H.R. 1999. Biological Characteristics of Coaster Brook Trout at Isle Royale National Park, Michigan, 1996-98. U.S. Fish and Wildlife Service Ashland Fishery Resources Office report. November 1999.  [Was (is?) from http://www.fws.gov/midwest/ashland/brook/biochar/biolchar.html.]
#' 
#' @keywords datasets
#' 
#' @examples
#' data(BrookTroutTH)
#' str(BrookTroutTH)
#' head(BrookTroutTH)
#' plot(log(catch)~age,data=BrookTroutTH)
#'
NULL
