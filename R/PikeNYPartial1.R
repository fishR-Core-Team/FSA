#' @title Capture histories (4 samples), in capture history format, of a subset of Northern Pike from Buckthorn Marsh, NY.
#'
#' @description Each line consists of the capture history over four samples of Northern Pike (\emph{Esox lucius}) in Buckthorn Marsh.  This file contains the capture histories for only those pike captured from April 1-4.
#'
#' @name PikeNYPartial1
#'
#' @docType data
#'
#' @format A data frame with 57 observations on the following 4 variables.
#'  \describe{
#'    \item{id}{A unique identification numbers}
#'    \item{first}{Indicator variable for the first sample (1=captured)} 
#'    \item{second}{Indicator variable for the second sample (1=captured)} 
#'    \item{third}{Indicator variable for the third sample (1=captured)} 
#'    \item{fourth}{Indicator variable for the fourth sample (1=captured)}
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Population Size 
#'    \item Abundance 
#'    \item Mark-Recapture
#'    \item Capture-Recapture
#'    \item Schnabel
#'    \item Schumacher-Eschmeyer
#'    \item Capture History
#'  }
#'
#' @concept Abundance 'Population Size' 'Mark-Recapture' 'Capture-Recapture' 'Schnabel' 'Capture History'
#'
#' @source New York Power Authority.  2004.  Use of buckhorn marsh and grand island tributaries by northern pike for spawning and as a nursery. Technical report, New York Power Authority, January 2004. Niagara Power Project (FERC No. 2216).
#'
#' @keywords datasets
#'
#' @examples
#' data(PikeNYPartial1)
#' str(PikeNYPartial1)
#' head(PikeNYPartial1)
#'
NULL
