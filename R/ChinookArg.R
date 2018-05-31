#' @title Lengths and weights for Chinook Salmon from three locations in Argentina.
#'
#' @description Lengths and weights for Chinook Salmon from three locations in Argentina.
#'
#' @name ChinookArg
#' 
#' @docType data
#' 
#' @format A data frame with 112 observations on the following 3 variables:
#'  \describe{
#'    \item{tl}{Total length (cm)}
#'    \item{w}{Weight (kg)}
#'    \item{loc}{Capture location (\code{Argentina}, \code{Petrohue}, \code{Puyehue})} 
#'  }
#' 
#' @section Topic(s):
#'  \itemize{
#'    \item Weight-Length 
#'  }
#' 
#' @concept 'Weight-Length'
#' 
#' @source From Figure 4 in Soto, D., I. Arismendi, C. Di Prinzio, and F. Jara.  2007.  Establishment of Chinook salmon (\emph{Oncorhynchus tshawytscha}) in Pacific basins of southern South America and its potential ecosystem implications.  Revista Chilena d Historia Natural, 80:81-98.  [Was (is?) from http://www.scielo.cl/pdf/rchnat/v80n1/art07.pdf.]
#' 
#' @keywords datasets
#' 
#' @examples
#' str(ChinookArg)
#' head(ChinookArg)
#' op <- par(mfrow=c(2,2),pch=19)
#' plot(w~tl,data=ChinookArg,subset=loc=="Argentina")
#' plot(w~tl,data=ChinookArg,subset=loc=="Petrohue")
#' plot(w~tl,data=ChinookArg,subset=loc=="Puyehue")
#' par(op)
#'
NULL