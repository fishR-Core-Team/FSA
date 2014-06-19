#' @title Number of Ameletus and Leuctra per benthic core in pools in Ohio headwater streams.
#'
#' @description Number of \emph{Ameletus} and \emph{Leuctra} per benthic core in pools in Ohio headwater streams.
#'
#' @name Garvey1
#' 
#' @docType data
#' 
#' @format A data frame of 72 observations on the following 2 variables:
#'  \describe{
#'    \item{Ameletus}{Number of \emph{Ameletus} in the benthic core.}
#'    \item{Leuctra}{Number of \emph{Leuctra} in the benthic core.}
#'  }
#' 
#' @source From Figure 1 in Garvey, J.E., E.A. Marschall, and R.A. Wright. 1998. \href{http://opensiuc.lib.siu.edu/fiaq_pubs/17/}{From star charts to stoneflies: detecting relationships in continuous bivariate data.} Ecology 79:442-447. 
#' 
#' @keywords datasets
#' 
#' @examples
#' data(Garvey1)
#' str(Garvey1)
#' head(Garvey1)
#' plot(Leuctra~Ameletus,data=Garvey1,pch=16,col=rgb(0,0,0,0.1))
NULL
