#' @title Desity of shad captured in offshore zones and sunfish seined in inshore areas of four Ohio reservoirs across 22 years.
#'
#' @description Desity of shad captured in offshore zones and sunfish seined in inshore areas of four Ohio reservoirs across 22 years.
#'
#' @name Garvey4a
#' 
#' @docType data
#' 
#' @format A data frame of 22 observations on the following 2 variables:
#'  \describe{
#'    \item{shad}{Number of shad per m^2.}
#'    \item{sunfish}{Number of sunfish per m^2.}
#'  }
#' 
#' @source From Figure 1 in Garvey, J.E., E.A. Marschall, and R.A. Wright. 1998. \href{http://opensiuc.lib.siu.edu/fiaq_pubs/17/}{From star charts to stoneflies: detecting relationships in continuous bivariate data.} Ecology 79:442-447. 
#' 
#' @keywords datasets
#' 
#' @examples
#' data(Garvey4a)
#' str(Garvey4a)
#' head(Garvey4a)
#' plot(sunfish~shad,data=Garvey4a,pch=16)
NULL
