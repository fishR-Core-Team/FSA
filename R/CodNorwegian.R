#' @title Stock and recruitment data for Norwegian cod,1937-1960.
#'
#' @description Norwegian cod (\emph{Gadus morhua}) stock and recruitment by year, 1937-1960.
#'
#' @name CodNorwegian
#' 
#' @docType data
#' 
#' @format A data frame of 24 observations on the following 3 variables:
#'  \describe{
#'    \item{year}{Year of data.}
#'    \item{recruits}{Recruits -- year-class strength index.}
#'    \item{stock}{Spawning stock index.} 
#'  }
#' 
#' @section Topic(s):
#'  \itemize{
#'    \item Stock-recruit
#'  }
#' 
#' @concept 'Stock-Recruit'
#' 
#' @source From Garrod, D.J. 1967.  Population dynamics of the Arcto-Norwegian cod.  Journal of the Fisheries Research Board of Canada, 24:145-190.
#' 
#' @keywords datasets
#' 
#' @examples
#' data(CodNorwegian)
#' str(CodNorwegian)
#' head(CodNorwegian)
#' op <- par(mfrow=c(1,2))
#' plot(recruits~year,data=CodNorwegian,type="l")
#' plot(recruits~stock,data=CodNorwegian)
#' par(op)
#'
NULL
