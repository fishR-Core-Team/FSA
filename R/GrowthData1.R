#' @title Hypothetical growth data for testing
#'
#' @description Hypothetical lengths at annual ages. These data are useful for testing growth related functions (e.g., \code{\link{findGrowthStarts}}) as they were generated from known growth functions (e.g., von Bertalanffy) with some random error and are, thus, \dQuote{as good as it gets} for testing.
#'
#' @name GrowthData1
#'
#' @docType data
#'
#' @format A data frame of 179 observations on the following 5 variables:
#'  \describe{
#'    \item{age}{Ages as a whole number}
#'    \item{tlV}{Total length simulated from a von Bertalanffy growth function with Linf=450, K=0.3, and t0=-0.5}
#'    \item{tlG}{Total length simulated from a Gompertz growth function with Linf=450, gi=0.3, and ti=3}
#'    \item{tlL}{Total length simulated from a logistic growth function with Linf=450, gninf=0.5, and ti=3}
#'    \item{tlR}{Total length simulated from a Richards growth function with Linf=450, k=0.5, ti=2, and b=-0.5}
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Growth
#'    \item von Bertalanffy
#'  }
#'
#' @concept Growth
#' @concept von Bertalanffy
#'
#' @seealso \code{\link{GrowthData2}}, \code{\link{GrowthData3}}, and \code{\link{findGrowthStarts}}
#'
#' @keywords datasets
#'
#' @examples
#' str(GrowthData1)
#' head(GrowthData1)
#' plot(tlV~age,data=GrowthData1)
#' plot(tlG~age,data=GrowthData1)
#' plot(tlL~age,data=GrowthData1)
#' plot(tlR~age,data=GrowthData1)
#'
NULL
