#' @title Hypothetical growth data for testing seasonal age models.
#'
#' @description Hypothetical lengths at seasonal ages. These data are useful for testing growth related functions (e.g., \code{\link{findGrowthStarts}}) as they were generated from known growth functions (e.g., von Bertalanffy) with some random error and are, thus, \dQuote{as good as it gets} for testing.
#'
#' @name GrowthData2
#'
#' @docType data
#'
#' @format A data frame of 126 observations on the following 2 variables:
#'  \describe{
#'    \item{age}{Ages with decimals representing a fraction of a year}
#'    \item{tlS}{Total length simulated from a the \dQuote{Somers} parameterization of the von Bertalanffy growth function with Linf=450, K=0.3, t0=-0.5, C=0.9, and ts=0.1}
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
#' @seealso \code{\link{GrowthData1}}, \code{\link{GrowthData2}}, and \code{\link{findGrowthStarts}}
#'
#' @keywords datasets
#'
#' @examples
#' str(GrowthData2)
#' head(GrowthData2)
#' plot(tlS~age,data=GrowthData2)
#'
NULL
