#' @title Hypothetical growth data for testing
#'
#' @description Hypothetical lengths at time of marking/tagging and recapture and time-at-large (i.e., between marking and recapture). These data are useful for testing growth related functions (e.g., \code{\link{findGrowthStarts}}) as they were generated from known growth functions (e.g., von Bertalanffy) with some random error and are, thus, \dQuote{as good as it gets} for testing.
#'
#' @name GrowthData3
#'
#' @docType data
#'
#' @format A data frame of 128 observations on the following 5 variables:
#'  \describe{
#'    \item{tag}{A unique fish ID (i.e., tag) number}
#'    \item{tlM}{Total length at time of marking/tagging simulated from a von Bertalanffy growth function with Linf=450, K=0.3, and t0=-0.5}
#'    \item{tlR}{Total length at time of recapture simulated from a von Bertalanffy growth function with Linf=450, gi=0.3, and ti=3 and assuming a random time-at-large from marking/tagging of roughly 1, 2, or 3 years.}
#'    \item{deltat}{Time-at-large (i.e., time between marking/tagging and recapture) simulate to be 1, 2, or 3 years (with decreasing probability) and some random error of a few days.}
#'    \item{deltaL}{Change in length between the time or marking/tagging and recapture (i.e., \code{tlR}-\code{tlM}).}
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
#' str(GrowthData3)
#' head(GrowthData3)
#' plot(tlR~tlM,data=GrowthData3)
#' abline(a=0,b=1,col="red")
#'
NULL
