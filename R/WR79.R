#' @title Age and length for a hypothetical sample from Westerheim and Ricker (1979).
#'
#' @description Age and length for a hypothetical sample in Westerheim and Ricker (1979).
#'
#' @details Age-length data in 5-cm increments taken exactly from Table 2A of the source which was a sample from a hypothetical poplation in which year-class strength varied in the ratio 2:1 and the rate of increase in length decreased with age.  Actual lengths in each 5-cm interval were simulated with a uniform distribution.  The aged fish in this file were randomly selected and an assessed age was assigned according to the information in Table 2A.
#'
#' @name WR79
#'
#' @docType data
#'
#' @format A data frame of 2369 observations on the following 3 variables:
#'  \describe{
#'    \item{ID}{Unique fish identifiers.}
#'    \item{len}{Length of an individual fish.} 
#'    \item{age}{Age of an individual fish.} 
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Age-length key
#'  }
#'
#' @concept 'Age-Length Key'
#'
#' @source Simulated from Table 2A in Westerheim, S.J. and W.E. Ricker. 1979. Bias in using age-length key to estimate age-frequency distributions. Journal of the Fisheries Research Board of Canada. 35:184-189.
#'
#' @keywords datasets
#'
#' @examples
#' data(WR79)
#' str(WR79)
#' head(WR79)
#'
#' ## Extract the aged sample
#' WR79.aged <- subset(WR79,!is.na(age))
#' str(WR79.aged)
#'
#' ## Extract the length sample
#' WR79.length <- subset(WR79,is.na(age))
#' str(WR79.length)
#'
NULL
