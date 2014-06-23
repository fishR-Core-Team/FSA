#' @title Length and weight of Bluegill from Lake Mary, MN.
#'
#' @description Length (standard, fork, and total) and weight of Bluegill (\emph{Lepomis macrochirus}) collected from Lake Mary, Minnesota.
#'
#' @name BluegillLM
#'
#' @docType data
#' 
#' @format A data frame with 100 observations on the following 5 variables:
#'  \describe{
#'    \item{sernum}{Unique serial number}
#'    \item{sl}{Standard length (mm)}
#'    \item{fl}{Fork length (mm)}
#'    \item{tl}{Total length (mm)}
#'    \item{wght}{Weight (g)}
#'  }
#' 
#' @section Topic(s):
#'  \itemize{
#'    \item Length-weight
#'    \item Length conversion
#'  }
#' 
#' @concept 'Length-Weight'
#' 
#' @keywords datasets
#' 
#' @seealso \code{lakemary} in \pkg{alr3} for a different sample of Bluegill from Lake Mary that has length and age.
#' 
#' @examples
#' data(BluegillLM)
#' str(BluegillLM)
#' head(BluegillLM)
#' op <- par(mfrow=c(3,2),pch=19)
#' plot(wght~sl,data=BluegillLM)
#' plot(wght~fl,data=BluegillLM)
#' plot(wght~tl,data=BluegillLM)
#' plot(tl~fl,data=BluegillLM)
#' plot(tl~sl,data=BluegillLM)
#' plot(fl~sl,data=BluegillLM)
#' par(op)
NULL
