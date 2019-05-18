#' @title Summarized multiple mark-recapture data for all Northern Pike from Buckhorn Marsh, NY.
#'
#' @description Summary results of capture histories (number captured, number of recaptured fish, and number of unmarked fish that were marked) for all Buckhorn Marsh Northern Pike (\emph{Esox lucius}).
#'
#' @name PikeNY
#'
#' @docType data
#'
#' @format A data frame with 21 observations on the following 4 variables:
#'  \describe{
#'    \item{date}{Capture date}
#'    \item{n}{Total fish captured in each sample}
#'    \item{m}{Marked fish captured in each sample}
#'    \item{R}{Marked fish returned to the population}
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
#'  }
#'
#' @concept Abundance
#' @concept Population Size
#' @concept Mark-Recapture
#' @concept Capture-Recapture
#' @concept Schnabel
#'
#' @source New York Power Authority. 2004. Use of Buckhorn Marsh and Grand Island tributaries by Northern Pike for spawning and as a nursery. Technical report, New York Power Authority, January 2004. Niagara Power Project (FERC No. 2216).
#'
#' @seealso Used in \code{\link{mrClosed}} examples. Also see \code{\link{PikeNYPartial1}}.
#' 
#' @keywords datasets
#'
#' @examples
#' str(PikeNY)
#' head(PikeNY)
#'
NULL
