#' @title Gabelhouse five-cell length categories for various species.
#'
#' @description Cutoffs for the Gabelhouse five-cell length categories for a variety of species.
#'
#' @name PSDlit
#'
#' @docType data
#'
#' @format A data frame of 58 observations on the following 11 variables:
#'  \describe{
#'    \item{species}{Species name.}
#'    \item{stock.in}{Stock length in inches.}
#'    \item{quality.in}{Quality length in inches.}
#'    \item{preferred.in}{Preferred length in inches.}
#'    \item{memorable.in}{Memorable length in inches.}
#'    \item{trophy.in}{Trophy length in inches.}
#'    \item{stock.cm}{Stock length in cm.}
#'    \item{quality.cm}{Quality length in cm.}
#'    \item{preferred.cm}{Preferred length in cm.}
#'    \item{memorable.cm}{Memorable length in cm.}
#'    \item{trophy.cm}{Trophy length in cm.}
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Size structure
#'    \item Proportional size structure
#'    \item Relative stock density
#'    \item Proportional stock density
#'  }
#'
#' @concept Size Structure
#' @concept PSD
#' 
#' @section IFAR Chapter: 6-Size Structure.
#'
#' @seealso See \code{\link{psdVal}}, \code{\link{psdCalc}}, \code{\link{psdPlot}}, \code{\link{psdAdd}}, and \code{\link{tictactoe}} for related functionality.
#'
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#'
#' @source Original summary table from Dr. Michael Hansen, University of Wisconsin-Stevens Point. Additional species have been added by the package author from the literature.
#'
#' @keywords datasets
#'
#' @examples
#' str(PSDlit)
#' head(PSDlit)
#'
NULL
