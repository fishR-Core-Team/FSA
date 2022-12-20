#' @title Catch-effort data for Little Silver Lake (Ont) Smallmouth Bass.
#'
#' @description Catch-effort data for Smallmouth Bass (\emph{Micropterus dolomieu}) in Little Silver Lake, Ont.
#'
#' @name SMBassLS
#'
#' @docType data
#'
#' @format A data frame with 10 observations on the following 3 variables:
#'  \describe{
#'    \item{day}{Day of the catch}
#'    \item{catch}{Number of smallmouth bass caught}
#'    \item{effort}{Number of traps set per day}
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Population size 
#'    \item Abundance
#'    \item Depletion methods 
#'    \item Leslie method
#'    \item DeLury method 
#'    \item Catchability
#'  }
#'
#' @concept Abundance
#' @concept Population Size
#' @concept Depletion
#' @concept Leslie
#' @concept DeLury
#' @concept Catchability
#'
#' @source From Omand, D.N. 1951. A study of populations of fish based on catch-effort statistics. Journal of Wildlife Management, 15:88-98. \href{https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/SMBassLS.csv}{CSV file}
#'
#' @seealso Used in \code{\link{depletion}} examples.
#' 
#' @keywords datasets
#'
#' @examples
#' str(SMBassLS)
#' head(SMBassLS)
#'
NULL
