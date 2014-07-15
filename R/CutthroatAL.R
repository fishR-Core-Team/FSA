#' @title Capture histories (9 samples) of Cutthroat Trout from Auke Lake.
#'
#' @description Individual capture histories of Cutthroat Trout (\emph{Oncorhynchus clarki}) in Auke Lake, Alaska, from samples taken in 1998-2006.
#'
#' @name CutthroatAL
#' 
#' @docType data
#' 
#' @format A data frame with 1684 observations on the following 10 variables.
#'  \describe{
#'    \item{id}{Unique identification numbers for each fish.}
#'    \item{y1998}{Indicator variable for whether the fish was captured in 1998 (\code{1}=captured).}
#'    \item{y1999}{Indicator variable for whether the fish was captured in 1999 (\code{1}=captured).}
#'    \item{y2000}{Indicator variable for whether the fish was captured in 2000 (\code{1}=captured).}
#'    \item{y2001}{Indicator variable for whether the fish was captured in 2001 (\code{1}=captured).}
#'    \item{y2002}{Indicator variable for whether the fish was captured in 2002 (\code{1}=captured).}
#'    \item{y2003}{Indicator variable for whether the fish was captured in 2003 (\code{1}=captured).}
#'    \item{y2004}{Indicator variable for whether the fish was captured in 2004 (\code{1}=captured).}
#'    \item{y2005}{Indicator variable for whether the fish was captured in 2005 (\code{1}=captured).}
#'    \item{y2006}{Indicator variable for whether the fish was captured in 2006 (\code{1}=captured).}
#'  }
#'
#' @section Topic(s):
#'  \itemize{
#'    \item Population size
#'    \item Abundance
#'    \item Mark-recapture
#'    \item Jolly-Seber method
#'    \item Capture history 
#'  }
#' 
#' @concept Abundance 'Population Size' 'Mark-Recapture' 'Jolly-Seber' 'Capture History'
#' 
#' @source Entered from Appendix A.3 of Harding, R.D., C.L. Hoover, and R.P. Marshall. 2010.  \href{http://www.sf.adfg.state.ak.us/FedAidPDFs/FDS10-82.pdf}{Abundance of Cutthroat Trout in Auke Lake, Southeast Alaska, in 2005 and 2006}.  Alaska Department of Fish and Game Fisheries Data Series No. 10-82 into \dQuote{RMark} format (see \code{CutthroatALf} in \pkg{FSAdata}) and then converted to individual format with \code{\link{capHistConvert}}.
#' 
#' @seealso See \code{\link{mrOpen}} for an example analysis.
#' 
#' @keywords datasets
#' 
#' @examples
#' data(CutthroatAL)
#' str(CutthroatAL)
#' head(CutthroatAL)
#'
NULL
