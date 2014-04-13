#'Summarized multiple mark-recapture data for all northern pike from Buckthorn Marsh.
#'
#'Summary results of capture histories (number captured, number of recaptured
#'fish, and number of unmarked fish that were marked) for all Buckthorn Marsh
#'northern pike (\emph{Esox lucius}).
#'
#'@name PikeNY
#'@docType data
#'@format A data frame with 21 observations on the following 4 variables:
#'\describe{
#' \item{date}{Capture date.} 
#' \item{n}{Total fish captured in each sample.} 
#' \item{m}{Marked fish captured in each sample.} 
#' \item{R}{Marked fish returned to the population.} 
#'}
#'@section Topic(s): \itemize{
#' \item Population size
#' \item Abundance
#' \item Mark-recapture
#' \item Schnabel method
#' \item Schumacher-Eschmeyer method
#'}
#'@concept Abundance 'Population Size' 'Mark-Recapture' 'Schnabel'
#'@source New York Power Authority.  2004.  Use of buckhorn marsh and grand
#'island tributaries by northern pike for spawning and as a nursery. Technical
#'report, New York Power Authority, January 2004. Niagara Power Project (FERC
#'No. 2216).
#'@keywords datasets
#'@examples
#'data(PikeNY)
#'str(PikeNY)
#'head(PikeNY)
#'
NULL
