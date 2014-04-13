#'Standard weight equations for various species.
#'
#'Parameters for standard weight equations for a variety of species.
#'
#'I constructed the English units minimum TL from the published minimum TL in mm by
#'rounding to what seemed like common units (inches, half inches, or quarter inches).
#'
#'@name WSlit
#'@docType data
#'@format A data frame with observations on the following 13 variables:
#'\describe{
#'\item{species}{Species name.}
#'\item{units}{Units of measurements.  \code{Metric} uses lengths in mm and weight in kilograms.  \code{English} uses lengths in inches and weight in pounds.}
#'\item{type}{Type of equation (\code{linear} or \code{quadratic}).}
#'\item{ref}{Reference quartile (\code{75}, \code{50}, or \code{25}).}
#'\item{int}{The intercept for the model.}
#'\item{slope}{The slope for the linear models or the linear coefficient for the quadratic equation.}
#'\item{quad}{The quadratic coefficient in the quadratic equations.}
#'\item{min.len}{Minimum total length (mm or in, depending on \code{units}) for which the equation should be applied.}
#'\item{max.len}{Maximum total length (mm or in, depending on \code{units}) for which the equation should be applied.}
#'\item{measure}{The type of length measurement used (\code{TL} or \code{FL}).}
#'\item{method}{The type of method used to derive the equation (\code{RLP},\code{EmP}, or \code{Other}).}
#'\item{comment}{Comments about use of equation.}
#'\item{source}{Source of the equation.}
#'}
#'@section Topic(s): \itemize{
#'\item Relative weight
#'\item Standard weight
#'\item Condition
#'}
#'@concept Condition 'Relative Weight' 'Standard Weight'
#'@seealso \code{\link{wsVal}}, \code{\link{wrAdd}}, and \code{\link{wrDataPrep}}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}.
#'@source Most of the data presented came from Blackwell, B.G., M.L. Brown, and
#'D.W. Willis.  2000.  Relative weight (Wr) status and current use in fisheries
#'assessment and management.  Reviews in Fisheries Science 8:1-44.  Available
#'at \url{http://pubstorage.sdstate.edu/wfs/280-F.pdf}.
#'
#'Other species have been added by the package author since this review was published.
#'@keywords datasets
#'@examples
#'data(WSlit)
#'str(WSlit)
#'head(WSlit)
#'
NULL
