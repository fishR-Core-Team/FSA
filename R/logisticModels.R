#' @title Show the logistic growth function formulae implemented in the FSA package.
#'
#' @description Show the logistic growth function formulae implemented in \code{\link{logisticFuns}}.
#'
#' @param \dots Additional arguments for \code{\link{plot}}.  Generally not needed
#'
#' @return A graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{logisticFuns}} for functions that represent the logistic growth function parameterizations.
#' 
#' @references See references in \code{\link{logisticFuns}}.
#'
#' @keywords manip hplot
#'
#' @examples
#' \dontrun{windows(5,5)}
#' logisticModels()
#'
#' @export
logisticModels <- function(...) {
  op <- par(mar=c(0,0,3,0),cex=1.25)
  plot(1,type="n",ylim=c(0,3),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Logistic Growth Parameterizations",...)
  iGrowthModels("CJ1", 0.1,2.5)
  iGrowthModels("CJ2", 0.1,1.5)
  iGrowthModels("Richards", 0.1,0.5)
  par(op)
}

## iGrowthModels internal function for plotting the different models is found in vbModels().
