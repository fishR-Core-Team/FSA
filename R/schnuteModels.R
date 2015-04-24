#' @title Show the cases for the Schnute growth function formulae implemented in the FSA package.
#'
#' @description Show the cases for the Schnute growth function formulae implemented in \code{\link{schnute}}.
#'
#' @param \dots Additional arguments for \code{\link{plot}}.  Generally not needed
#'
#' @return A graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{schnute}} for functions that represent the Schnute growth function cases.
#' 
#' @references Schnute, J.  1981.  A versatile growth model with statistical stable parameters.  Canadian Journal of Fisheris and Aquatic Sciences 38:1128-1140.
#'
#' @keywords manip hplot
#'
#' @examples
#' \dontrun{windows(5,5)}
#' schnuteModels()
#'
#' @export
schnuteModels <- function(...) {
  op <- par(mar=c(0,0,3,0),cex=1.25)
  plot(1,type="n",ylim=c(0,4),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Schnute Growth Model Cases",...)
  iGrowthModels("Schnute1", 0.1,3.5)
  iGrowthModels("Schnute2", 0.1,2.5)
  iGrowthModels("Schnute3", 0.1,1.5)
  iGrowthModels("Schnute4", 0.1,0.5)
  par(op)
}

## iGrowthModels internal function for plotting the different models is found in vbModels().
