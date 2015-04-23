#' @title Show the Gompertz function formulae implemented in the FSA package.
#'
#' @description Show the Gompertz function formulae implemented in \code{\link{gompFuns}}.
#'
#' @param \dots Additional arguments for \code{\link{plot}}.  Generally not needed
#'
#' @return A graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{gompFuns}} for functions that represent the Gompertz parameterizations.
#' 
#' @references See references in \code{\link{gompFuns}}.
#'
#' @keywords manip hplot
#'
#' @examples
#' \dontrun{windows(5,5)}
#' gompModels()
#'
#' @export
gompModels <- function(...) {
  op <- par(mar=c(0,0,3,0),cex=1.25)
  plot(1,type="n",ylim=c(0,5),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Gompertz Parameterizations",...)
  iGrowthModels("gRicker1", 0.1,4.5)
  iGrowthModels("gRicker2", 0.1,3.5)
  iGrowthModels("gRicker3", 0.1,2.5)
  iGrowthModels("gQD3",     0.1,1.5)
  iGrowthModels("gOriginal",0.1,0.5)
  par(op)
}

## iGrowthModels internal function for plotting the different models is found in vbModels().
