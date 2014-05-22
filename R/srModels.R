#'Show the stock-recruitment model formulas implemented in FSA.
#'
#'Show the stock-recruitment model formulas implemented in \code{\link{srFuns}}, \code{\link{srStarts}}, and \code{srSim}.
#'
#' @param \dots Additional arguments for \code{plot}.  Generally not needed.
#'
#' @return A graphic that uses \code{plotmath} to show the model formulas in a pretty format.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{srFuns}}, \code{\link{srStarts}}, and \code{\link{srSim}}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/StockRecruit.pdf}
#'
#' @keywords manip hplot
#'
#' @examples
#'srModels()
#'
#' @export
srModels <- function(...) {
  op <- par(mar=c(0,0,2,0))
  plot(1,type="n",ylim=c(0,6),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Stock-Recruit Model Parametrizations",...)
  text(0,5.5,expression(paste("BevertonHolt #1: ",~~~R==frac(aS,1+bS))),pos=4)
  text(0,4,expression(paste("BevertonHolt #2: ",~~~R==frac(aS,1+a*~frac(S,R[p])))),pos=4)
  text(0,2.5,expression(paste("BevertonHolt #3: ",~~~R==frac(S,tilde(a)+tilde(b)*S))),pos=4)
  text(0,1,expression(paste("BevertonHolt #4: ",~~~R==frac(S,tilde(a)+frac(S,R[p])))),pos=4)  

  abline(v=0.5)
  text(0.55,5.5,expression(paste("Ricker #1: ",~~~R==aSe^{-bS})),pos=4)
  text(0.55,4,expression(paste("Ricker #2: ",~~~R==Se^{tilde(a)-bS})),pos=4)
  text(0.55,2.5,expression(paste("Ricker #3: ",~~~R==aSe^{-a*~frac(S,R[p]*~e)})),pos=4)
  par(op)
}
