#' @title Show the stock-recruitment model formulas implemented in FSA.
#'
#' @description Show the stock-recruitment model formulas implemented in \code{\link{srFuns}}, \code{\link{srStarts}}, and \code{srSim}.
#'
#' @param \dots Additional arguments for \code{plot}.  Generally not needed.
#'
#' @return A graphic that uses \code{plotmath} to show the model formulas in a pretty format.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{srFuns}} and \code{\link{srStarts}}.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/StockRecruit.pdf}
#'
#' @keywords manip hplot
#'
#' @examples
#' \dontrun{windows(6,5)}
#' srModels()
#'
#' @export
srModels <- function(...) {
  op <- par(mar=c(0,0,2,0))
  plot(1,type="n",ylim=c(0,6),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
       main="FSA Stock-Recruit Model Parametrizations",...)
  iSRModels("BH1",0,5.5)
  iSRModels("BH2",0,4.25)
  iSRModels("BH3",0,3)
  iSRModels("BH4",0,1.75)

  iSRModels("R1",0.55,5.5)
  iSRModels("R2",0.55,4.25)
  iSRModels("R3",0.55,3)
  iSRModels("Shepherd",0.55,1.75)
  iSRModels("SailaLorda",0.55,0.5)
  par(op)
}

## Internal function for plotting the different models.  Send positions in xpos and ypos.
iSRModels <- function(which,xpos,ypos) {
  switch(which,
         BH1={text(xpos,ypos,expression(plain("BevertonHolt #1: ")~~~R==frac(aS,1+bS)),pos=4)},
         BH2={text(xpos,ypos,expression(plain("BevertonHolt #2: ")~~~R==frac(aS,1+a*~frac(S,R[p]))),pos=4)},
         BH3={text(xpos,ypos,expression(plain("BevertonHolt #3: ")~~~R==frac(S,tilde(a)+tilde(b)*S)),pos=4)},
         BH4={text(xpos,ypos,expression(plain("BevertonHolt #4: ")~~~R==frac(S,tilde(a)+frac(S,R[p]))),pos=4)},
         R1 ={text(xpos,ypos,expression(plain("Ricker #1: ")~~~R==aSe^{-bS}),pos=4)},
         R2 ={text(xpos,ypos,expression(plain("Ricker #2: ")~~~R==Se^{tilde(a)-bS}),pos=4)},
         R3 ={text(xpos,ypos,expression(plain("Ricker #3: ")~~~R==aSe^{-a*~frac(S,R[p]*~e)}),pos=4)},
         Shepherd={text(xpos,ypos,expression(plain("Shepherd: ")~~~R==frac(aS,1+(bS)^{c})),pos=4)},
         SailaLorda={text(xpos,ypos,expression(plain("Saila-Lorda: ")~~~R==aS^{c}*e^{-bS}),pos=4)}
  )
}