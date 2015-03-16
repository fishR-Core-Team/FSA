#' @title Show the von Vertalanffy model formulas implemented in the FSA package.
#'
#' @description Show the von Bertalanffy model formulas implemented in \code{\link{vbFuns}} and \code{\link{vbStarts}}.
#'
#' @aliases vbModels
#'
#' @param \dots Additional arguments for \code{plot}.  Generally not needed
#'
#' @return A graphic that uses \code{plotmath} to show the model formulas in a pretty format.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link{vbFuns}} for functions that represent the von Bertalanffy parameterizations and \code{\link{vbStarts}} for methods to find starting values.
#'
#' @keywords manip hplot
#'
#' @examples
#' \dontrun{windows(8,5)}
#' vbModels()
#'
#' @export
vbModels <- function(...) {
  op <- par(mar=c(0,0,3,0))
  plot(1,type="n",ylim=c(0,7),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA von Bertalanffy Parameterizations",...)
  iGrowthModels("vbOriginal",0,6.0)
  iGrowthModels("vbTypical", 0,4.0)
  iGrowthModels("vbGQ",      0,2.0)
  iGrowthModels("vbMooij",   0,0.5)
  abline(v=0.5)
  iGrowthModels("vbWeisberg",0.50,6.0)
  iGrowthModels("vbSchnute", 0.50,4.0)
  iGrowthModels("vbFrancis", 0.50,2.0)
  iGrowthModels("vbFrancis2",0.65,0.5)
  par(op)
}

## Internal function for plotting the different models.  Send positions in xpos and ypos.
iGrowthModels <- function(which,xpos,ypos) {
  switch(which,
   vbOriginal= {text(xpos,ypos,expression(plain("vbOriginal: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt}),pos=4)},
   vbTypical=  {text(xpos,ypos,expression(plain("vbTypical: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)},
   vbGQ=       {text(xpos,ypos,expression(plain("vbGQ: ")~~~E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)},
   vbMooij=    {text(xpos,ypos,expression(plain("vbMooij: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t}),pos=4)},
   vbWeisberg= {text(xpos,ypos,expression(plain("vbWeisberg: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-frac(log(2),(K[0]~-~t[0]))*(t~-~t[0])},")")),pos=4)},
   vbSchnute=  {text(xpos,ypos,expression(plain("vbSchnute: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[3]~-~t[1])})),pos=4)},
   vbFrancis=  {text(xpos,ypos,expression(plain("vbFrancis: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2})),pos=4)},
   vbFrancis2= {text(xpos,ypos,expression(plain("where" )~r==frac(L[3]-L[2],L[2]-L[1])),pos=4)},
   Gompertz1=  {text(xpos,ypos,expression(plain("Gompertz1: ")~~~E(L[t])==L[0]*~e^{G*bgroup("(",1-e^{-gt},")")}),pos=4)},
   Gompertz2=  {text(xpos,ypos,expression(plain("Gompertz2: ")~~~E(L[t])==L[infinity]*~e^{-g*(t-t^symbol("\052"))}),pos=4)},
   Gompertz3=  {text(xpos,ypos,expression(plain("Gompertz3: ")~~~E(L[t])==L[infinity]*~e^{-~frac(1,g)*~bgroup("(",e^{-g*~(~t~-~t[0])},")")}),pos=4)}
  ) # end swich
} ## end iGrowthModels internal function
