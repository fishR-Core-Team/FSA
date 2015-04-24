#' @title Show the von Vertalanffy model formulas implemented in the FSA package.
#'
#' @description Show the von Bertalanffy model formulas implemented in \code{\link{vbFuns}} and \code{\link{vbStarts}}.
#'
#' @param \dots Additional arguments for \code{\link{par}}.  Generally not needed
#'
#' @return A graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth}.
#'
#' @seealso See \code{\link{vbFuns}} for functions that represent the von Bertalanffy parameterizations and \code{\link{vbStarts}} for methods to find starting values.
#' 
#' @references Ogle, D.H.  2016.  Introductory Fisheries Analyses with R.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#'
#' @keywords manip hplot
#'
#' @examples
#' \dontrun{windows(8,5)}
#' vbModels()
#'
#' @export
vbModels <- function(...) {
  op <- par(mar=c(0,0,3,0),...)
  plot(1,type="n",ylim=c(0,7),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA von Bertalanffy Parameterizations")
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
## Used in gompModels as well
iGrowthModels <- function(which,xpos,ypos) {
  switch(which,
         vbOriginal= {text(xpos,ypos,expression(plain("Original: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt}),pos=4)},
         vbTypical=  {text(xpos,ypos,expression(plain("Typical: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)},
         vbGQ=       {text(xpos,ypos,expression(plain("GQ: ")~~~E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)},
         vbMooij=    {text(xpos,ypos,expression(plain("Mooij: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t}),pos=4)},
         vbWeisberg= {text(xpos,ypos,expression(plain("Weisberg: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-frac(log(2),(K[0]~-~t[0]))*(t~-~t[0])},")")),pos=4)},
         vbSchnute=  {text(xpos,ypos,expression(plain("Schnute: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[3]~-~t[1])})),pos=4)},
         vbFrancis=  {text(xpos,ypos,expression(plain("Francis: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2})),pos=4)},
         vbFrancis2= {text(xpos,ypos,expression(plain("where" )~r==frac(L[3]-L[2],L[2]-L[1])),pos=4)},
         
         gOriginal=  {text(xpos,ypos,expression(plain("Original:         ")~~~E(L[t])==L[infinity]*~e^{-e^{a-g[i]*t}}),pos=4)},
         gRicker1=   {text(xpos,ypos,expression(plain("Ricker1:          ")~~~E(L[t])==L[infinity]*~e^{-e^{-g[i]*(t-t[i])}}),pos=4)},
         gRicker2=   {text(xpos,ypos,expression(plain("Ricker2, QD1: ")~~~E(L[t])==L[0]*~e^{a*bgroup("(",1-e^{-g[i]*t},")")}),pos=4)},
         gRicker3=   {text(xpos,ypos,expression(plain("Ricker3, QD2: ")~~~E(L[t])==L[infinity]*~e^{-a*~e^{-g[i]*t}}),pos=4)},
         gQD3=       {text(xpos,ypos,expression(plain("QD3:              ")~~~E(L[t])==L[infinity]*~e^{-~frac(1,g[i])*~e^{-g[i]*~(~t~-~t^{plain("*")})}}),pos=4)},
         
         CJ1=  {text(xpos,ypos,expression(plain("CJ1: ")~~~E(L[t])==frac(L[infinity],1+g[-infinity]*(t-t[i]))),pos=4)},
         CJ2=  {text(xpos,ypos,expression(plain("CJ2: ")~~~E(L[t])==frac(L[infinity],1+~ae^{-g[-infinity]*t})),pos=4)},
         Richards=  {text(xpos,ypos,expression(plain("Richards: ")~~~E(L[t])==frac(L[0]*L[infinity],L[0]+(L[infinity]-L[0])*~e^{-g[-infinity]*t})),pos=4)},
         
         Schnute1=  {text(xpos,ypos,expression(plain("Case 1: ")~~~E(L[t])==bgroup("[",L[1]^{gamma}+(L[3]^{gamma}-L[1]^{gamma})*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])}),"]")^{~frac(1,gamma)}),pos=4)},
         Schnute2=  {text(xpos,ypos,expression(plain("Case 2: ")~~~E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])})}),pos=4)},
         
         Schnute3=  {text(xpos,ypos,expression(plain("Case 3: ")~~~E(L[t])==bgroup("[",L[1]^{gamma}+(L[3]^{gamma}-L[1]^{gamma})*~frac(~t~-~t[1],~t[3]~-~t[1]),"]")^{~frac(1,gamma)}),pos=4)},
         Schnute4=  {text(xpos,ypos,expression(plain("Case 4: ")~~~E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(~t~-~t[1],~t[3]~-~t[1])}),pos=4)}
  ) # end swich
} ## end iGrowthModels internal function
