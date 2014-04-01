#'Show the growth model formulas implemented in the FSA package.
#'
#'Show the growth model formulas implemented in \code{\link{vbFuns}},
#'\code{\link{vbStarts}}, and \code{growthModelSim} in \pkg{FSATeach}.
#'
#'@aliases growthModels vbModels
#'@param \dots Additional arguments for \code{plot}.  Generally not needed
#'@return A graphic that uses \code{plotmath} to show the model formulas in a
#'pretty format. \code{vbModels()} shows just the von Bertalanffy models.
#'@seealso \code{\link{vbFuns}}, \code{\link{vbStarts}}, \code{growthModelSim}
#'in \pkg{FSATeach}
#'@export growthModels vbModels
#'@keywords manip hplot
#'@examples
#'growthModels()
#'vbModels()
#'
growthModels <- function(...) {
  op <- par(mar=c(0,0,2,0))
  plot(1,type="n",ylim=c(0,10),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Growth Model Parametrizations",...)
  text(0,9.5,expression(plain("vbOriginal: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt}),pos=4)
  text(0,8,expression(plain("vbTypical: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)
  text(0,6.5,expression(plain("vbGallucciQuinn: ")~~~E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)
  text(0,5,expression(plain("vbMooij: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t}),pos=4)
  text(0,3.5,expression(plain("vbSchnute: ")~~~E(L[t])==L[1]+(L[2]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[2]~-~t[1])})),pos=4)
  text(0,2,expression(plain("vbFrancis: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2})),pos=4)
  text(0.15,0.75,expression(plain("where" )~r==frac(L[3]-L[2],L[2]-L[1])),pos=4)
  abline(v=0.55)
  text(0.55,9.5,expression(plain("Gomp1: ")~~~E(L[t])==L[0]*~e^{G*bgroup("(",1-e^{-gt},")")}),pos=4)
  text(0.55,8,expression(plain("Gomp2: ")~~~E(L[t])==L[infinity]*~e^{-g*(t-t^symbol("\052"))}),pos=4)
  text(0.55,6.5,expression(plain("Gomp3: ")~~~E(L[t])==L[infinity]*~e^{-~frac(1,g)*~bgroup("(",e^{-g*~(~t~-~t[0])},")")}),pos=4)
  par(op)
}


vbModels <- function(...) {
  op <- par(mar=c(0,0,3,0))
  plot(1,type="n",ylim=c(0,7),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA von Bertalanffy Parametrizations",...)
  text(0,6.5,expression(plain("Original: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt}),pos=4)
  text(0,4.5,expression(plain("Typical: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)
  text(0,2.5,expression(plain("GallucciQuinn: ")~~~E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)
  abline(v=0.5)
  text(0.5,6.5,expression(plain("Mooij: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t}),pos=4)
  text(0.5,4.5,expression(plain("Schnute: ")~~~E(L[t])==L[1]+(L[2]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[2]~-~t[1])})),pos=4)
  text(0.5,2.5,expression(plain("Francis: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2})),pos=4)
  text(0.65,1,expression(plain("where" )~r==frac(L[3]-L[2],L[2]-L[1])),pos=4)
  par(op)
}
