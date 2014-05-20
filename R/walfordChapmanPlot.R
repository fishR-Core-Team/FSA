#'Constructs Ford-Walford and Chapman plots for mean length-at-age data.
#'
#'Constructs Ford-Walford and Chapman plots for mean length-at-age data and computes
#'estimates of asymptotic mean length (Linf) and the Brody growth coefficient (K).
#'
#'@aliases walfordPlot walfordPlot.default walfordPlot.formula chapmanPlot
#'chapmanPlot.default chapmanPlot.formula
#'
#'@param age Either a numeric vector of lengths ages or a formula of the form \code{len~age}.
#'@param len A numeric vector of observed lengths.  Ignored if \code{age} is a formula.
#'@param data A data frame in which \code{age} and \code{len} can be found.
#'@param xlab A string for labeling the x-axis.
#'@param ylab A string for labeling the y-axis.
#'@param colLS A string that indicates the color of the least-squares line.
#'@param colRL A string that indicates the color of the 1:1 reference line.
#'@param lwdLS A numeric that indicates the line width of the least-squares line.
#'@param lwdRL A numeric that indicates the line width of the 1:1 reference line.
#'@param ltyLS A numeric that indicates the line type of the least-squares line.
#'@param ltyRL A numeric that indicates the line type of the 1:1 reference line.
#'@param pch A numeric that indicates the plotting character type for the observed data.
#'@param col A string that indicates the color for the observed data.
#'@param pchLinf A numeric that indicates the plotting character type that indicates
#'the Linf value.
#'@param colLinf A string that indicates the color of the point that indicates the Linf
#'value.
#'@param cexLinf A numeric that indicates the character expansion factor of the
#'point that indicates the Linf value.
#'@param showLS A logical that indicates whether the coefficients from the
#'least-squares fit should be shown on the plot.
#'@param showVB A logical that indicates whether the estimate of Linf and K should
#'be shown on the plot.
#'@param \dots Other arguments to be passed to the \code{plot} function.
#'
#'@return None.  However, a plot is produced.
#'
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#'@seealso \code{\link{vbStarts}}
#'
#'@keywords hplot
#'
#'@examples
#'data(SMBassWB)
#'walfordPlot(lencap~agecap,data=SMBassWB)
#'chapmanPlot(lencap~agecap,data=SMBassWB)
#'
#'@rdname walfordChapmanPlot
#'@export walfordPlot
walfordPlot <- function(age,...) {
  UseMethod("walfordPlot")  
}

#'@rdname walfordChapmanPlot
#'@export
walfordPlot.default <- function(age,len,xlab=expression(L[t]),ylab=expression(L[t+1]),colLS="blue",colRL="red",lwdLS=2,lwdRL=2,ltyLS=1,ltyRL=2,pch=19,col="black",pchLinf=9,colLinf="red",cexLinf=1.5,showLS=TRUE,showVB=TRUE,...) {
  meanL <- tapply(len,age,mean,na.rm=TRUE)
  Lt1 <- meanL[-1]
  Lt <- meanL[-length(meanL)]
  lm1 <- lm(Lt1~Lt)
  a <- coef(lm1)[1]
  b <- coef(lm1)[2]
  K <- -log(b)
  Linf <- a/(1-b)
  plot(Lt1~Lt,xlim=c(min(Lt),max(c(Lt,Linf))),ylim=c(min(Lt1),max(c(Lt1,Linf))),xlab=xlab,ylab=ylab,pch=pch)
  abline(coef=c(0,1),lwd=lwdRL,lty=ltyRL,col=colRL)
  abline(coef=coef(lm1),lwd=lwdLS,lty=ltyLS,col=colLS)
  if (showLS) legend("bottomright",legend=c(paste("slope =",formatC(b,digits=3,format="f")),paste("intercept =",formatC(a,digits=1,format="f"))),bty="n")
  if (showVB) legend("topleft",legend=c(paste("Linf =",formatC(Linf,digits=1,format="f")),paste("K =",formatC(K,digits=3,format="f"))),bty="n")
  points(Linf,Linf,pch=pchLinf,cex=cexLinf,col=colLinf)
}

#'@rdname walfordChapmanPlot
#'@export
walfordPlot.formula <- function(age,data=NULL,...) {
  mf <- model.frame(age,data)
  walfordPlot.default(mf[,2],mf[,1],...)
}

#'@rdname walfordChapmanPlot
#'@export chapmanPlot
chapmanPlot <- function(age,...) {
  UseMethod("chapmanPlot")  
}

#'@rdname walfordChapmanPlot
#'@export
chapmanPlot.default <- function(age,len,xlab=expression(L[t]),ylab=expression(L[t+1]-L[t]),colLS="blue",ltyLS=1,lwdLS=2,pch=19,col="black",pchLinf=9,colLinf="red",cexLinf=1.5,showLS=TRUE,showVB=TRUE,...) {
  meanL <- tapply(len,age,mean,na.rm=TRUE)
  dLt <- diff(meanL)
  Lt <- meanL[-length(meanL)]
  lm1 <- lm(dLt~Lt)
  a <- coef(lm1)[1]
  b <- coef(lm1)[2]
  K <- -log(1+b)
  Linf <- -a/b
  plot(dLt~Lt,xlim=c(min(Lt),max(c(Lt,Linf))),ylim=c(0,max(dLt)),xlab=xlab,ylab=ylab,pch=pch)
  abline(coef=coef(lm1),lwd=lwdLS,lty=ltyLS,col=colLS)
  abline(h=0)
  if (showLS) legend("bottomleft",legend=c(paste("slope =",formatC(b,digits=3,format="f")),paste("intercept =",formatC(a,digits=1,format="f"))),bty="n")
  if (showVB) legend("topright",legend=c(paste("Linf =",formatC(Linf,digits=1,format="f")),paste("K =",formatC(K,digits=3,format="f"))),bty="n")
  points(Linf,0,pch=pchLinf,cex=cexLinf,col=colLinf)
}

#'@rdname walfordChapmanPlot
#'@export
chapmanPlot.formula <- function(age,data=NULL,...) {
  mf <- model.frame(age,data)
  chapmanPlot.default(mf[,2],mf[,1],...)
}
