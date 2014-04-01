#'Plots typical fisheries stock-recruitment models with cobwebbing traces.
#'
#'Plots typical fisheries stock-recruitment models with cobwebbing traces for four
#'parameterizations of the Beverton-Holt and three parameterizations of the Ricker
#'stock-recruit models.
#'
#'This function can be used to explore the dynamics of stock-recruitment models for
#'various parameter choices.
#'
#'The \code{type=} argument is used to choose either the \code{"BevertonHolt"}
#'or \code{"Ricker"} models.  Different parameterizations of these two models
#'are chosen with the \code{param=} argument.  Four paramaterizations of the Beverton-Holt
#'model and three parameterizations of the Ricker model are allowed.  See \code{\link{srModels}}
#'and \code{\link{srFuns}} for a representation of each parameterization.
#'
#'@aliases srCobWeb
#'@param type A single string indicating which stock-recruitment model to use.  
#'See \code{\link{srModels}}.
#'@param param A single numeric indicating which parameterization of the \code{type}
#'to use.  See \code{\link{srModels}}.
#'@param N A single numeric indicating the number of iterations to run for the cobwebbing.
#'@param initS A single numeric indicating the starting value of spawning stock size.
#'@param N2ignore A single numeric indicating the number of initial iterattions to
#'ignore when plotting the results.  This allows the function to \sQuote{burn-in}
#'before showing results.
#'@param \dots Optional parameters that are used in stock-recruitment model indicated
#'with \code{type} and \code{param}.  See \code{\link{srModels}} and \code{\link{srFuns}}\
#'for the list and meanings of the parameters.  Also see the examples..
#'@return None.  However a two-paned graphic is printed.  The top pane represents
#'the stock-recruit function as a blue line and the 1:1 replacement line as a red
#'line.  In addition, the \sQuote{cob-webbing} procedure is shown by a gray line.
#'The gray line of the cobwebbing will become \sQuote{darker} as more cobwebbing
#'lines are plotted on top of each other.  This makes it easer to see periodicity
#'if it exists.  The \sQuote{stopping} point in the cobwebbing is shown as a red
#'dot and the \sQuote{starting} point is shown, as long as \code{N2Ignore=0}, a 
#'green dot.  The bottom panel is the stock size plotted against time.
#'@seealso \code{\link{srModels}} and \code{\link{srFuns}}; \code{srSim} in \pkg{FSATeach}
#'@keywords iplot
#'@examples
#'# First parametrization of the Ricker model
#'#   exhibiting various dynamics
#'srCobWeb(type="Ricker",param=1,a=5,b=0.01)
#'srCobWeb(type="Ricker",param=1,a=7,b=0.01)
#'srCobWeb(type="Ricker",param=1,a=10,b=0.01)
#'srCobWeb(type="Ricker",param=1,a=13,b=0.01)
#'srCobWeb(type="Ricker",param=1,a=14.5,b=0.01)
#'srCobWeb(type="Ricker",param=1,a=15,b=0.01)
#'srCobWeb(type="Ricker",param=1,a=16,b=0.01)
#'srCobWeb(type="Ricker",param=1,a=17,b=0.01)
#'
#'# demonstration N2ignore
#'srCobWeb(type="Ricker",param=1,a=13,b=0.01,N2ignore=30)
#'
#'# Third parametrization of the Ricker model
#'srCobWeb(type="Ricker",param=3,a=13,Rp=300)
#'
#'# First parametrization of the Beverton-Holt model
#'srCobWeb(type="BevertonHolt",param=1,a=20,b=0.01)
#'
#'@rdname srCobWeb
#'@export srCobWeb
#'
srCobWeb <- function(type=c("BevertonHolt","Ricker"),param=1,N=100,initS=50,N2ignore=0,...) {
  type <- match.arg(type)
  # Get model function
  mdl <- srFuns(type=type,param=param)
  # Iterate N times
  S <- numeric(N)
  S[1] <- initS
  for (i in 2:N) S[i] <- mdl(S[i-1],...)
  
  opar <- par(mfrow=c(2,1),mar=c(3.5,3.5,0.5,0.5),mgp=c(2,0.75,0)); on.exit(par <- opar)
  # make base cobweb plot
  x <- 0:ceiling(max(S))
  y <- mdl(x,...)
  # The curve
  plot(y~x,type="l",lwd=4,col="blue",xlim=c(0,max(c(x,y))),ylim=c(0,max(c(x,y))),xlab="Spawners / Stock",ylab="Recruits")
  # The replacement line
  abline(a=0,b=1,lwd=3,col="red")
  # add cobwebs
  clrs <- rgb(0,0,0,1/(N/20))
  for (i in (N2ignore+1):(N-1)) {
    # go vertical to the function line
    if (i==1) {
      points(S[i],0,col="green",pch=19)                              # mark start
      lines(c(S[i],S[i]),c(0,S[i+1]),lwd=2,col=clrs)
    } else lines(c(S[i],S[i]),c(S[i],S[i+1]),lwd=2,col=clrs)
    # go horizontal to replacement line
    if (i==(N-1)) points(S[i],S[i+1],col="red",pch=19)               # mark end
    else lines(c(S[i],S[i+1]),c(S[i+1],S[i+1]),lwd=2,col=clrs)
  }
  # Time plot
  plot(S,type="l",xlab="Time",ylab="Spawners / Stock")
  points(((N2ignore+1):N),S[(N2ignore+1):N],pch=16,cex=0.75)
}
