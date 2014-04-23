#'Dynamics plots to explore length-weight power function models.
#'
#'Plots a length-weight power function evaluated at a vector of lengths with or
#'without observed data.  A set of slider bars is included so that the parameter 
#'values can be adjusted by the user to explore the effects of a parameter on the 
#'model or for visually fitting a length-weight model to a set of data.
#'
#'If the \code{x} and \code{y} arguments are used then the observed data in
#'these vectors is plotted and the length-weight model is superimposed.  If these
#'arguments are \code{NULL} then the length-weight model is shown without any 
#'observed data.  The former option is useful for visually fitting a model to
#'actual data.  The latter option is useful for exploring the effects of different
#'parameter values on the model dynamics.
#'
#'A list slider bars is created from which the parameters of the model -- W=a*L^b --
#'can be dynamically changed.  In addition, the minimum and maximum length over 
#'which to evaluate the length-weight model are controlled by the slider.  The
#'maximum weight plotted is controlled by an argument to the function.
#'
#'The plot object should be \dQuote{exit}ed when finished exploring the model.
#'
#'@note The range of values allowed for each of the parameters was chosen to allow
#'a wide variety of modeling opportunities.  However, it is highly likely that
#'these ranges do not encompass every possible set of values that a user may wish
#'to view.  Thus, this function is more useful as a learning tool rather than a
#'research simulation tool.
#'
#'@param x A numeric vector of observed ages.  See details.
#'@param y A numeric vector of observed lengths.  See details.
#'@param wt.max A single numeric that indicates the maximum weight for the y-axis scale.
#'@return None.  An interactive graphic is produced.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@keywords iplot
#'@examples
#'if (interactive()) {
#'
#'# Exploration of model dynamics
#'lengthWeightSim()
#'
#'# Visual fit of length-weight model to one set of ruffe length-weight data from the FSA package
#'library(FSA)
#'data(RuffeWs)
#'r1 <- Subset(RuffeWs,regrnum==220)
#'lengthWeightSim(x=r1$fl,y=r1$wt)
#'
#'} # end if interactive
#'@export
#'
lengthWeightSim <- function(x=NULL,y=NULL,wt.max=50) {
  ## Internal plotting function
  lwSimPlot <- function(x,y,wt.max,len.min,len.max,a,b) {
    len <- seq(len.min,len.max,length.out=(len.max-len.min)*5)
    vals <- a*len^b
    if (is.null(x)||is.null(y)) { 
      plot(len,vals,xlab="Length",ylab="Mean Weight",type="l",lwd=2,col="blue",ylim=c(0,wt.max))
    } else {
      plot(x,y,xlab="Length",ylab="Weight",ylim=c(0,max(y)),xlim=c(len.min,len.max),pch=16)
      lines(len,vals,type="l",lwd=2,col="blue")
    }    
  } ## end lwSimPlot internal function
  
  ## Internal refresh function
  refresh.lw <- function(...) {
    old.par <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.4,0), tcl=-0.2); on.exit(par(old.par))
    len.min <- relax::slider(no=1)
    len.max <- relax::slider(no=2)
    a <- relax::slider(no=3)
    b <- relax::slider(no=4)
    lwSimPlot(x,y,wt.max,len.min,len.max,a,b)
  } ## end refresh.lw internal function

  ## Main Function
  relax::gslider(refresh.lw,prompt=TRUE,
             sl.names=   c("Min Length", "Max Length",        "a", "b"),
             sl.mins=    c(          0,           50,  0.00000001,  2),
             sl.maxs=    c(        200,         1000,  0.00001,     4),
             sl.deltas=  c(          5,            5,  0.00000001,  0.01),
             sl.defaults=c(          0,          200,  0.000004,   3),
             title = "Length-Weight Power Function", pos.of.panel="left")
}
