#'Constructs a plot of population size versus time from the projection of
#'Leslie matrix.
#'
#'Constructs a plot of population size versus time from the projection of
#'Leslie matrix using \code{pop.projection()} from the \pkg{popbio} package.
#'
#'@param object An object saved from \code{pop.projection()} from the
#'\pkg{popbio} package.
#'@param use.log A logical that indicates if the population sizes should be logged
#'before plotting.
#'@param xlab A string for labelling the x-axis.
#'@param ylab A string for labelling the y-axis.
#'@param type A type of plot -- use \code{l} for lines, \code{p} for points,
#'and \code{b} for both.
#'@param lwd A numeric that indicates the line width to use.
#'@param add A logical that indicates whether the data should be added to a
#'currently existing plot.
#'@param \dots Extra arguments to be sent to the default plot routine.
#'@return A plot of either population sizes or log population sizes versus time.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@note This function is meant to work with an object saved from
#'\code{pop.projection()} in the \pkg{popbio} package.  It is not a general
#'function for plotting population size versus time.
#'@seealso \code{pop.projection} and \code{stage.vectors.plot} in \pkg{popbio}.
#'@export
#'@keywords hplot
#'@examples
#'## These examples are not run because they require a dependency on the popbio package.
#'\dontrun{
#'require(popbio)
#'example(pop.projection)
#'popSizesPlot(p)
#'}
#'
popSizesPlot <- function(object,use.log=FALSE,xlab="Time",ylab="Population Size",type="l",lwd=2,add=FALSE,...) {
  t <- 0:(length(object$pop.sizes)-1)
  N <- object$pop.sizes
  if (use.log) {
    N <- log(N)
    if (ylab=="Population Size") ylab="log Population Size"
  }
  df <- data.frame(t,N)
  if (!add) plot(N~t,data=df,type=type,lwd=lwd,xlab=xlab,ylab=ylab,...)
    else points(N~t,data=df,type=type,lwd=lwd,...)
}
