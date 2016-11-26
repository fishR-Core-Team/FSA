#' @title Plots a binary response variable versus a quantitative explanatory variable.
#'
#' @description A function to plot a binary response variable versus a quantitative explanatory variable.
#'
#' @details This function produces a plot that can be used to visualize the density of points for a binary response variable as a function of a quantitative explanatory variable.  In addition, the proportion of \dQuote{1}s for the response variable at various \dQuote{levels} of the explanatory variable are shown.
#'
#' @note This function is meant to allow newbie students the ability to visualize the data corresponding to a binary logistic regression without getting \dQuote{bogged-down} in the gritty details of how to produce this plot.
#'
#' @aliases plotBinResp plotBinResp.default plotBinResp.formula
#'
#' @param x A quantitative explanatory variable or a formula of the form \code{factor~quant}.
#' @param y A binary response variable.
#' @param data The data frame from which the formula should be evaluated.
#' @param xlab A string for labelling the x-axis.
#' @param ylab A string for labelling the y-axis.
#' @param plot.pts A logical that indicates (\code{TRUE} (default)) whether the points should be plotted (\code{TRUE}; default) or not (\code{FALSE}).
#' @param col.pt A string used to indicate the color of the plotted points.  Will be transparent unless \code{transparency=1}.
#' @param transparency A numeric that indicates how many points would be plotted on top of each other before the \sQuote{point} would have the full \code{col.pt} color.  The reciprocal of this value is the alpha transparency value.
#' @param plot.p A logical that indicates if the proportion for categorized values of X are plotted (\code{TRUE}; default).
#' @param breaks A number that indicates how many intervals over which to compute proportions or a numeric vector that contains the endpoints of the intervals over which to compute proportions if \code{plot.p=TRUE}.
#' @param p.col A color to plot the proportions.
#' @param p.pch A plotting character for plotting the proportions.
#' @param p.cex A character expansion factor for plotting the proportions.
#' @param yaxis1.ticks A numeric vector that indicates where tick marks should be placed on the left y-axis (for the proportion of \sQuote{successes}).
#' @param yaxis1.lbls A numeric vector that indicates labels for the tick marks on the left y-axis (for the proportion of \sQuote{successes}).
#' @param yaxis2.show A logical that indicates whether the right y-axis should be created (\code{=TRUE}; default) or not.
#' @param \dots Other arguments to be passed to the plot functions.
#'
#' @return None.  However, a plot is produced.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso \code{\link{fitPlot}} and \code{\link{cdplot}}.
#'
#' @keywords hplot models
#'
#' @examples
#' ## NASA space shuttle o-ring failures -- from graphics package
#' fail <- factor(c(2,2,2,2,1,1,1,1,1,1,2,1,2,1,1,1,1,2,1,1,1,1,1),
#' levels = 1:2, labels = c("no","yes"))
#' temperature <- c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)
#' d <- data.frame(temperature,fail,fail2=factor(fail,levels=c("yes","no")))
#'
#' ## Default plot (using formula notation)
#' plotBinResp(fail~temperature,data=d)
#' plotBinResp(fail2~temperature,data=d)
#'
#' ## Controlling where proportions are computed with a sequence in breaks
#' plotBinResp(fail~temperature,data=d,breaks=seq(50,85,5))
#'
#' ## Controlling where proportions are computed with an integer in breaks
#' plotBinResp(fail~temperature,data=d,breaks=10)
#' 
#' ## Controlling where proportions are computed at each value of x
#' plotBinResp(fail~temperature,data=d,breaks=NULL)
#'
#' ## Don't plot points, just plot proportions
#' plotBinResp(fail~temperature,data=d,plot.pts=FALSE)
#'
#' ## Don't plot proportions, just plot points
#' plotBinResp(fail~temperature,data=d,plot.p=FALSE)
#'
#' ## Change points colors, and eliminate transparency
#' plotBinResp(fail~temperature,data=d,col.pt="red",transparency=1)
#'
#' ## Remove the right y-axis
#' plotBinResp(fail~temperature,data=d,yaxis2.show=FALSE)
#'
#' ## Change left y-axis ticks
#' plotBinResp(fail~temperature,data=d,yaxis1.ticks=c(0,1),yaxis1.lbls=c(0,1))
#'
#' @rdname plotBinResp
#' @export
plotBinResp <- function(x,...) { # nocov start
  UseMethod("plotBinResp") 
} # nocov end

#' @rdname plotBinResp
#' @export
plotBinResp.default <- function(x,y,
    xlab=paste(deparse(substitute(x))),ylab=paste(deparse(substitute(y))),
    plot.pts=TRUE,col.pt="black",transparency=NULL,
    plot.p=TRUE,breaks=25,p.col="blue",p.pch=3,p.cex=1.25,
    yaxis1.ticks=seq(0,1,0.1),yaxis1.lbls=c(0,0.5,1),yaxis2.show=TRUE,...) { # nocov start
  # convert factor to 0s and 1s
  if (is.factor(y)) yn <- as.numeric(y)-1
    else yn <- y
  # will cause points not to be visible
  if (!plot.pts) col.pt="white"
  # make transparency value equal to max number of points that overlap
  if (is.null(transparency)) transparency <- max(tapply(yn,x,length))
  # adjust for maximum allowable transparency
  if (transparency>500) transparency <- 500
  # plot raw data points
  graphics::plot(yn~x,pch=16,col=col2rgbt(col.pt,1/transparency),yaxt="n",xlab=xlab,ylab=ylab,...)
  # puts on ticks
  graphics::axis(2,yaxis1.ticks,FALSE,cex.axis=graphics::par()$cex.axis)
  # only label a few
  graphics::axis(2,yaxis1.lbls,cex.axis=graphics::par()$cex.axis)
  if (yaxis2.show) graphics::axis(4,c(0,1),levels(y))
  # plot proportions points
  if (plot.p) {
    if (is.null(breaks)) {
      # if no p intervals defined on call then find ps for each value of x
      p.i <- tapply(yn,x,mean)  
      xs <- as.numeric(names(p.i)) 
    } else {
      if (length(breaks)==1) {
        # handle if just a number of breaks is given
        x.i <- lencat(x,startcat=min(x),w=round((max(x)-min(x))/breaks))
      } else {
        # handle if actual breaks are given
        x.i <- lencat(x,breaks=breaks)
      }
      p.i <- tapply(yn,x.i,mean)
      xs <- as.numeric(names(p.i))
      xs <- xs + min(diff(xs))/2
    }
    graphics::points(p.i~xs,pch=p.pch,col=p.col,cex=p.cex)
  }
} # nocov end

#' @rdname plotBinResp
#' @export
plotBinResp.formula <- function(x,data=NULL,xlab=names(mf)[2],ylab=names(mf)[1],...) {
  # nocov start
  mf <- stats::model.frame(x,data)
  x <- mf[,2]
  y <- mf[,1]
  plotBinResp.default(x,y,xlab=xlab,ylab=ylab,...)
} # nocov end
