#' @title DEPRECATED -- Fitted model plot for an lm, glm, or nls object.
#'
#' @description A generic function for constructing a fitted model plot for an \code{lm}, \code{glm}, or \code{nls} object. Supported objects are linear models from simple linear regression (SLR), indicator variable regression (IVR), one-way ANOVA, or two-way ANOVA models; general linear models that are logistic regressions with a binary response; and non-linear regression with a single numerical response variable, at least one continuous explanatory variable and up to two group-factor explanatory variables.
#'
#' @details This function does not work with a multiple linear regression, indicator variable regressions with more than two factors, ANOVAs other than one-way and two-way, or models with a categorical response variable. In addition, if the linear model contains a factor then the model must be fit with the quantitative explanatory variable first, followed by the factor(s). This function only works for non-linear models with two or fewer groups.
#'
#' This function is basically a wrapper to a variety of other functions. For one-way or two-way ANOVAs the primary functions called are \code{interaction.plot} and \code{lineplot.CI}. For simple linear regression the function performs similarly to \code{abline} except that the line is constrained to the domain. For indicator variable regression the function behaves as if several \code{abline} functions had been called.
#'
#' A legend can be added to the plot in three different ways. First, if \code{legend = TRUE} then the R console is suspended until the user places the legend on the graphic by clicking on the graphic at the point where the upper-left corner of the legend should appear. Second, the \code{legend=} argument can be set to one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} and \code{"center"}. In this case, the legend will be placed inside the plot frame at the given location. Finally, the \code{legend=} argument can be set to a vector of length two which identifies the plot coordinates for the upper-left corner of where the legend should be placed. A legend will not be drawn if \code{legend = FALSE} or \code{legend = NULL}. A legend also will not be drawn if there are not multiple groups in the model.
#'
#' @note This function is meant to allow newbie students the ability to visualize the most common linear models found in an introductory or intermediate level undergraduate statistics course without getting \dQuote{bogged-down} in the gritty details of a wide variety of functions. This generic function and it's S3 functions allow the student to visualize the means plot of a one-way anova, the main effects and interaction plots of a two-way ANOVA, the fit of a simple linear regression, the fits of many lines in an indicator variable regression, and the fit of a non-linear model with a simple and mostly common set of arguments -- generally, all that is required is a fitted linear model of the type mentioned here as the first argument. This function thus allows newbie students to interact with and visualize moderately complex linear models in a fairly easy and efficient manner. THIS IS NOT A RESEARCH GRADE FUNCTION and the user should learn how to use the functions that this function is based on, build plots from \dQuote{scratch}, or use more sophisticated plotting packages (e.g., \pkg{ggplot2} or \pkg{lattice}).
#' 
#' @aliases fitPlot fitPlot.lm fitPlot.SLR fitPlot.IVR fitPlot.POLY fitPlot.ONEWAY fitPlot.TWOWAY fitPlot.nls fitPlot.glm fitPlot.logreg
#'
#' @param object An \code{lm} or \code{nls} object (i.e., returned from fitting a model with either \code{lm} or \code{nls}).
#' @param interval In SLR or IVR, a string that indicates whether to plot confidence (\code{="confidence"}) or prediction (\code{="prediction"}) intervals. For a SLR object both can be plotted by using \code{="both"}. In one-way or two-way ANOVA, a logical that indicates whether the confidence intervals should be plotted or not.
#' @param conf.level A decimal numeric that indicates the level of confidence to use for confidence and prediction intervals.
#' @param plot.pts A logical that indicates (\code{TRUE} (default)) whether the points are plotted along with the fitted lines. Set to \code{FALSE} to plot just the fitted lines.
#' @param pch A numeric or vector of numerics that indicates what plotting character codes should be used. In SLR this is the single value to be used for all points. In IVR a vector is used to identify the characters for the levels of the second factor.
#' @param col A vector of color names or the name of a palette (from \code{\link[grDevices]{hcl.pals}}) that indicates what color of points and lines to use for the levels of the first factor in an IVR or the second factor in a two-way ANOVA.
#' @param col.pt A string used to indicate the color of the plotted points. Used only for SLR and logistic regression objects.
#' @param col.mdl A string used to indicate the color of the fitted line. Used only for SLR and logistic regression objects.
#' @param lwd A numeric used to indicate the line width of the fitted line. 
#' @param lty A numeric or vector of numerics used to indicate the type of line used for the fitted line. In SLR this is a single value to be used for the fitted line. In IVR a vector is used to identify the line types for the levels of the second factor. See \code{par}.
#' @param lty.ci a numeric used to indicate the type of line used for the confidence band lines for SLR objects or interval lines for one-way and two-way ANOVA. For IVR, the confidence band types are controlled by \code{lty}.
#' @param lty.pi a numeric used to indicate the type of line used for the prediction band lines for SLR objects. For IVR, the prediction band types are controlled by \code{lty}. See \code{par}.
#' @param xlab a string for labeling the x-axis.
#' @param ylab a string for labeling the y-axis.
#' @param main a string for the main label to the plot. Defaults to the model call.
#' @param legend Controls use and placement of the legend. See details.
#' @param type The type of graphic to construct in a one-way and two-way ANOVA. If \code{"b"} then points are plotted and lines are used to connect points (DEFAULT). If \code{"p"} then only points are used and if \code{"l"} then only lines are drawn.
#' @param ci.fun A function used to put error bars on the one-way or two-way ANOVA graphs. The default is to use the internal \code{iCIfp} function which will place t-distribution based confidence intervals on the graph. The user can provide alternative functions that may plot other types of \sQuote{error bars}. See examples in \code{\link[sciplot]{lineplot.CI}} function of \pkg{sciplot} package.
#' @param col.ci A vector of color names or numbers or the name of a palette (see details) that indicates what colors to use for the confidence interval bars in one-way and two-way ANOVAs.
#' @param which A character string listing the factor in the two-way ANOVA for which the means should be calculated and plotted. This argument is used to indicate for which factor a main effects plot should be constructed. If left missing then an interaction plot is constructed.
#' @param change.order A logical that is used to change the order of the factors in the \code{lm} object. This is used to change which factor is plotted on the x-axis and which is used to connect the means when constructing an interaction plot (ignored if \code{which} is used).
#' @param cex.leg A single numeric values used to represent the character expansion value for the legend. Ignored if \code{legend=FALSE}.
#' @param box.lty.leg A single numeric values used to indicate the type of line to use for the box around the legend. The default is to not plot a box.
#' @param d A data frame that contains the variables used in construction of the \code{nls} object.
#' @param jittered A logical that indicates whether the points should be jittered horizontally.
#' @param legend.lbls A vector of strings that will be the labels for the legend in an nls fitPlot graphic.
#' @param transparency A numeric that indicates how many points would be plotted on top of each other in a logistic regression before the \sQuote{point} would have the full \code{pt.col} color. The reciprocal of this value is the alpha transparency value.
#' @param plot.p A logical that indicates if the proportion for categorized values of X are plotted (\code{TRUE}; default).
#' @param breaks A number that indicates how many intervals over which to compute proportions or a numeric vector that contains the endpoints of the intervals over which to compute proportions if \code{plot.p=TRUE}.
#' @param p.col A color to plot the proportions.
#' @param p.pch A plotting character for plotting the proportions.
#' @param p.cex A character expansion factor for plotting the proportions.
#' @param mdl.vals A numeric that represents the number of values to use for plotting the logistic regression. A larger number means a smoother line.
#' @param xlim A vector of length two to control the x-axis in the logistic regression plot. If this is changed from the default then the domain over which the logistic regression model is plotted will change.
#' @param ylim A vector of length two to control the y-axis in the nonlinear regression plot.
#' @param yaxis1.ticks A numeric vector that indicates where tick marks should be placed on the left y-axis (for the proportion of \sQuote{successes}) for the logistic regression plot.
#' @param yaxis1.lbls A numeric vector that indicates labels for the tick marks on the left y-axis (for the proportion of \sQuote{successes}) for the logistic regression plot.
#' @param yaxis2.show A logical that indicates whether the right y-axis should be created (\code{=TRUE}; default) or not for the logistic regression plot.
#' @param \dots Other arguments to be passed to the plot functions.
#'
#' @return None. However, a fitted-line plot is produced.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @seealso See \code{\link{abline}}, \code{\link[car]{regLine}} in \pkg{car}, \code{\link[psych]{error.bars}} in \pkg{psych}, \code{interaction.plot}, and \code{\link[sciplot]{lineplot.CI}} in \pkg{sciplot} for similar functionality.
#' 
#' @keywords hplot models
#' 
#' @examples
#' # create year as a factor variable
#' Mirex$fyear <- factor(Mirex$year)
#' # reduce number of years for visual simplicity for iVRs
#' Mirex2 <- droplevels(subset(Mirex,fyear %in% c(1977,1992)))
#' 
#' ## One-way ANOVA
#' aov1 <- lm(mirex~fyear,data=Mirex)
#' fitPlot(aov1)
#'
#' ## Two-way ANOVA
#' aov2 <- lm(mirex~fyear*species,data=Mirex)
#' # interaction plots and a color change
#' fitPlot(aov2,legend="bottomleft")
#' fitPlot(aov2,change.order=TRUE)
#' # main effects plots
#' fitPlot(aov2,which="species")
#' fitPlot(aov2,which="fyear")
#'
#' ## Simple linear regression (show color change and confidence/prediction bands)
#' slr1 <- lm(mirex~weight,data=Mirex)
#' fitPlot(slr1)
#' fitPlot(slr1,interval="both")
#'
#' ## Indicator variable regression with one factor (also showing confidence bands)
#' ivr1 <- lm(mirex~weight*fyear,data=Mirex2)
#' fitPlot(ivr1,legend="topleft")
#' fitPlot(ivr1,legend="topleft",interval="confidence")
#' fitPlot(ivr1,legend="topleft",interval="confidence",col="Dark 2")
#' 
#' ## Indicator variable regression with one factor (assuming parallel lines)
#' ivr2 <- lm(mirex~weight+species,data=Mirex2)
#' fitPlot(ivr2,legend="topleft")
#'
#' ## Indicator variable regression with two factors
#' ivr3 <- lm(mirex~weight*fyear*species,data=Mirex2)
#' fitPlot(ivr3,ylim=c(0,0.8),legend="topleft")
#' fitPlot(ivr3,ylim=c(0,0.8),legend="topleft",col="Spectral")
#' 
#' ## Polynomial regression
#' poly1 <- lm(mirex~weight+I(weight^2),data=Mirex)
#' fitPlot(poly1,interval="both")
#'
#' ## Non-linear model example
#' lr.sv <- list(B1=6,B2=7.2,B3=-1.5)
#' nl1 <- nls(cells~B1/(1+exp(B2+B3*days)),start=lr.sv,data=Ecoli)
#' fitPlot(nl1,Ecoli,cex.main=0.7,lwd=2)
#'
#' ## Logistic regression example
#' ## NASA space shuttle o-ring failures -- from graphics package
#' d <- data.frame(fail=factor(c(2,2,2,2,1,1,1,1,1,1,2,1,2,1,1,1,1,2,1,1,1,1,1),
#'                             levels = 1:2, labels = c("no","yes")),
#'                 temperature <- c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,
#'                                  72,73,75,75,76,76,78,79,81))
#' glm1 <- glm(fail~temperature,data=d,family="binomial")
#' fitPlot(glm1)
#' fitPlot(glm1,yaxis1.ticks=c(0,1),yaxis1.lbls=c(0,1))
#'
#' @rdname fitPlot
#' @export
fitPlot <- function (object, ...) {
  if ("lm" %in% class(object)) ## This is a hack so no double deprecation warning
    .Deprecated(msg="'fitPlot' is deprecated and will soon be removed from 'FSA'; see fishR post from 25-May-2021 for alternative methods.")
  UseMethod("fitPlot") 
}

#' @rdname fitPlot
#' @export
fitPlot.lm <- function(object, ...) {
  object <- iTypeoflm(object)
  if (object$Rnum>1)
    STOP("'fitPlot()' does not work with more than 1 LHS variable.")
  if (object$type=="MLR")
    STOP("Multiple linear regression objects are not supported by fitPlot.")
  fitPlot(object,...)
}

#' @rdname fitPlot
#' @export
fitPlot.SLR <- function(object,plot.pts=TRUE,pch=16,col.pt="black",
                        col.mdl="red",lwd=3,lty=1,
                        interval=c("none","confidence","prediction","both"),
                        conf.level=0.95,lty.ci=2,lty.pi=3,
                        xlab=object$Enames[1],ylab=object$Rname,main="",
                        ylim=NULL,...) {
  ## Some tests
  
  ## Check on conf.level
  iCheckConfLevel(conf.level) 
  
  interval <- match.arg(interval)
  if (length(col.pt)>1) {
    WARN("Only first color used for points in this SLR.")
    col.pt <- col.pt[1]
  }
  if (length(col.mdl)>1) {
    WARN("Only first color used for the model in this SLR.")
    col.mdl <- col.mdl[1]
  }
  ## Get data ready
  # extract x and y variables
  y <- object$mf[,object$Rname]
  x <- object$mf[,object$Enames[1]]
  # create predictions to draw the line
  xvals <- seq(min(x),max(x),length.out=200)
  newdf <- data.frame(xvals)
  # sets name of variables so that predict() will work
  names(newdf) <- object$Enames[1]
  # computes predicted values (and CI for use later)
  predC <- stats::predict(object$mdl,newdf,interval="confidence")
  predP <- stats::predict(object$mdl,newdf,interval="prediction")
  ## Put plot together                                             # nocov start
  # Find y-axis range
  if (is.null(ylim)) {
    if (interval %in% c("prediction","both")) ylim <- range(predP)
    else ylim <- range(predC)
  }
  # plot points in white to "disappear" if asked for
  if (!plot.pts) col.pt <- "white"
  graphics::plot(y~x,pch=pch,col=col.pt,ylim=ylim,
                 xlab=xlab,ylab=ylab,main=main,...)
  # plot fitted line over range of data
  graphics::lines(xvals,predC[,"fit"],col=col.mdl,lwd=lwd,lty=lty)
  # puts CI on graph if asked for
  if (interval %in% c("confidence","both")) {
    graphics::lines(xvals,predC[,"upr"],col=col.mdl,lwd=1,lty=lty.ci)
    graphics::lines(xvals,predC[,"lwr"],col=col.mdl,lwd=1,lty=lty.ci)
  }
  # puts PI on graph if asked for
  if (interval %in% c("prediction","both")) {
    graphics::lines(xvals,predP[,"upr"],col=col.mdl,lwd=1,lty=lty.pi)
    graphics::lines(xvals,predP[,"lwr"],col=col.mdl,lwd=1,lty=lty.pi)
  } # nocov end
}


#' @rdname fitPlot
#' @export
fitPlot.IVR <- function(object,...) {
  ## Do some checks
  if (object$ENumNum>1)
    STOP("'fitPlot()' cannot handle >1 covariate in an IVR.")
  if (object$EFactNum>2)
    STOP("'fitPlot()' cannot handle >2 factors in an IVR.")
  ## Decide if a one-way or two-way IVR
  if (object$EFactNum==1) iFitPlotIVR1(object,...)
  else iFitPlotIVR2(object,...)
}

iFitPlotIVR1 <- function(object,plot.pts=TRUE,pch=c(16,21,15,22,17,24,c(3:14)),
                         col="black",lty=rep(1:6,6),lwd=3,
                         interval=c("none","confidence","prediction","both"),
                         conf.level=0.95,
                         xlab=names(object$mf[object$ENumPos]),
                         ylab=object$Rname,main="",
                         legend="topright",cex.leg=1,box.lty.leg=0,...) {
  ## Some checks
  
  ## Check on conf.level
  iCheckConfLevel(conf.level) 
  
  interval <- match.arg(interval)
  # extract y and x quantitative variables
  y <- object$mf[,object$Rname]
  x <- object$mf[,object$ENumPos[1]]
  # extract the factor variable(s) from the 2nd position
  f1 <- object$mf[,object$EFactPos[1]]
  # find number of levels of each factor
  levs.f1 <- unique(f1)
  num.f1 <- length(levs.f1)
  # Handle colors, pchs, ltys -- one for each level of f1 factor unless
  # only one color is given
  col <- iFitPlotClrs2(f1,col)
  pch <- iFitPlotPchs2(f1,pch)
  lty <- iFitPlotLtys2(f1,lty)
  ## Check if groups will be able to be seen
  if (sum(c(length(unique(pch))==1,
            length(unique(lty))==1,
            length(unique(col))==1))>1)
    WARN("Your choices for 'col', 'pch', and 'lty' will make it difficult to see groups.")
  ### Plot the points
  # Creates plot schematic -- no points or lines                   # nocov start
  graphics::plot(y~x,col="white",xlab=xlab,ylab=ylab,main=main,...)
  if (plot.pts) {
    # Plots points w/ different colors & points
    for (i in 1:num.f1) graphics::points(x[f1==levs.f1[i]],
                                         y[f1==levs.f1[i]],
                                         col=col[i],pch=pch[i])
  }
  for (i in 1:num.f1) {
    # Make the predictions at a bunch of values of x
    x.obs <- x[f1==levs.f1[i]]
    y.obs <- y[f1==levs.f1[i]] 
    xvals <- seq(min(x.obs),max(x.obs),length.out=200)
    newdf <- data.frame(xvals,as.factor(rep(levs.f1[i],length(xvals))))
    names(newdf) <- names(object$mf)[c(object$ENumPos,object$EFactPos)]
    predC <- stats::predict(object$mdl,newdf,interval="confidence")
    # Plot just the line if no intervals called for
    graphics::lines(xvals,predC[,"fit"],col=col[i],lwd=lwd,lty=lty[i])
    # add CI if asked for
    if (interval %in% c("confidence","both")) {
      graphics::lines(xvals,predC[,"upr"],col=col[i],lwd=1,lty=lty[i])
      graphics::lines(xvals,predC[,"lwr"],col=col[i],lwd=1,lty=lty[i])     
    }
    # add PI if asked for
    if (interval %in% c("prediction","both")) {
      predP <- stats::predict(object$mdl,newdf,interval="prediction")
      graphics::lines(xvals,predP[,"upr"],col=col[i],lwd=1,lty=lty[i])
      graphics::lines(xvals,predP[,"lwr"],col=col[i],lwd=1,lty=lty[i])
    }        
  } # end for i
  # Prepare list of col,pch,lty for legend
  leg <- iLegendHelp(legend)
  if (leg$do.legend) {
    if (plot.pts) graphics::legend(x=leg$x,y=leg$y,legend=levs.f1,col=col,
                                   pch=pch,lty=lty,cex=cex.leg,box.lty=box.lty.leg)
    else graphics::legend(x=leg$x,y=leg$y,legend=levs.f1,col=col,lty=lty,
                          cex=cex.leg,box.lty=box.lty.leg)
    graphics::box()
  }  # nocov end
}

iFitPlotIVR2 <- function(object,plot.pts=TRUE,pch=c(16,21,15,22,17,24,c(3:14)),
                         col="Dark 2",lty=rep(1:6,6),lwd=3,
                         interval=c("none","confidence","prediction","both"),
                         conf.level=0.95,
                         xlab=names(object$mf[object$ENumPos]),
                         ylab=object$Rname,main="",
                         legend="topright",cex.leg=1,box.lty.leg=0,...) {
  
  ## Check on conf.level
  iCheckConfLevel(conf.level) 
  
  interval <- match.arg(interval)
  # extract y and x quantitative variables
  y <- object$mf[,object$Rname]
  x <- object$mf[,object$ENumPos[1]]
  # extract the factor variable(s)
  f1 <- object$mf[,object$EFactPos[1]]
  f2 <- object$mf[,object$EFactPos[2]]
  # find number of levels of each factor
  levs.f1 <- unique(f1)
  levs.f2 <- unique(f2)
  num.f1 <- length(levs.f1)
  num.f2 <- length(levs.f2)
  # Handle cols, pchs, lty1 -- one for each level of f1 factor unless
  # only one color is given
  col <- iFitPlotClrs2(f1,col)
  pch <- iFitPlotPchs2(f2,pch)
  lty <- iFitPlotLtys2(f2,lty)
  ### Plot the points
  # Creates plot schematic -- no points or lines
  # nocov start
  graphics::plot(y~x,col="white",xlab=xlab,ylab=ylab,main=main,...)
  if (plot.pts) {
    for (i in 1:num.f1) {
      for (j in 1:num.f2) {
        # Plots points w/ different colors & points
        x.obs <- x[f1==levs.f1[i] & f2==levs.f2[j]]
        y.obs <- y[f1==levs.f1[i] & f2==levs.f2[j]] 
        graphics::points(x.obs,y.obs,col=col[i],pch=pch[j])
      }
    }
  }
  for (i in 1:num.f1) {
    for (j in 1:num.f2) {
      # Plots points w/ different colors & points
      x.obs <- x[f1==levs.f1[i] & f2==levs.f2[j]]
      y.obs <- y[f1==levs.f1[i] & f2==levs.f2[j]] 
      # Make the predictions at a bunch of values of x
      xvals <- seq(min(x.obs),max(x.obs),length.out=200)
      newdf <- data.frame(xvals,
                          as.factor(rep(levs.f1[i],length(xvals))),
                          as.factor(rep(levs.f2[j],length(xvals))))
      names(newdf) <- names(object$mf)[c(object$ENumPos,object$EFactPos)]
      pred <- stats::predict(object$mdl,newdf,interval="confidence")
      # Plot just the line if no intervals called for
      graphics::lines(xvals,pred[,"fit"],col=col[i],lwd=lwd,lty=lty[j])
      # add CI if asked for
      if (interval %in% c("confidence","both")) {
        graphics::lines(xvals,pred[,"upr"],col=col[i],lwd=1,lty=lty[j])
        graphics::lines(xvals,pred[,"lwr"],col=col[i],lwd=1,lty=lty[j])     
      }
      # add PI if asked for
      if (interval %in% c("prediction","both")) {
        pred <- stats::predict(object$mdl,newdf,interval="prediction")
        graphics::lines(xvals,pred[,"upr"],col=col[i],lwd=1,lty=lty[j])
        graphics::lines(xvals,pred[,"lwr"],col=col[i],lwd=1,lty=lty[j])
      }        
    } # end for j
  } # end for i
  # Prepare list of col,pch,lty for legend
  leg <- iLegendHelp(legend)
  if (leg$do.legend) {
    lcol <- rep(col,each=num.f2)
    lpch <- rep(pch,times=num.f1)
    llty <- rep(lty,times=num.f1)
    levs <- expand.grid(levs.f1,levs.f2,stringsAsFactors=FALSE,
                        KEEP.OUT.ATTRS=FALSE)
    levs <- paste(levs[,1],levs[,2],sep =":")
    if (plot.pts) graphics::legend(x=leg$x,y=leg$y,legend=levs,
                                   col=lcol,pch=lpch,lty=llty,
                                   cex=cex.leg,box.lty=box.lty.leg)
    else graphics::legend(x=leg$x,y=leg$y,legend=levs,col=lcol,
                          lty=llty,cex=cex.leg,box.lty=box.lty.leg)
    graphics::box()
  }  # nocov end
}

#' @rdname fitPlot
#' @export
fitPlot.POLY <- function(object,...) {
  fitPlot.SLR(object,...)
}


#' @rdname fitPlot
#' @export
fitPlot.ONEWAY <- function (object,
                            xlab=object$Enames[1],ylab=object$Rname,main="",
                            type="b",pch=16,lty=1,col="black",
                            interval=TRUE,conf.level=0.95,ci.fun=iCIfp(conf.level),
                            col.ci=col,lty.ci=1,
                            ...) {
  
  ## Check on conf.level
  iCheckConfLevel(conf.level) 
  
  if (length(col)>1) {
    WARN("Only first color used.")
    col <- col[1]
  }
  if (length(col.ci)>1) {
    WARN("Only first color used for the CIs.")
    col.ci <- col.ci[1]
  }
  # extract x and y variables
  y <- object$mf[,object$Rname]
  f1 <- object$mf[,object$Ename[1]]
  # nocov start
  if (interval) {
    sciplot::lineplot.CI(f1,y,main=main,xlab=xlab,ylab=ylab,
                         type=type,pch=pch,lty=lty,col=col,legend=FALSE,
                         ci.fun=ci.fun,err.col=col.ci,err.lty=lty.ci,...)
  } else stats::interaction.plot(f1,rep(1,length(y)),y,
                                 main=main,xlab=xlab,ylab=ylab,type=type,
                                 pch=pch,lty=lty,col=col,legend=FALSE,...)
} # nocov end


#' @rdname fitPlot
#' @export
fitPlot.TWOWAY <- function(object,which,change.order=FALSE,
                           xlab=object$Enames[ord[1]],ylab=object$Rname,
                           main="",type="b",
                           pch=c(16,21,15,22,17,24,c(3:14)),lty=c(1:6,1:6,1:6),
                           col="Dark 2",
                           interval=TRUE,conf.level=0.95,
                           ci.fun=iCIfp(conf.level),lty.ci=1,
                           legend="topright",cex.leg=1,box.lty.leg=0,
                           ...) {
  
  ## Check on conf.level
  iCheckConfLevel(conf.level) 
  
  # extract y variables
  y <- object$mf[,object$Rname]
  # find the factor variables
  if (missing(which)) {
    # need both factors, check to see if order was changed from model fit
    ifelse(change.order,ord <- c(2,1),ord <- c(1,2))
    x.factor <- object$mf[,object$Enames[ord[1]]]
    group <- object$mf[,object$Enames[ord[2]]]
    ngrps <- length(levels(group))
  } else { # nocov start
    # one of the factors was chosen, pick just that variable
    ord <- match(which,object$Enames)
    x.factor <- object$mf[,object$Enames[ord[1]]]
    # handle "other" factor differently depending on if interval is constructed
    if(interval) group <- NULL
    else group <- rep(1,length(y))
    ngrps <- 1
    if (col=="Dark 2") col <- "black"
  }
  col <- iFitPlotClrs2(group,col)
  pch <- iFitPlotPchs2(group,pch)
  lty <- iFitPlotLtys2(group,lty)
  if (interval) {
    sciplot::lineplot.CI(x.factor,y,group,
                         main=main,xlab=xlab,ylab=ylab,type=type,
                         pch=pch[1:ngrps],lty=lty[1:ngrps],col=col[1:ngrps],
                         legend=FALSE,ci.fun=ci.fun,err.lty=lty.ci,...)
  } else stats::interaction.plot(x.factor,group,y,
                                 main=main,xlab=xlab,ylab=ylab,type=type,
                                 pch=pch[1:ngrps],lty=lty[1:ngrps],
                                 col=col[1:ngrps],legend=FALSE,...) 
  if(ngrps>1) {
    leg <- iLegendHelp(legend)
    graphics::legend(leg$x,leg$y,legend=levels(group),pch=pch[1:ngrps],
                     lty=1:ngrps,col=col,cex=cex.leg,box.lty=box.lty.leg)
  }
  graphics::box() # nocov end
}


#' @rdname fitPlot
#' @export
fitPlot.nls <- function(object,d,
                        pch=c(19,1),col.pt=c("black","red"),col.mdl=col.pt,
                        lwd=2,lty=1,plot.pts=TRUE,jittered=FALSE,ylim=NULL,
                        legend=FALSE,legend.lbls=c("Group 1","Group 2"),
                        ylab=names(mdl$model)[1],xlab=names(mdl$model)[xpos],
                        main="", ...) { # nocov start
  ## add the model option to the NLS object so that data can be extracted
  mdl <- stats::update(object,model=TRUE)
  ## finds number of variables in the model (this is needed because for some
  ##   models (e.g., Francis VBGF) the mdel might contain "other" data)
  numvars <- length(attr(stats::terms(mdl$model),"term.labels"))
  if (missing(d)) { d <- mdl$data }
  else if (!is.data.frame(d)) d <- as.data.frame(d) # make sure is data.frame
  ## find y variable from model
  y <- mdl$model[[1]]
  if (numvars==2) {
    # find position of x-variable and groups
    xpos <- 2
    gpos <- NULL
  } else {
    for (i in 2:numvars) {
      if (!all(mdl$model[[i]]==0 | mdl$model[[i]]==1)) xpos <- i
    }
    gpos <- seq(1,4)[-c(1,xpos)]
    g1 <- mdl$model[[gpos[1]]]
    g2 <- mdl$model[[gpos[2]]]
    if (length(pch)==1) pch <- rep(pch,2)
    if (length(col.pt)==1) col.pt <- rep(col.pt,2)
    if (length(col.mdl)==1) col.mdl <- rep(col.mdl,2)
    if (length(lwd)==1) lwd <- rep(lwd,2)
    if (length(lty)==1) lty <- rep(lty,2)
  }
  # find x variable from model
  x <- mdl$model[[xpos]]
  # create a vector of x values for making predictions -- many to make smooth
  fitx <- data.frame(seq(min(x),max(x),length.out=max(100,length(x))))
  
  if (numvars==2) {
    # change name of x to name of x in model so that predict will work
    names(fitx) <- names(mdl$model)[xpos]
    # data.frame of x values and predicted y values from model
    fits <- data.frame(x=fitx,y=stats::predict(mdl,fitx))
    names(fits) <- c("x","y")
    # find limit for y-axis
    if (is.null(ylim)) ylim <- range(c(y,fits$y))
    if (jittered) x <- jitter(x)    
    if (plot.pts) graphics::plot(x,y,pch=pch[1],col=col.pt[1],ylim=ylim,
                                 xlab=xlab,ylab=ylab,main=main,...)
    else graphics::plot(x,y,type="n",ylim=ylim,xlab=xlab,ylab=ylab,main=main,...)
    graphics::lines(fits$x,fits$y,lwd=lwd[1],lty=lty[1],col=col.mdl[1])
  } else {
    explg1 <- data.frame(x=fitx,g1=rep(1,length(fitx)),g2=rep(0,length(fitx)))
    explg2 <- data.frame(x=fitx,g1=rep(0,length(fitx)),g2=rep(1,length(fitx)))
    names(explg1) <- names(explg2) <- names(mdl$model)[c(xpos,gpos[1],gpos[2])]
    fitsg1 <- data.frame(x=fitx,y=stats::predict(mdl,explg1))
    fitsg2 <- data.frame(x=fitx,y=stats::predict(mdl,explg2))
    names(fitsg1) <- names(fitsg2) <- c("x","y")
    # find limit for y-axis
    if (is.null(ylim)) ylim <- range(c(y,fitsg1$y,fitsg2$y))
    if (jittered) x <- jitter(x)
    graphics::plot(x,y,type="n",ylim=ylim,xlab=xlab,ylab=ylab,main=main,...)
    if (plot.pts) {
      graphics::points(x[g1==1],y[g1==1],pch=pch[1],col=col.pt[1])
      graphics::points(x[g2==1],y[g2==1],pch=pch[2],col=col.pt[2])
    }
    graphics::lines(fitsg1$x,fitsg1$y,lwd=lwd[1],lty=lty[1],col=col.mdl[1])
    graphics::lines(fitsg2$x,fitsg2$y,lwd=lwd[2],lty=lty[2],col=col.mdl[2])
    leg <- iLegendHelp(legend)
    if (leg$do.legend) {
      if (plot.pts) graphics::legend(x=leg$x,y=leg$y,legend=legend.lbls,
                                     col=col.pt,pch=pch,lty=lty)
      else graphics::legend(x=leg$x,y=leg$y,legend=legend.lbls,
                            col=col.mdl,lty=lty)
    }
  }
}  # nocov end

#' @rdname fitPlot
#' @export
fitPlot.glm <- function(object, ...) {
  if (object$family$family=="binomial" & object$family$link=="logit")
    fitPlot.logreg(object,...)
  else
    STOP("Currently only logistic regression GLM models are supported by fitPlot.")
}

#' @rdname fitPlot
#' @export
fitPlot.logreg <- function(object,
                           xlab=names(object$model)[2],ylab=names(object$model)[1],
                           main="",plot.pts=TRUE,col.pt="black",transparency=NULL,
                           plot.p=TRUE,breaks=25,p.col="blue",p.pch=3,p.cex=1,
                           yaxis1.ticks=seq(0,1,0.1),yaxis1.lbls=c(0,0.5,1),
                           yaxis2.show=TRUE,
                           col.mdl="red",lwd=2,lty=1,mdl.vals=50,xlim=range(x),
                           ...) { # nocov start
  ## Get data to plot
  yc <- object$model[,1]
  x <- object$model[,2]
  ## Prepare values to plot the fitted line
  nd <- data.frame(seq(min(xlim),max(xlim),length.out=mdl.vals))
  names(nd) <- names(object$model)[2]
  ## Make the plot
  iPlotBinResp(x,yc,xlab,ylab,plot.pts,col.pt,transparency,
               plot.p,breaks,p.col,p.pch,p.cex,
               yaxis1.ticks=yaxis1.ticks,yaxis1.lbls=yaxis1.lbls,
               yaxis2.show=yaxis2.show,
               main=main,xlim=xlim,...)
  graphics::lines(nd[,1],stats::predict(object,nd,type="response"),
                  col=col.mdl,lwd=lwd,lty=lty)
} # nocov end


################################################################################
### internal functions used in fitPlot
################################################################################
iCIfp1 <- function(x,conf.level) {
  t <- stats::qt((1-conf.level)/2,validn(x)-1)
  c(mean(x)-t*se(x),mean(x)+t*se(x))
}

iCIfp <- function(conf.level) function(x) iCIfp1(x,conf.level)

iFitPlotClrs2 <- function(var,col,defpal) {
  num.grps <- length(unique(var))
  if (num.grps==0) num.grps <- 1   # a hack for which= in two-way ANOVA
  if (length(col)==1) {
    if (col %in% grDevices::hcl.pals())
      col <- grDevices::hcl.colors(num.grps,palette=col)
    else col <- rep(col,num.grps)
  } else if (length(col)<num.grps) {
    WARN("Fewer colors sent (",length(col),
         ") then levels (",num.grps,"; changed to default colors.")
    col <- grDevices::hcl.colors(num.grps,pal="Dark 2")
  } else col <- col[1:num.grps]
  col
}

iFitPlotPchs2 <- function(var,pch) {
  num.grps <- length(unique(var))
  if (length(pch)>1 & num.grps <= length(pch)) pch <- pch[1:num.grps]
  else if (length(pch)==1 & num.grps>1) pch <- rep(pch,num.grps)
  else if (length(pch)<num.grps) {
    WARN("Fewer pchs sent then levels. Changed to default pchs.")
    pch <- c(16,21,15,22,17,24,c(3:14))[1:num.grps]
  }
  pch
}

iFitPlotLtys2 <- function(var,lty) {
  num.grps <- length(unique(var))
  if (length(lty)>1 & num.grps <= length(lty)) lty <- lty[1:num.grps]
  else if (length(lty)==1& num.grps>1) lty <- rep(lty,num.grps)
  else if (length(lty)<num.grps) {
    WARN("Fewer ltys sent then levels. Changed to default ltys.")
    lty <- c(1:6,1:6)[1:num.grps]
  }
  lty
}

iPlotBinResp <- function(x,y,
                         xlab=paste(deparse(substitute(x))),
                         ylab=paste(deparse(substitute(y))),
                         plot.pts=TRUE,col.pt="black",transparency=NULL,
                         plot.p=TRUE,breaks=25,p.col="blue",p.pch=3,p.cex=1.25,
                         yaxis1.ticks=seq(0,1,0.1),yaxis1.lbls=c(0,0.5,1),
                         yaxis2.show=TRUE,...) { # nocov start
  # convert factor to 0s and 1s
  if (is.factor(y)) yn <- as.numeric(y)-1
  else yn <- y
  # will cause points not to be visible
  if (!plot.pts) col.pt <- "white"
  # make transparency value equal to max number of points that overlap
  if (is.null(transparency)) transparency <- max(stats::xtabs(~yn+x))
  # adjust for maximum allowable transparency
  if (transparency>50) transparency <- 50
  # plot raw data points
  graphics::plot(yn~x,pch=16,col=col2rgbt(col.pt,1/transparency),
                 yaxt="n",xlab=xlab,ylab=ylab,...)
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
        x.i <- lencat(x,startcat=min(x),w=(max(x)-min(x))/breaks)
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
