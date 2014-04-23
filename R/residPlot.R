#'Construct a residual plot from lm or nls objects.
#'
#'Constructs a residual plot for \code{lm} or \code{nls} objects.  Different
#'symbols for different groups can be added to the plot if an indicator
#'variable regression is used.
#'
#'If the user chooses to add a legend without identifying coordinates for the
#'upper-left corner of the legend (i.e., \code{legend=TRUE}) then the R console
#'is suspended until the user places the legend by clicking on the produced
#'graphic at the point where the upper-left corner of the legend should appear.
#'A legend will only be placed if the \code{mdl} is an indicator variable
#'regression, even if \code{legend=TRUE}.
#'
#'If \code{outlier.test=TRUE} then significant outliers are detected with
#'\code{outlierTest()} from the \pkg{car} package.  See the help for this
#'function for more details.
#'
#'The \code{residualPlot()} is just a pass-through convenience function.
#'
#'@aliases residPlot residPlot.lm residPlot.SLR residPlot.IVR residPlot.POLY
#'residPlot.ONEWAY residPlot.TWOWAY residPlot.nls residualPlot
#'@param object An \code{lm} or \code{nls} object (i.e., returned from fitting
#'a model with either \code{lm} or \code{nls}).
#'@param student A logical that indicates if the studentized residuals (\code{TRUE}];
#'default) are plotted or not.
#'@param outlier.test A logical that indicates if an \code{outlierTest} will 
#'\code{TRUE} (default) be performed and if the indivdiual with the largest
#'studentized residual is deemed to be a significant outlier it will be noted
#'on the residual plot by its observation number.
#'@param loess A logical that indicates if a loess smoother line is fit to and
#'shown on the residual plot (\code{TRUE}).
#'@param bp A logical that indicates if the plot for the one-way and two-way ANOVA
#'will be a boxplot (\code{TRUE}; default) or not.
#'@param alpha A numeric that indicates the alpha level to use for the outlier test
#'(only used if \code{outlier.test=TRUE}).
#'@param xlab A string for labelling the x-axis.
#'@param ylab A string for labelling the y-axis.
#'@param main A string for the main label to the plot.  Defaults to the model call.
#'@param pch A numeric that indicates the plotting charachter to be used or a
#'vector of numerics that indicates what plotting charachters codes to use for the
#'levels of the second factor.  See \code{par}.
#'@param col A vector of color names that indicates what color of points and lines
#'to use for the levels of the first factor.  See \code{par}.
#'@param lty.ref A numeric that indicates the line type to use for the reference
#'line at residual=0.  See \code{par}.
#'@param lwd.ref A numeric that indicates the line width to use for the reference
#'line at residual=0.  See \code{par}.
#'@param col.ref A numeric or character that indicates the line color to use for
#'the reference line at residual=0.  See \code{par}.
#'@param lty.loess A numeric that indicates the line type to use for loess fit
#'line.  See \code{par}.
#'@param lwd.loess A numeric that indicates the line width to use for loess fit
#'line.  See \code{par}.
#'@param col.loess A numeric or character that indicates the line color to use for
#'loess fit line.  See \code{par}.
#'@param loess.f A numeric for the smoother span. This gives the proportion of
#'points in the plot which influence the smooth at each value.  Larger values
#'give more smoothness.
#'@param legend If \code{TRUE}, draw a legend and the user must click in the
#'upper-left corner of where the legend should be placed; if \code{FALSE} do
#'not draw a legend.  If a vector of length 2 then draw the upper left corner
#'of the legend at the coordinates given in the vector of length 2.
#'@param \dots Other arguments to the generic \code{plot} function.
#'@return None.  However, a residual plot is produced.
#'@note This function is meant to allow newbie students the ability to easily
#'construct residual plots for one-way ANOVA, two-way ANOVA, simple linear
#'regression, and indicator variable regressions.  The plots can be constructed
#'by submitting a saved linear model to this function which allows students to
#'interact with and visualize moderately complex linear models in a fairly easy
#'and efficient manner.
#'@seealso \code{residualPlots} and \code{outlierTest} in \pkg{car}; \code{\link{fitPlot}}.
#'@keywords hplot models
#'@examples
#'data(Mirex)
#'Mirex$year <- factor(Mirex$year)
#'
#'## Indicator variable regression with two factors
#'lm1 <- lm(mirex~weight*year*species,data=Mirex)
#'# defaults
#'residPlot(lm1)
#'# add a loess line to highlight any non-linearities in the residuals
#'residPlot(lm1,loess=TRUE)
#'# modify colors used
#'residPlot(lm1,col="rainbow")
#'# use only one point type -- notice that all points are of same type
#'residPlot(lm1,pch=16)
#'# use only one point and one color (might as well not use legend also)
#'residPlot(lm1,pch=16,col="black",legend=FALSE)
#'# can accomplish same thing just by removing the legend
#'residPlot(lm1,legend=FALSE)
#'# modify the reference line
#'residPlot(lm1,col.ref="blue",lwd.ref=5)
#'
#'## Indicator variable regression with same two factors but in different order
#'##   (notice use of colors and symbols)
#'lm1a <- lm(mirex~weight*species*year,data=Mirex)
#'residPlot(lm1a)
#'
#'## Indicator variable regression with only one factor
#'lm2 <- lm(mirex~weight*year,data=Mirex)
#'residPlot(lm2)
#'
#'## Indicator variable regression (assuming same slope)
#'lm3 <- lm(mirex~weight+year,data=Mirex)
#'residPlot(lm3)
#'
#'## Simple linear regression
#'lm4 <- lm(mirex~weight,data=Mirex)
#'residPlot(lm4)
#'
#'## One-way ANOVA
#'lm5 <- lm(mirex~year,data=Mirex)
#'# default (uses boxplots)
#'residPlot(lm5)
#'# use points rather than boxplot
#'residPlot(lm5,bp=FALSE)
#'
#'## Two-Way ANOVA
#'lm6 <- lm(mirex~species*year,data=Mirex)
#'# default (uses boxplots)
#'residPlot(lm6)
#'# No boxplots
#'residPlot(lm6,bp=FALSE)
#'
#'
#'## Example showing outlier detection
#'x <- c(runif(100))
#'y <- c(7,runif(99))
#'lma <- lm(y~x)
#'residPlot(lma)
#'
#'@rdname residPlot
#'@export residPlot
residPlot <- function (object, ...) {
  UseMethod("residPlot") 
}

#'@rdname residPlot
#'@method residPlot lm
#'@S3method residPlot lm
residPlot.lm <- function(object,...) {   
  object <- typeoflm(object)
  if (object$type=="MLR") stop("Multiple linear regression objects are not supported by residPlot.",call.=FALSE)
  residPlot(object,...)                          
}                         

#'@rdname residPlot
#'@method residPlot SLR
#'@S3method residPlot SLR
residPlot.SLR <- function(object,student=TRUE,outlier.test=TRUE,loess=FALSE,alpha=0.05,
                          xlab="Fitted Values",ylab="Residuals",main=NULL,
                          pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                          lty.loess=2,lwd.loess=2,col.loess="red",loess.f=2/3,...) {
  getMainTitle(object,main)
  fv <- object$mdl$fitted.values                                                # Put fitted.values into fv vector
  ifelse(student, r <- rstudent(object$mdl), r <- object$mdl$residuals)         # Put Studentized or raw residuals into r vector
  if (student & ylab=="Residuals") ylab <- "Studentized Residuals"              # Change y-label axis if Studentized residuals are used
  plot(r~fv,xlab=xlab,ylab=ylab,main=main,pch=pch,col=col,...)
  abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)
  if (loess) addLoessLine(r,fv,loess.f,lwd.loess,lty.loess,col.loess)
  if (outlier.test) addOutlierTestResults(object,fv,r,alpha) 
}

#'@rdname residPlot
#'@method residPlot POLY
#'@S3method residPlot POLY
residPlot.POLY <- function(object,...) {
  residPlot.SLR(object,...)
}

#'@rdname residPlot
#'@method residPlot IVR
#'@S3method residPlot IVR
residPlot.IVR <- function(object,student=TRUE,outlier.test=TRUE,loess=FALSE,alpha=0.05,
                          xlab="Fitted Values",ylab="Residuals",main=NULL,
                          pch=c(16,21,15,22,17,24,c(3:14)),col="rich",lty.ref=3,lwd.ref=1,col.ref="black",
                          lty.loess=2,lwd.loess=2,col.loess="red",loess.f=2/3,legend="topright",...) {
  getMainTitle(object,main)
  fv <- object$mdl$fitted.values                                                # Put fitted.values into fv vector
  ifelse(student, r <- rstudent(object$mdl), r <- object$mdl$residuals)         # Put Studentized or raw residuals into r vector
  if (student & ylab=="Residuals") ylab <- "Studentized Residuals"              # Change y-label axis if Studentized residuals are used
  if (dim(object$mf)[2]>4) stop("Function does not handle models with more than two covariates or more than three factors.",call.=FALSE)
    else {
      leg <- legendHelp(legend)   # will there be a legend
      if (!leg$do.legend) plot(r~fv,xlab=xlab,ylab=ylab,main=main,pch=16,col="black",...)
      else {      
        f1 <- object$mf[,3]
        ifelse(dim(object$mf)[2]==4,f2 <- object$mf[,4],f2 <- as.factor(rep(1,length(r))))
        num.f1 <- length(levels(f1))
        num.f2 <- length(levels(f2))
       ### Handle colors -- one for each level of f1 factor unless only one color is given
        if (length(col)<num.f1) {
          if (length(col)>1) {
            warning(paste("Fewer colors sent then levels of ",names(object$mdl)[3],".  Changed to use rich colors.",sep=""),call.=FALSE)
            col <- chooseColors("rich",num.f1)
          } else if (col %in% paletteChoices()) col <- chooseColors(col,num.f1) # choose colors from given palette type
              else col <- rep(col,num.f1)                                       # If one color given, repeat it so same color used for all levels    
        }
       ### Handle pchs -- one for each level of f2 factor unless only one pch is given
        if (length(pch)>1 & num.f2 <= length(pch)) pch <- pch[1:num.f2]
          else if (length(pch)==1 & num.f2>1) pch <- rep(pch,num.f2)
            else if (length(pch)<num.f2) {
              warning(paste("Fewer point types sent then levels of",names(object$mdl)[4],".  Changed to default point types."),call.=FALSE)
              pch <- c(16,21,15,22,17,24,c(3:14))[1:num.f2]
            }
       ### Plot the points
        ifelse(leg$do.legend,xlim <- c(min(fv),max(fv)+0.3*(max(fv)-min(fv))), xlim <- range(fv))  # Makes room for legend 
        plot(r~fv,col="white",xlab=xlab,ylab=ylab,main=main,xlim=xlim,...)      # Creates plot schematic -- no points or lines
        for (i in 1:num.f1) {
          for (j in 1:num.f2) {
            fv.obs <- fv[unclass(f1)==i & unclass(f2)==j]                       # Plots points w/ different colors & points
            r.obs <- r[unclass(f1)==i & unclass(f2)==j]
            points(fv.obs,r.obs,col=col[i],pch=pch[j])
          }   # end for j
        }     # end for i
       ### Prepare and place the legend
        lcol <- rep(col,each=num.f2)
        lpch <- rep(pch,times=num.f1)
        ifelse(num.f2>1,levs <- levels(f1:f2),levs <- levels(f1))
        if (leg$do.legend) legend(x=leg$x,y=leg$y,legend=levs,col=lcol,pch=lpch)    
      }
    }  
    abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)
    if (loess) addLoessLine(r,fv,loess.f,lwd.loess,lty.loess,col.loess)
    if (outlier.test) addOutlierTestResults(object,fv,r,alpha)
}

#'@rdname residPlot
#'@method residPlot ONEWAY
#'@S3method residPlot ONEWAY
residPlot.ONEWAY <- function(object,student=TRUE,bp=TRUE,outlier.test=TRUE,loess=FALSE,alpha=0.05,
                          xlab="Fitted Values",ylab="Residuals",main=NULL,
                          pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                          lty.loess=2,lwd.loess=2,col.loess="red",loess.f=2/3,...) {
  getMainTitle(object,main)
  if (bp & xlab=="Fitted Values") xlab <- "Treatment Group"
  fv <- object$mdl$fitted.values                                                # Put fitted.values into fv vector
  ifelse(student, r <- rstudent(object$mdl), r <- object$mdl$residuals)         # Put Studentized or raw residuals into r vector
  if (student & ylab=="Residuals") ylab <- "Studentized Residuals"              # Change y-label axis if Studentized residuals are used
  gf <- object$mf[,2]
  if (bp) boxplot(r~gf,xlab=xlab,ylab=ylab,main=main)
    else {
      plot(r~fv,xlab=xlab,ylab=ylab,main=main,pch=pch,col=col,...)
      if (loess) addLoessLine(r,fv,loess.f,lwd.loess,lty.loess,col.loess)
      if (outlier.test) addOutlierTestResults(object,fv,r,alpha)
    }
  abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)
}

#'@rdname residPlot
#'@method residPlot TWOWAY
#'@S3method residPlot TWOWAY
residPlot.TWOWAY <- function(object,student=TRUE,bp=TRUE,outlier.test=TRUE,loess=FALSE,alpha=0.05,
                          xlab="Fitted Values",ylab="Residuals",main=NULL,pch=16,col="black",
                          lty.ref=3,lwd.ref=1,col.ref="black",
                          lty.loess=2,lwd.loess=2,col.loess="red",loess.f=2/3,...) {
  getMainTitle(object,main)
  if (bp & xlab=="Fitted Values") xlab <- "Treatment Group"
  fv <- object$mdl$fitted.values                                                # Put fitted.values into fv vector
  ifelse(student, r <- rstudent(object$mdl), r <- object$mdl$residuals)         # Put Studentized or raw residuals into r vector
  if (student & ylab=="Residuals") ylab <- "Studentized Residuals"              # Change y-label axis if Studentized residuals are used
  gf1 <- object$mf[,2]
  gf2 <- object$mf[,3]
  gf <- interaction(gf1,gf2)
  if (bp) boxplot(r~gf,xlab=xlab,ylab=ylab,main=main)
    else {
      plot(r~fv,xlab=xlab,ylab=ylab,main=main,pch=pch,col=col,...)
      if (loess) addLoessLine(r,fv,loess.f,lwd.loess,lty.loess,col.loess)
      if (outlier.test) addOutlierTestResults(object,fv,r,alpha)
    }
  abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)  
}

#'@rdname residPlot
#'@method residPlot nls
#'@S3method residPlot nls
residPlot.nls<-function(object,loess=FALSE,xlab="Fitted Values",ylab="Residuals",main="",
                          pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                          lty.loess=2,lwd.loess=2,col.loess="red",loess.f=2/3,...) {
  fv <- fitted(object)                                                          # Put fitted.values into fv vector
  r <- residuals(object)
  plot(r~fv,pch=pch,col=col,xlab=xlab,ylab=ylab,main=main,...)
  abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)
  if (loess) lines(stats::lowess(r~fv,f=loess.f,iter=5),lwd=lwd.loess,lty=lty.loess,col=col.loess)
}
