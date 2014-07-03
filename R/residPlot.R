#' @title Construct a residual plot from lm or nls objects.
#'
#' @description Constructs a residual plot for \code{lm} or \code{nls} objects.  Different symbols for different groups can be added to the plot if an indicator variable regression is used.
#'
#' @details If the user chooses to add a legend without identifying coordinates for the upper-left corner of the legend (i.e., \code{legend=TRUE}) then the R console is suspended until the user places the legend by clicking on the produced graphic at the point where the upper-left corner of the legend should appear.  A legend will only be placed if the \code{mdl} is an indicator variable regression, even if \code{legend=TRUE}.
#'
#' If \code{outlier.test=TRUE} then significant outliers are detected with \code{outlierTest()} from the \pkg{car} package.  See the help for this function for more details.
#'
#' @note This function is meant to allow newbie students the ability to easily construct residual plots for one-way ANOVA, two-way ANOVA, simple linear regression, and indicator variable regressions.  The plots can be constructed by submitting a saved linear model to this function which allows students to interact with and visualize moderately complex linear models in a fairly easy and efficient manner.
#'
#' @aliases residPlot residPlot.lm residPlot.SLR residPlot.IVR residPlot.POLY residPlot.ONEWAY residPlot.TWOWAY residPlot.nls residualPlot
#'
#' @param object An \code{lm} or \code{nls} object (i.e., returned from fitting a model with either \code{lm} or \code{nls}).
#' @param student A logical that indicates if the studentized residuals (\code{TRUE}]; default) are plotted or not.
#' @param outlier.test A logical that indicates if an \code{outlierTest} will \code{TRUE} (default) be performed and if the indivdiual with the largest studentized residual is deemed to be a significant outlier it will be noted on the residual plot by its observation number.
#' @param loess A logical that indicates if a loess smoother line and approximate confidence interval band is fit to and shown on the residual plot (\code{TRUE}).
#' @param bp A logical that indicates if the plot for the one-way and two-way ANOVA will be a boxplot (\code{TRUE}; default) or not.
#' @param alpha A numeric that indicates the alpha level to use for the outlier test (only used if \code{outlier.test=TRUE}).
#' @param xlab A string for labelling the x-axis.
#' @param ylab A string for labelling the y-axis.
#' @param main A string for the main label to the plot.  Defaults to the model call.
#' @param pch A numeric that indicates the plotting charachter to be used or a vector of numerics that indicates what plotting charachters codes to use for the levels of the second factor.  See \code{par}.
#' @param col A vector of color names that indicates what color of points and lines to use for the levels of the first factor.  See \code{par}.
#' @param lty.ref A numeric that indicates the line type to use for the reference line at residual=0.  See \code{par}.
#' @param lwd.ref A numeric that indicates the line width to use for the reference line at residual=0.  See \code{par}.
#' @param col.ref A numeric or character that indicates the line color to use for the reference line at residual=0.  See \code{par}.
#' @param lty.loess A numeric that indicates the line type to use for loess fit line.  See \code{par}.
#' @param lwd.loess A numeric that indicates the line width to use for loess fit line.  See \code{par}.
#' @param col.loess A numeric or character that indicates the line color to use for loess fit line.  See \code{par}.
#' @param trans.loess A single numeric that indicates how transparent the loess band should be (larger numbers are more transparent).
#' @param legend If \code{TRUE}, draw a legend and the user must click in the upper-left corner of where the legend should be placed; if \code{FALSE} do not draw a legend.  If a vector of length 2 then draw the upper left corner of the legend at the coordinates given in the vector of length 2.
#' @param inclHist A logical that indicates if a second pane that includes the histogram of residuals should be constructed.
#' @param \dots Other arguments to the generic \code{plot} function.
#'
#' @return None.  However, a residual plot is produced.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{residualPlots} in \pkg{car} for similar functionality and \code{\link{fitPlot}} and \code{outlierTest} in \pkg{car} for related methods.
#'
#' @keywords hplot models
#'
#' @examples
#' data(Mirex)
#' Mirex$year <- factor(Mirex$year)
#'
#' ## Indicator variable regression with two factors
#' lm1 <- lm(mirex~weight*year*species,data=Mirex)
#' # defaults
#' residPlot(lm1)
#' # remove the histogram
#' residPlot(lm1,inclHist=FALSE)
#' # remove the loess line
#' residPlot(lm1,loess=FALSE,inclHist=FALSE)
#' # modify colors used
#' residPlot(lm1,col="rainbow",inclHist=FALSE)
#' # use only one point type -- notice that all points are of same type
#' residPlot(lm1,pch=16,inclHist=FALSE)
#' # use only one point and one color (might as well not use legend also)
#' residPlot(lm1,pch=16,col="black",legend=FALSE,inclHist=FALSE)
#' # can accomplish same thing just by removing the legend
#' residPlot(lm1,legend=FALSE,inclHist=FALSE)
#' # modify the reference line
#' residPlot(lm1,col.ref="blue",lwd.ref=5,inclHist=FALSE)
#' # use Studentized residuals
#' residPlot(lm1,student=TRUE)
#'
#' ## Indicator variable regression with same two factors but in different order
#' ##   (notice use of colors and symbols)
#' lm1a <- lm(mirex~weight*species*year,data=Mirex)
#' residPlot(lm1a)
#'
#' ## Indicator variable regression with only one factor
#' lm2 <- lm(mirex~weight*year,data=Mirex)
#' residPlot(lm2)
#'
#' ## Indicator variable regression (assuming same slope)
#' lm3 <- lm(mirex~weight+year,data=Mirex)
#' residPlot(lm3)
#'
#' ## Simple linear regression
#' lm4 <- lm(mirex~weight,data=Mirex)
#' residPlot(lm4)
#'
#' ## One-way ANOVA
#' lm5 <- lm(mirex~year,data=Mirex)
#' # default (uses boxplots)
#' residPlot(lm5)
#' # use points rather than boxplot
#' residPlot(lm5,bp=FALSE)
#'
#' ## Two-Way ANOVA
#' lm6 <- lm(mirex~species*year,data=Mirex)
#' # default (uses boxplots)
#' residPlot(lm6)
#' # No boxplots
#' residPlot(lm6,bp=FALSE)
#'
#' ## Example showing outlier detection
#' x <- c(runif(100))
#' y <- c(7,runif(99))
#' lma <- lm(y~x)
#' residPlot(lma)
#' # with studentized residuals
#' residPlot(lm1,student=TRUE)
#'
#' @rdname residPlot
#' @export
residPlot <- function (object,...) {
  UseMethod("residPlot") 
}

#' @rdname residPlot
#' @export
residPlot.lm <- function(object,...) {   
  object <- iTypeoflm(object)
  if (object$type=="MLR") stop("Multiple linear regression objects are not supported by residPlot.",call.=FALSE)
  residPlot(object,...)                          
}                         

#' @rdname residPlot
#' @export
residPlot.SLR <- function(object,xlab="Fitted Values",ylab="Residuals",main=NULL,
                          pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                          student=FALSE,outlier.test=TRUE,alpha=0.05,
                          loess=TRUE,lty.loess=2,lwd.loess=1,col.loess="black",trans.loess=4,
                          inclHist=TRUE,...) {
  iGetMainTitle(object,main)
  fv <- object$mdl$fitted.values
  ifelse(student, r <- rstudent(object$mdl), r <- object$mdl$residuals)
  if (student & ylab=="Residuals") ylab <- "Studentized Residuals"
  if (inclHist) par(mfrow=c(1,2))
  iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                     loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
  points(r~fv,pch=pch,col=col)
  if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha) 
  if (inclHist) iHistResids(r,ylab)
}

#' @rdname residPlot
#' @export
residPlot.POLY <- function(object,...) {
  residPlot.SLR(object,...)
}

#' @rdname residPlot
#' @export
residPlot.IVR <- function(object,xlab="Fitted Values",ylab="Residuals",main=NULL,
                          pch=c(16,21,15,22,17,24,c(3:14)),col="rich",lty.ref=3,lwd.ref=1,col.ref="black",
                          student=FALSE,outlier.test=TRUE,alpha=0.05,
                          loess=TRUE,lty.loess=2,lwd.loess=1,col.loess="black",trans.loess=4,
                          legend="topright",inclHist=TRUE,...) {
  iGetMainTitle(object,main)
  fv <- object$mdl$fitted.values
  ifelse(student, r <- rstudent(object$mdl), r <- object$mdl$residuals)
  if (student & ylab=="Residuals") ylab <- "Studentized Residuals"
  if (dim(object$mf)[2]>4) stop("Function does not handle models with more than two covariates or more than three factors.",call.=FALSE)
    else {
      if (inclHist) par(mfrow=c(1,2))
      leg <- iLegendHelp(legend)   # will there be a legend
      if (!leg$do.legend) {
        iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                           loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
        points(r~fv,pch=pch,col="black")
        if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
      } else {      
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
       # Makes room for legend
       ifelse(leg$do.legend,xlim <- c(min(fv),max(fv)+0.3*(max(fv)-min(fv))), xlim <- range(fv)) 
       # Creates plot schematic -- no points or lines
       iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                          loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
       for (i in 1:num.f1) {
         for (j in 1:num.f2) {
           # Plots points w/ different colors & points
           fv.obs <- fv[unclass(f1)==i & unclass(f2)==j]
           r.obs <- r[unclass(f1)==i & unclass(f2)==j]
           points(fv.obs,r.obs,col=col[i],pch=pch[j])
         }   # end for j
       }     # end for i
       ## add outlier test if asked for
       if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
       ### Prepare and place the legend
       lcol <- rep(col,each=num.f2)
       lpch <- rep(pch,times=num.f1)
       ifelse(num.f2>1,levs <- levels(f1:f2),levs <- levels(f1))
       if (leg$do.legend) legend(x=leg$x,y=leg$y,legend=levs,col=lcol,pch=lpch)    
     } # end for no legend 
     if (inclHist) iHistResids(r,ylab)
   }
}

#' @rdname residPlot
#' @export
residPlot.ONEWAY <- function(object,xlab="Fitted Values",ylab="Residuals",main=NULL,
                             pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                             student=FALSE,bp=TRUE,outlier.test=TRUE,alpha=0.05,
                             loess=TRUE,lty.loess=2,lwd.loess=1,col.loess="black",trans.loess=4,
                             inclHist=TRUE,...) {
  iGetMainTitle(object,main)
  if (bp & xlab=="Fitted Values") xlab <- "Treatment Group"
  fv <- object$mdl$fitted.values
  ifelse(student, r <- rstudent(object$mdl), r <- object$mdl$residuals)
  if (student & ylab=="Residuals") ylab <- "Studentized Residuals"
  gf <- object$mf[,2]
  if (inclHist) par(mfrow=c(1,2))
  if (bp) {
    boxplot(r~gf,xlab=xlab,ylab=ylab,main=main)
    abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)
  } else {
      iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                         loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
      points(r~fv,pch=pch,col=col)
      if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
  } 
  if (inclHist) iHistResids(r,ylab)
}

#' @rdname residPlot
#' @export
residPlot.TWOWAY <- function(object,xlab="Fitted Values",ylab="Residuals",main=NULL,
                             pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                             student=FALSE,bp=TRUE,outlier.test=TRUE,alpha=0.05,
                             loess=TRUE,lty.loess=2,lwd.loess=1,col.loess="black",trans.loess=4,
                             inclHist=TRUE,...) {
  iGetMainTitle(object,main)
  if (bp & xlab=="Fitted Values") xlab <- "Treatment Group"
  fv <- object$mdl$fitted.values
  ifelse(student, r <- rstudent(object$mdl), r <- object$mdl$residuals)
  if (student & ylab=="Residuals") ylab <- "Studentized Residuals"
  gf1 <- object$mf[,2]
  gf2 <- object$mf[,3]
  gf <- interaction(gf1,gf2)
  if (inclHist) par(mfrow=c(1,2))
  if (bp) {
    boxplot(r~gf,xlab=xlab,ylab=ylab,main=main)
    abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref) 
  } else {
      iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                         loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
      points(r~fv,pch=pch,col=col)
      if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
  } 
  if (inclHist) iHistResids(r,ylab) 
}

#' @rdname residPlot
#' @export
residPlot.nls<-function(object,xlab="Fitted Values",ylab="Residuals",main="",
                        pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                        loess=TRUE,lty.loess=2,lwd.loess=1,col.loess="black",trans.loess=4,
                        inclHist=TRUE,...) {
  fv <- fitted(object)
  r <- residuals(object)
  if (inclHist) par(mfrow=c(1,2))
  iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                     loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
  points(r~fv,pch=pch,col=col) 
  if (inclHist) iHistResids(r,ylab)
}


##################################################################
### internal functions used in residPlot
##################################################################
iMakeBaseResidPlot <- function(r,fv,xlab,ylab,main,
                               lty.ref,lwd.ref,col.ref,
                               loess,lty.loess,lwd.loess,col.loess,trans.loess,
                               ...) {
  ## makes a base plot that has the axes with appropriate range
  ##  and labels, the horizontal reference line at 0, and, if
  ##  asked for, a loess smoother for the points.  The functions
  ##  that call this then just need to add the points.
  xrng <- range(fv)
  yrng <- range(r)
  plot(r~fv,col="white",xlab=xlab,ylab=ylab,main=main,...)
  if (loess) iAddLoessLine(r,fv,lty.loess,lwd.loess,col.loess,trans.loess)
  abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)
}

iAddOutlierTestResults <- function(object,fv,r,alpha) {
  # get results
  out <- car::outlierTest(object$mdl,cutoff=alpha)
  # number of points returned by outlierTest
  num <- length(out$bonf.p)
  # If only one point returned then ...
  if (num==1) {
    if (is.na(out$bonf.p)) num <- 0      # if it is NA then p>1 ... so not a significant point
    else if (out$bonf.p>alpha) num <- 0  # if p>alpha then ... not a significant point
  }
  # If there are significant points to be highlighted then ...
  if (num>0) {
    # Determine which observation(s) is/are "significant" outlier(s)
    obs <- as.numeric(names(out$bonf.p))
    # Set text position based on sign of r if only one "outlier" is detected
    if (num==1) ifelse(r[obs]<0,pos <- 3,pos <- 1)
      # Use thigmophobe to find better text positions if more "outliers" are detected
      else pos <- thigmophobe(fv,r)[obs]
    # place labels
    text(fv[obs],r[obs],obs,cex=1.1,col="red",pos=pos,xpd=TRUE)
  }
}  # end iAddOutlierTestResults internal function

iAddLoessLine <- function(r,fv,lty.loess,lwd.loess,col.loess,trans.loess) {
  mdl <- loess(r~fv)
  xrng <- range(fv)
  xseq <- seq(from=xrng[1],to=xrng[2],length=80)
  pred <- predict(mdl,newdata=data.frame(fv=xseq),se=TRUE)
  y <- pred$fit
  ci <- pred$se.fit*qt(0.95/2+.5,pred$df)
  ymin <- y-ci
  ymax <- y+ci
  polygon(c(xseq,rev(xseq)),c(ymin,rev(ymax)),col=iMakeColor(col.loess,trans.loess),border=NA)
  lines(y~xseq,lwd=lwd.loess,lty=lty.loess,col=col.loess)
}  # end iAddLoessLine internal function


iGetMainTitle <- function(object,main) {
  # if no main title was sent to function
  if (is.null(main)) {
    # get formula parts
    frm.chr <- as.character(formula(object$mdl))
    # put together as a main title
    main <- paste(frm.chr[2],frm.chr[1],frm.chr[3])
  }
  # return the title (NULL if NULL was sent)
  main
}  # end iGetMainTitle internal function

iHistResids <- function(r,xlab) {
  hist(~r,xlab=xlab)
}