#' @title DEPRECATED -- Construct a residual plot from lm or nls objects.
#'
#' @description Constructs a residual plot for \code{lm} or \code{nls} objects. Different symbols for different groups can be added to the plot if an indicator variable regression is used.
#'
#' @details Three types of residuals are allowed for most model types. Raw residuals are simply the difference between the observed response variable and the predicted/fitted value. Standardized residuals are internally studentized residuals returned by \code{\link{rstandard}} for linear models and are the raw residual divided by the standard deviation of the residuals for nonlinear models (as is done by \code{\link[nlstools]{nlsResiduals}} from \pkg{nlstools}). Studentized residuals are the externally studentized residuals returned by \code{\link{rstudent}} for linear models and are not available for nonlinear models.
#' 
#' Externally Studentized residuals are not supported for \code{nls} or \code{nlme} objects.
#' 
#' If \code{outlier.test=TRUE} then significant outliers are detected with \code{\link[car]{outlierTest}} from the \pkg{car} package. See the help for this function for more details.
#'
#' The user can include the model call as a title to the residual plot by using \code{main="MODEL"}. This only works for models created with \code{lm()}.
#' 
#' If the user chooses to add a legend without identifying coordinates for the upper-left corner of the legend (i.e., \code{legend=TRUE}) then the R console is suspended until the user places the legend by clicking on the produced graphic at the point where the upper-left corner of the legend should appear. A legend will only be placed if the \code{mdl} is an indicator variable regression, even if \code{legend=TRUE}.
#'
#' @note This function is meant to allow newbie students the ability to easily construct residual plots for one-way ANOVA, two-way ANOVA, simple linear regression, and indicator variable regressions. The plots can be constructed by submitting a saved linear model to this function which allows students to interact with and visualize moderately complex linear models in a fairly easy and efficient manner.
#'
#' @aliases residPlot residPlot.lm residPlot.SLR residPlot.IVR residPlot.POLY residPlot.ONEWAY residPlot.TWOWAY residPlot.nls
#'
#' @param object An \code{lm} or \code{nls} object (i.e., returned from fitting a model with either \code{lm} or \code{nls}).
#' @param resid.type  The type of residual to use. \sQuote{Raw} residuals are used by default. See details.
#' @param outlier.test A logical that indicates if an \code{outlierTest} will \code{TRUE} (default) be performed and if the individual with the largest studentized residual is deemed to be a significant outlier it will be noted on the residual plot by its observation number.
#' @param loess A logical that indicates if a loess smoother line and approximate confidence interval band is fit to and shown on the residual plot (\code{TRUE}).
#' @param bp A logical that indicates if the plot for the one-way and two-way ANOVA will be a boxplot (\code{TRUE}; default) or not.
#' @param alpha A numeric that indicates the alpha level to use for the outlier test (only used if \code{outlier.test=TRUE}).
#' @param xlab A string for labeling the x-axis.
#' @param ylab A string for labeling the y-axis.
#' @param main A string for the main label to the plot. See details.
#' @param pch A numeric that indicates the plotting character to be used or a vector of numerics that indicates what plotting character codes to use for the levels of the second factor. See \code{par}.
#' @param col A vector of color names that indicates what color of points and lines to use for the levels of the first factor. See \code{par}.
#' @param lty.ref A numeric that indicates the line type to use for the reference line at residual=0. See \code{par}.
#' @param lwd.ref A numeric that indicates the line width to use for the reference line at residual=0. See \code{par}.
#' @param col.ref A numeric or character that indicates the line color to use for the reference line at residual=0. See \code{par}.
#' @param lty.loess A numeric that indicates the line type to use for loess fit line. See \code{par}.
#' @param lwd.loess A numeric that indicates the line width to use for loess fit line. See \code{par}.
#' @param col.loess A numeric or character that indicates the line color to use for loess fit line. See \code{par}.
#' @param trans.loess A single numeric that indicates how transparent the loess band should be (larger numbers are more transparent).
#' @param legend If \code{TRUE}, draw a legend and the user must click in the upper-left corner of where the legend should be placed; if \code{FALSE} do not draw a legend. If a vector of length 2 then draw the upper left corner of the legend at the coordinates given in the vector of length 2.
#' @param cex.leg A single numeric values used to represent the character expansion value for the legend. Ignored if \code{legend=FALSE}.
#' @param box.lty.leg A single numeric values used to indicate the type of line to use for the box around the legend. The default is to not plot a box.
#' @param inclHist A logical that indicates if a second pane that includes the histogram of residuals should be constructed.
#' @param \dots Other arguments to the generic \code{plot} function.
#'
#' @return None. However, a residual plot is produced.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link[car]{residualPlots}} in \pkg{car} and \code{\link[nlstools]{nlsResiduals}} in \pkg{nlstools}) for similar functionality. See \code{\link[car]{outlierTest}} in \pkg{car} for related methods.
#'
#' @keywords hplot models
#'
#' @examples
#' # create year factor variable
#' Mirex$fyear <- factor(Mirex$year)
#' Mirex$cyear <- as.character(Mirex$year)
#' Mirex$cspecies <- as.character(Mirex$species)
#' 
#' ## One-way ANOVA
#' aov1 <- lm(mirex~fyear,data=Mirex)
#' residPlot(aov1)
#'
#' ## Two-Way ANOVA
#' aov2 <- lm(mirex~species*fyear,data=Mirex)
#' residPlot(aov2)
#' 
#' ## Simple linear regression
#' slr1 <- lm(mirex~weight,data=Mirex)
#' residPlot(slr1)
#' residPlot(slr1,loess=TRUE,main="MODEL")
#' 
#' ## Indicator variable regression with only one factor
#' ivr1 <- lm(mirex~weight*fyear,data=Mirex)
#' residPlot(ivr1)
#' residPlot(ivr1,inclHist=FALSE,pch=19)
#' residPlot(ivr1,inclHist=FALSE,pch=19,col="black")
#' residPlot(ivr1,legend=FALSE,loess=TRUE)
#'
#' ## Indicator variable regression (assuming same slope)
#' ivr2 <- lm(mirex~weight+fyear,data=Mirex)
#' residPlot(ivr2,legend=FALSE,loess=TRUE)
#' 
#' ## Indicator variable regression with two factors
#' ##    Reduce number of years for visual simplicity
#' Mirex2 <- droplevels(subset(Mirex,fyear %in% c(1977,1992)))
#' 
#' ivr3 <- lm(mirex~weight*fyear*species,data=Mirex2)
#' residPlot(ivr3)
#' residPlot(ivr3,loess=TRUE,legend=FALSE)
#'
#' ## IVR w/ factors in different order (notice use of colors and symbols)
#' ivr4 <- lm(mirex~weight*species*fyear,data=Mirex2)
#' residPlot(ivr4)
#'
#'
#' ## Nonlinear regression ... from first example in nls()
#' DNase1 <- subset(DNase,Run==1)
#' fm1DNase1 <- nls(density~SSlogis(log(conc),Asym,xmid,scal),DNase1)
#' residPlot(fm1DNase1)
#' residPlot(fm1DNase1,resid.type="standardized")
#'
#'
#' ## Examples showing outlier detection
#' x <- c(runif(100))
#' y <- c(7,runif(98),-5)
#' lma <- lm(y~x)
#' residPlot(lma)
#' residPlot(lma,resid.type="studentized")
#' 
#' @rdname residPlot
#' @export
residPlot <- function (object,...) {
  if ("lm" %in% class(object)) ## This is a hack so no double deprecation warning
    .Deprecated(msg="'residPlot' is deprecated and will soon be removed from 'FSA'; see fishR post from 1-Jun-2021 for alternative methods.")
  UseMethod("residPlot") 
}

#' @rdname residPlot
#' @export
residPlot.lm <- function(object,...) { # nocov start
  object <- iTypeoflm(object)
  if (object$type=="MLR")
    STOP("Multiple linear regression objects are not supported by residPlot.")
  residPlot(object,...)                          
}  # nocov end

#' @rdname residPlot
#' @export
residPlot.SLR <- function(object,xlab="Fitted Values",ylab="Residuals",main="",
                          pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                          resid.type=c("raw","standardized","studentized"),
                          outlier.test=TRUE,alpha=0.05,
                          loess=FALSE,lty.loess=2,lwd.loess=1,col.loess="black",
                          trans.loess=8,inclHist=TRUE,...) { # nocov start
  main <- iGetMainTitle(object,main)
  fv <- object$mdl$fitted.values
  tmp <- iHndlResidType(object,match.arg(resid.type),ylab)
  r <- tmp$r
  ylab <- tmp$ylab
  if (inclHist) withr::local_par(list(mfrow=c(1,2)))
  iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                     loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
  graphics::points(r~fv,pch=pch,col=col)
  if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha) 
  if (inclHist) iHistResids(r,ylab)
} # nocov end

#' @rdname residPlot
#' @export
residPlot.POLY <- function(object,...) { # nocov start
  residPlot.SLR(object,...)
} # nocov end

#' @rdname residPlot
#' @export
residPlot.IVR <- function(object,legend="topright",cex.leg=1,box.lty.leg=0,...) {
  ## Do some checks
  if (object$ENumNum>1) STOP("'residPlot()' cannot handle >1 covariate in an IVR.")
  if (object$EFactNum>2) STOP("'resodPlot()' cannot handle >2 factors in an IVR.")
  ## Decide if a one-way or two-way IVR
  if (object$EFactNum==1) iResidPlotIVR1(object,legend,cex.leg,box.lty.leg,...)
  else iResidPlotIVR2(object,legend,cex.leg,box.lty.leg,...)
}


iResidPlotIVR1 <- function(object,legend,cex.leg,box.lty.leg,
                           xlab="Fitted Values",ylab="Residuals",main="",
                           pch=c(16,21,15,22,17,24,c(3:14)),col="Dark 2",
                           lty.ref=3,lwd.ref=1,col.ref="black",
                           resid.type=c("raw","standardized","studentized"),
                           outlier.test=TRUE,alpha=0.05,
                           loess=FALSE,lty.loess=2,lwd.loess=1,col.loess="black",
                           trans.loess=8,inclHist=TRUE,...) { # nocov start
  main <- iGetMainTitle(object,main)
  fv <- object$mdl$fitted.values
  tmp <- iHndlResidType(object,match.arg(resid.type),ylab)
  r <- tmp$r
  ylab <- tmp$ylab
  if (inclHist) withr::local_par(list(mfrow=c(1,2)))
  leg <- iLegendHelp(legend)   # will there be a legend
  if (!leg$do.legend) {
    iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                       loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
    graphics::points(r~fv,pch=pch[1],col=ifelse(col=="Dark 2","black",col))
    if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
  } else {      
    # extract the factor variable from the 2nd position
    f1 <- object$mf[,object$EFactPos[1]]
    # Handle colors, pchs, ltys -- one for each level of f1 factor unless only
    #   one color is given
    col <- iFitPlotClrs2(f1,col)
    pch <- iFitPlotPchs2(f1,pch)
    ### Plot the points
    # Makes room for legend
    ifelse(leg$do.legend,xlim <- c(min(fv),max(fv)+0.3*(max(fv)-min(fv))),
                         xlim <- range(fv)) 
    # Creates plot schematic -- no points or lines
    iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                       loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
    # Plots points w/ different colors & points
    levs.f1 <- unique(f1)
    for (i in seq_along(levs.f1)) {
      fv.obs <- fv[f1==levs.f1[i]]
      r.obs <- r[f1==levs.f1[i]]
      graphics::points(fv.obs,r.obs,col=col[i],pch=pch[i])
    }     # end for i
    ## add outlier test if asked for
    if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
    ### Prepare and place the legend
    if (leg$do.legend) {
      graphics::legend(x=leg$x,y=leg$y,legend=levs.f1,col=col,pch=pch,
                       cex=cex.leg,box.lty=box.lty.leg)
      graphics::box()
    }
  }
  if (inclHist) iHistResids(r,ylab)
} # nocov end


iResidPlotIVR2 <- function(object,legend,cex.leg,box.lty.leg,
                           xlab="Fitted Values",ylab="Residuals",main="",
                           pch=c(16,21,15,22,17,24,c(3:14)),col="Dark 2",
                           lty.ref=3,lwd.ref=1,col.ref="black",
                           resid.type=c("raw","standardized","studentized"),
                           outlier.test=TRUE,alpha=0.05,
                           loess=FALSE,lty.loess=2,lwd.loess=1,col.loess="black",
                           trans.loess=8,inclHist=TRUE,...) { # nocov start
  main <- iGetMainTitle(object,main)
  fv <- object$mdl$fitted.values
  tmp <- iHndlResidType(object,match.arg(resid.type),ylab)
  r <- tmp$r
  ylab <- tmp$ylab
  if (inclHist) withr::local_par(list(mfrow=c(1,2)))
  leg <- iLegendHelp(legend)   # will there be a legend
  if (!leg$do.legend) {
    iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                       loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
    graphics::points(r~fv,pch=pch[1],col="black")
    if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
  } else {
    f1 <- object$mf[,object$EFactPos[1]]
    f2 <- object$mf[,object$EFactPos[2]]
    # find number of levels of each factor
    levs.f1 <- unique(f1)
    num.f1 <- length(levs.f1)
    levs.f2 <- unique(f2)
    num.f2 <- length(levs.f2)
    # Handle colors, pchs, ltys -- one for each level of f1 factor unless
    # only one color is given
    col <- iFitPlotClrs2(f1,col)
    pch <- iFitPlotPchs2(f2,pch)
    ### Plot the points
    # Makes room for legend
    ifelse(leg$do.legend,
           xlim <- c(min(fv),max(fv)+0.3*(max(fv)-min(fv))),
           xlim <- range(fv)) 
    # Creates plot schematic -- no points or lines
    iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                       loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
    for (i in seq_along(levs.f1)) {
      for (j in seq_along(levs.f2)) {
        # Plots points w/ different colors & points
        fv.obs <- fv[f1==levs.f1[i] & f2==levs.f2[j]]
        r.obs <- r[f1==levs.f1[i] & f2==levs.f2[j]]
        graphics::points(fv.obs,r.obs,col=col[i],pch=pch[j])
      }   # end for j
    }     # end for i
    ## add outlier test if asked for
    if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
    ### Prepare and place the legend
    if (leg$do.legend) {
      lcol <- rep(col,each=num.f2)
      lpch <- rep(pch,times=num.f1)
      levs <- expand.grid(levs.f1,levs.f2,stringsAsFactors=FALSE,
                          KEEP.OUT.ATTRS=FALSE)
      levs <- paste(levs[,1],levs[,2],sep =":")
      graphics::legend(x=leg$x,y=leg$y,legend=levs,col=lcol,pch=lpch,
                       cex=cex.leg,box.lty=box.lty.leg)
      graphics::box()
    }
  }
  if (inclHist) iHistResids(r,ylab)
} # nocov end

#' @rdname residPlot
#' @export
residPlot.ONEWAY <- function(object,xlab="Fitted Values",ylab="Residuals",main="",
                             pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                             resid.type=c("raw","standardized","studentized"),
                             bp=TRUE,outlier.test=TRUE,alpha=0.05,
                             loess=FALSE,lty.loess=2,lwd.loess=1,col.loess="black",trans.loess=8,
                             inclHist=TRUE,...) { # nocov start
  main <- iGetMainTitle(object,main)
  if (bp & xlab=="Fitted Values") xlab <- "Treatment Group"
  fv <- object$mdl$fitted.values
  tmp <- iHndlResidType(object,match.arg(resid.type),ylab)
  r <- tmp$r
  ylab <- tmp$ylab
  gf <- object$mf[,2]
  if (inclHist) withr::local_par(list(mfrow=c(1,2)))
  if (bp) {
    graphics::boxplot(r~gf,xlab=xlab,ylab=ylab,main=main)
    graphics::abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)
  } else {
      iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                         loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
    graphics::points(r~fv,pch=pch,col=col)
      if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
  } 
  if (inclHist) iHistResids(r,ylab)
} # nocov end

#' @rdname residPlot
#' @export
residPlot.TWOWAY <- function(object,xlab="Fitted Values",ylab="Residuals",main="",
                             pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                             resid.type=c("raw","standardized","studentized"),
                             bp=TRUE,outlier.test=TRUE,alpha=0.05,
                             loess=FALSE,lty.loess=2,lwd.loess=1,col.loess="black",trans.loess=8,
                             inclHist=TRUE,...) { # nocov start
  main <- iGetMainTitle(object,main)
  if (bp & xlab=="Fitted Values") xlab <- "Treatment Group"
  fv <- object$mdl$fitted.values
  tmp <- iHndlResidType(object,match.arg(resid.type),ylab)
  r <- tmp$r
  ylab <- tmp$ylab
  gf1 <- object$mf[,2]
  gf2 <- object$mf[,3]
  gf <- interaction(gf1,gf2)
  if (inclHist) withr::local_par(list(mfrow=c(1,2)))
  if (bp) {
    graphics::boxplot(r~gf,xlab=xlab,ylab=ylab,main=main)
    graphics::abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref) 
  } else {
      iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                         loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
    graphics::points(r~fv,pch=pch,col=col)
      if (outlier.test) iAddOutlierTestResults(object,fv,r,alpha)
  } 
  if (inclHist) iHistResids(r,ylab) 
} # nocov end

#' @rdname residPlot
#' @export
residPlot.nls<-function(object,xlab="Fitted Values",ylab="Residuals",main="",
                        pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                        resid.type=c("raw","standardized","studentized"),
                        loess=FALSE,lty.loess=2,lwd.loess=1,
                        col.loess="black",trans.loess=8,inclHist=TRUE,...) { # nocov start
  fv <- stats::fitted(object)
  tmp <- iHndlResidType(object,match.arg(resid.type),ylab)
  r <- tmp$r
  ylab <- tmp$ylab
  if (inclHist) withr::local_par(list(mfrow=c(1,2)))
  iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                     loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
  graphics::points(r~fv,pch=pch,col=col) 
  if (inclHist) iHistResids(r,ylab)
} # nocov end



#' @rdname residPlot
#' @export
residPlot.nlme<-function(object,xlab="Fitted Values",ylab="Residuals",main="",
                         pch=16,col="black",lty.ref=3,lwd.ref=1,col.ref="black",
                         resid.type=c("raw","standardized","studentized"),
                         loess=FALSE,lty.loess=2,lwd.loess=1,
                         col.loess="black",trans.loess=8,inclHist=TRUE,...) { # nocov start
  fv <- stats::fitted(object)
  tmp <- iHndlResidType(object,match.arg(resid.type),ylab)
  r <- tmp$r
  ylab <- tmp$ylab
  if (inclHist) withr::local_par(list(mfrow=c(1,2)))
  iMakeBaseResidPlot(r,fv,xlab,ylab,main,lty.ref,lwd.ref,col.ref,
                     loess,lty.loess,lwd.loess,col.loess,trans.loess,...)
  graphics::points(r~fv,pch=pch,col=col) 
  if (inclHist) iHistResids(r,ylab)
} # nocov end



##################################################################
### internal functions used in residPlot
##################################################################
iMakeBaseResidPlot <- function(r,fv,xlab,ylab,main,
                               lty.ref,lwd.ref,col.ref,
                               loess,lty.loess,lwd.loess,col.loess,trans.loess,
                               ...) {
  ## makes a base plot that has the axes with appropriate range
  ##  and labels, the horizontal reference line at 0, and, if
  ##  asked for, a loess smoother for the points. The functions
  ##  that call this then just need to add the points.
  xrng <- range(fv)
  yrng <- range(r)
  graphics::plot(r~fv,col="white",xlab=xlab,ylab=ylab,main=main,...)
  if (loess) iAddLoessLine(r,fv,lty.loess,lwd.loess,col.loess,trans.loess)
  graphics::abline(h=0,lty=lty.ref,lwd=lwd.ref,col=col.ref)
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
  if (num>0) { # nocov start
    # Determine which observation(s) is/are "significant" outlier(s)
    obs <- which(names(fv) %in% names(out$bonf.p))
    # Set text position based on sign of r if only one "outlier" is detected
    if (num==1) ifelse(r[obs]<0,pos <- 3,pos <- 1)
    # Use thigmophobe to find better text positions if more "outliers" are detected
    else pos <- plotrix::thigmophobe(fv,r)[obs]
    # place labels
    graphics::text(fv[obs],r[obs],names(fv)[obs],
                   cex=1.1,col="red",pos=pos,xpd=TRUE)
  } # nocov end
}  # end iAddOutlierTestResults internal function

iGetMainTitle <- function(object,main) {
  if (main=="MODEL") {
    ## user asked to use model
    # get formula parts (extra spaces are removed)
    frm.chr <- gsub("\\s+","",as.character(stats::formula(object$mdl)))
    # put together as a main title
    main <- paste0(frm.chr[2],frm.chr[1],frm.chr[3])
  }
  # return the title (will be original if not main="MODEL")
  main
}  # end iGetMainTitle internal function

iHistResids <- function(r,xlab) {
  hist.formula(~r,xlab=xlab)
}

iHndlResidType <- function(object,resid.type,ylab) {
  suppressWarnings(if(!inherits(object,c("nls","nlme"))) {
    switch(resid.type,
           raw= { r <- object$mdl$residuals },
           standardized= { r <- stats::rstandard(object$mdl) },
           studentized= { r <- stats::rstudent(object$mdl) }
           )
  } else if (inherits(object,"nls")) {
    r <- stats::residuals(object)
    if (resid.type=="studentized") STOP("resid.type= cannot be 'studentized' for NLS objects. Try resid.type='standardized'.")
    else if (resid.type=="standardized") {
      # this follows nlsResiduals() from nlstools
      r <- (r-mean(r))/summary(object)$sigma
    }
  } else {
    if (resid.type=="studentized") STOP("resid.type= cannot be 'studentized' for NLME objects. Try resid.type='standardized'.")
    else if (resid.type=="standardized") { 
      r <- stats::residuals(object,type="pearson")
    } else {
      r <- stats::residuals(object,type="response")
    }
  }
  )
  if (resid.type!="raw" & ylab=="Residuals") {
    if (resid.type=="standardized") ylab <- "Standardized Residuals"
    else ylab <- "Studentized Residuals"
  }
  return(list(r=r,ylab=ylab))
}