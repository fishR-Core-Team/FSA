#'Mortality estimates from the descending limb of a catch curve.
#'
#'Fits a linear model to the user-defined descending limb of a catch curve.  A
#'plot method highlights the descending-limb, shows the linear model on the 
#'descending limb, and, optionally, prints the estimated instantaneous (Z) and 
#'annual (A) mortality rates.
#'
#'The default is to use all ages in the age vector.  This is appropriate only
#'if the age and catch vectors contain only the ages and catches on the
#'descending limb of the catch curve.
#'
#'If \code{use.weights=TRUE} then a weighted regression is used where the
#'weights are the log(number) at each age predicted from the unweighted
#'regression of log(number) on age (as proposed by Maceina and Bettoli (1998)).
#'
#'@aliases catchCurve catchCurve.default catchCurve.formula plot.catchCurve summary.catchCurve
#'coef.catchCurve anova.catchCurve confint.catchCurve
#'@param x A numerical vector of the assigned ages in the catch curve or a
#'formula of the form \code{catch~age} when used in \code{catchCurve}.  An object
#'saved from \code{catchCurve} (i.e., of class \code{catchCurve}) when used in
#'the methods.
#'@param object An object saved from the \code{catchCurve} call (i.e., of class \code{catchCurve}).
#'@param catch A numerical vector of the catches or CPUEs for the ages in the catch
#'curve.  Not used if \code{x} is a formula.
#'@param data A data frame from which the variables in the \code{x} formula can
#'be found.  Not used if \code{x} is not a formula.
#'@param ages2use A numerical vector of the ages that define the descending
#'limb of the catch curve.
#'@param use.weights A logical that indicates whether a weighted regression should
#'be used.  See details.
#'@param pos.est A string to identify where to place the estimated mortality
#'rates on the plot.  Can be set to one of \code{"bottomright"},
#'\code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"},
#'\code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"} for
#'positioning the estimated mortality rates on the plot.  Typically
#'\code{"bottomleft"} (DEFAULT) and \code{"topright"} will be
#'\dQuote{out-of-the-way} placements.  Set \code{pos.est} to \code{NULL} to
#'remove the estimated mortality rates from the plot.
#'@param ylab A label for the y-axis (\code{"log(Catch)"} is the default).
#'@param xlab A label for the x-axis (\code{"Age"} is the default).
#'@param col.pt A string that indicates the color of the plotted points.
#'@param col.mdl A string that indicates the color of the fitted line.
#'@param lwd A numeric that indicates the line width of the fitted line.
#'@param lty A numeric that indicates the type of line used for the fitted line.
#'@param type A string that indicates what type of summary should be
#'returned.  If \code{type="lm"} then summaries of the underlying linear model
#'are returned.  If \code{type="params"} then summaries of the Z and A
#'parameters are returned.
#'@param parm A numeric or string (of parameter names) vector that specifies which
#'parameters are to be given confidence intervals  If missing, all parameters are
#'considered.
#'@param conf.level A number representing the level of confidence to use for
#'constructing confidence intervals.
#'@param level Same as \code{conf.level}.  Used for compatability with the
#'generic \code{confint} function.
#'@param \dots Additional arguments for methods.
#'@return A list that contains the following items:
#'\itemize{
#'\item age The original vector of assigned ages.
#'\item catch The original vector of observed catches or CPUEs.
#'\item age.e A vector of assigned ages for which the catch curve was fit.
#'\item log.catch.e A vector of log catches or CPUEs for which the catch
#'curve was fit.
#'\item W A vector of weights used in the catch curve fit.  Will be \code{NULL}
#'unless \code{use.weights=TRUE}.
#'\item lm An \code{lm} object from the fit to the ages and log catches
#'or CPUEs on the descending limb (i.e., in age.e and log.catch.e).
#'}
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{chapmanRobson}}, \code{\link{catchCurveSim}},
#'and \code{agesurv} and \code{agesurvcl} in \pkg{fishmethods}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/CatchCurve.pdf}
#'@references Maceina, M.J., and P.W. Bettoli.  1998.  Variation in largemouth
#'bass recruitment in four mainstream impoundments on the Tennessee River.
#'North American Journal of Fisheries Management 18:998-1003.
#'@keywords hplot htest manip
#'@examples
#'data(BrookTroutTH)
#'cc <- with(BrookTroutTH,catchCurve(age,catch,2:6))
#'par(mfrow=c(2,1))
#'plot(cc)
#'plot(cc,pos.est="topright")
#'summary(cc)
#'coef(cc)
#'confint(cc)
#'
#'## demonstration of formula notation
#'cc2 <- catchCurve(catch~age,data=BrookTroutTH,ages2use=2:6)
#'summary(cc2)
#'
#'## demonstration of using weights
#'cc3 <- catchCurve(catch~age,data=BrookTroutTH,ages2use=2:6,use.weights=TRUE)
#'summary(cc3)
#'
#'## demonstration of receiving the linear model results
#'summary(cc2,type="lm")
#'coef(cc2,type="lm")
#'confint(cc2,type="lm")
#'
#'@rdname catchCurve
#'@export catchCurve
catchCurve <- function (x,...) {
  UseMethod("catchCurve") 
}

#'@rdname catchCurve
#'@method catchCurve formula
#'@S3method catchCurve formula
catchCurve.formula <- function(x,data,ages2use=age,use.weights=FALSE,...) {
  mf <- model.frame(x,data=data)
  age <- mf[,2]
  catch <- mf[,1]
  catchCurve.default(age,catch,ages2use=ages2use,use.weights=use.weights,...)
}

#'@rdname catchCurve
#'@method catchCurve default
#'@S3method catchCurve default
catchCurve.default <- function(x,catch,ages2use=age,use.weights=FALSE,...) {
  age <- x
  log.catch <- log(catch)                                      # Log of all catches
  rows2use <- match(ages2use,age)                              # Find rows to use according to ages to use
  age.e <- age[rows2use]; log.catch.e <- log.catch[rows2use]   # Ages and log catches of descending limb
  cclm <- lm(log.catch.e~age.e)                                # Fit the model to descending limb
  if (use.weights) {                                           # Fit weighted regression is asked for
    W <- predict(cclm)                                         # Find the weights from the unweighted regression
    cclm <- lm(log.catch.e~age.e,weights=W)
  } else {
    W <- NULL
  }
  cc <- list(age=age,catch=catch,age.e=age.e,log.catch.e=log.catch.e,weights.e=W,lm=cclm)
  class(cc) <- "catchCurve"
  cc
}

#'@rdname catchCurve
#'@method plot catchCurve
#'@S3method plot catchCurve
plot.catchCurve <- function(x,pos.est="bottomleft",ylab="log(Catch)",xlab="Age",
                    col.pt="black",col.mdl="red",lwd=2,lty=1,...) {
  old.par <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
  yrng <- c(min(0,min(log(x$catch),na.rm=TRUE)),max(log(x$catch),na.rm=TRUE))
  plot(log(x$catch)~x$age,col=col.pt,xlab=xlab,ylab=ylab,ylim=yrng,...)         # Plot raw data
  points(x$age.e,x$log.catch.e,col=col.pt,pch=19,cex=1.25)                      # Highlight descending limb portion
  lines(x$age.e,predict(x$lm,data.frame(x$age.e)),lwd=lwd,lty=lty,col=col.mdl)  # Put model on descending limb
  if (!is.null(pos.est)) {                                                      # Put mortality val on graph
    Z <- -coef(x$lm)[2]                                                         #   Find mortality values
    A <- 100*(1-exp(-Z))
    legend(pos.est,legend=paste("Z=",round(Z,3),"\nA=",round(A,1),"%",sep=""),bty="n")
  }
}

#'@rdname catchCurve
#'@method summary catchCurve
#'@S3method summary catchCurve
summary.catchCurve <- function(object,type=c("params","lm"),...) {
  type <- match.arg(type)
  if (type=="lm") summary(object$lm,...)
    else {
      Z <- summary(object$lm)$coef[2,]
      Z[c(1,3)] <- -Z[c(1,3)]
      A <- c(100*(1-exp(-Z[1])),NA,NA,NA)
      rbind(Z,A)
    }
}

#'@rdname catchCurve
#'@method coef catchCurve
#'@S3method coef catchCurve
coef.catchCurve <- function(object,type=c("params","lm"),...) {
  type <- match.arg(type)
  if (type=="lm") coef(object$lm,...)
    else {
      Z <- -coef(object$lm)[2]
      A <- 100*(1-exp(-Z))
      d <- cbind(Z,A)
      rownames(d) <- ""
      d
    }
}

#'@rdname catchCurve
#'@method anova catchCurve
#'@S3method anova catchCurve
anova.catchCurve <- function(object,...) {
  anova(object$lm,...)
}

#'@rdname catchCurve
#'@method confint catchCurve
#'@S3method confint catchCurve
confint.catchCurve <- function(object,parm=c("all","both","Z","A"),level=conf.level,conf.level=0.95,type=c("params","lm"),...) {
  type <- match.arg(type)
  parm <- match.arg(parm)
  ci <- confint(object$lm,conf.level=level,...)
  if (type=="lm") res <- ci
  else {
    Zres <- rbind(Z=-ci[2,2:1])
    Ares <- rbind(A=100*(1-exp(ci[2,2:1])))
    if (parm=="all" | parm=="both") res <- rbind(Zres,Ares)
      else if (parm=="Z") res <- Zres
        else res <- Zres
    colnames(res) <- ciLabel(conf.level)
  }
  res
}
