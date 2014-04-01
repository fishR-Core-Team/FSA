#'Computes the Leslie or Delury population estimate from catch and effort data.
#'
#'Computes the Leslie or Delury estimates of population size and catchability
#'coefficient from paired catch and effort data.  The Ricker modification may
#'also be used.
#'
#'Fits a linear regression model to catch-per-unit-effort on cumulative catch.
#'The catchability coefficient (q) is estimated from the negative of the slope and
#'the initial population size (No)  is estimated by dividing the intercept by the
#'catchability coefficient.
#'
#'Standard errors for the catchability and population size estimates are
#'computed using formulas from Seber (1982).
#'
#'Confidence intervals are computed using standard large-sample normal
#'distribution theory with the regression error df.
#'
#'@aliases depletion plot.depletion summary.depletion coef.depletion
#'anova.depletion confint.depletion
#'@param catch A numeric vector of catches of fish at each time.
#'@param effort A numeric vector of efforts expended at each time.
#'@param type A single string identifying which depletion method to use (in
#'\code{depletion} or a string indicating the type of summary or coefficients
#'to extract.  In the latter case, if \code{type="params"} (the default) then
#'results for No and q are returned.  If \code{type="lm"} then results for the
#'underlying linear model are returned.
#'@param ricker.mod A single logical indicating whether to use the modification
#'proposed by Ricker (=TRUE) or not (=FALSE, default).
#'@param object An object saved from the \code{removal} call (i.e., of class \code{depletion}).
#'@param x An object saved from the \code{depletion} call (i.e., of class \code{depletion}).
#'@param pos.est A single string to identify where to place the estimated population
#'estimate and catchability on the plot.  Can be set to one of \code{"bottomright"},
#'\code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"},
#'\code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"} for
#'positioning the estimated mortality rates on the plot.  Typically
#'\code{"bottomleft"} (DEFAULT) and \code{"topright"} will be
#'\dQuote{out-of-the-way} placements.  Set \code{pos.est} to \code{NULL} to
#'remove the estimated population size and catchability coefficient from the plot.
#'@param ylab A label for the y-axis.
#'@param xlab A label for the x-axis.
#'@param pch A numeric that indicates the type of plotting character.
#'@param col.pt A string that indicates the color of the plotted points.
#'@param col.mdl A string that indicates the color of the fitted line.
#'@param lwd A numeric that indicates the line width of the fitted line.
#'@param lty A numeric that indicates the type of line used for the fitted line.
#'@param parm A specification of which parameters are to be given confidence
#'intervals, either a vector of numbers or a vector of names.  If missing, all
#'parameters are considered.
#'@param conf.level A single number that represents the level of confidence to use for
#'constructing confidence intervals.
#'@param level Same as \code{conf.level} but used for compatability with
#'generic \code{confint} function.
#'@param \dots Additional arguments for methods.
#'@return A list with the following items:
#'\itemize{
#'\item type A string indicating whether the \code{"Leslie"} or \code{"Delury"} model was used.
#'\item catch The original vector of catches.
#'\item effort The orginal vector of efforts.
#'\item cpe A computed vector of catch-per-unit-effort for each time.
#'\item KorE A computed vector of cumulative catch (K; Leslie method) or effort (E; Delury method).
#'\item lm The \code{lm} object from the fit of log(CPE) on K (Leslie method) or E (Delury method).
#'\item est A 2x2 matrix that contains the estimates and standard errors for No and q.
#'}
#'@seealso \code{\link{removal}}, \code{leslieSim} in \pkg{FSATeach}, and
#'\code{deplet} in \pkg{fishmethods}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/Depletion.pdf}
#'@references Ricker, W.E.  1975. Computation and interpretation of biological
#'statistics of fish populations. Technical Report Bulletin 191, Bulletin of
#'the Fisheries Research Board of Canada.
#'
#'Seber, G.A.F. 1982. The Estimation of Animal Abundance. Edward Arnold, second edition.
#'@keywords hplot manip
#'@examples
#'data(SMBassLS)
#'
#'# Leslie model examples
#'l1 <- depletion(SMBassLS$catch,SMBassLS$effort,type="Leslie")
#'plot(l1)
#'confint(l1)
#'summary(l1)
#'summary(l1,type="lm")
#'l2 <- depletion(SMBassLS$catch,SMBassLS$effort,type="Leslie",ricker.mod=TRUE)
#'plot(l2)
#'confint(l2)
#'
#'# Delury model examples
#'d1 <- depletion(SMBassLS$catch,SMBassLS$effort,type="Delury")
#'plot(d1)
#'confint(d1)
#'summary(d1)
#'summary(d1,type="lm")
#'d2 <- depletion(SMBassLS$catch,SMBassLS$effort,type="Delury",ricker.mod=TRUE)
#'plot(d2)
#'confint(d2)
#'
#'@rdname depletion
#'@export depletion
depletion <- function(catch,effort,type=c("Leslie","Delury"),ricker.mod=FALSE) {
  cpe <- catch/effort
  type <- match.arg(type)
  switch(type,
    Leslie= { 
      if (!ricker.mod) K <- cumsum(catch)-catch
        else K <- cumsum(catch)-(catch/2)
      lm1 <- lm(cpe~K)
      s <- summary(lm1)$sigma
      n <- length(catch)
      q <- -coef(lm1)[2]
      q.SE <- summary(lm1)$coef[4]
      N0 <- coef(lm1)[1]/q
      ss.K <- var(K)*(n-1)
      N0.SE <- s/q*sqrt(1/n + ((N0-mean(K))^2)/ss.K)    
    }, # end Leslie
    Delury= { 
      if (!ricker.mod) { E <- cumsum(effort)-effort }
        else {E <- cumsum(effort)-(effort/2)  }
      lm1 <- lm(log(cpe)~E)
      s <- summary(lm1)$sigma
      n <- length(effort)
      q <- -coef(lm1)[2]
      q.SE <- summary(lm1)$coef[4]
      N0 <- exp(coef(lm1)[1])/q
      ss.E <- var(E)*(n-1)
      N0.SE <- s*N0*sqrt(1/n + (((q*mean(E)-1)/q)^2)*(1/ss.E))    
    } # end Delury
  ) # end switch
  mres <- cbind(c(N0,q),c(N0.SE,q.SE))
  rownames(mres) <- c("No","q")
  colnames(mres) <- c("Estimate","Std. Err.")
  if (type=="Leslie") { d <- list(type=type,catch=catch,effort=effort,cpe=cpe,K=K,lm=lm1,est=mres) }
    else { d <- list(type=type,catch=catch,effort=effort,cpe=cpe,E=E,lm=lm1,est=mres) }
  class(d) <- "depletion"
  d  
}

#'@rdname depletion
#'@method plot depletion
#'@S3method plot depletion
plot.depletion <- function(x,pos.est="topright",xlab=NULL,ylab=NULL,
                        pch=19,col.pt="black",col.mdl="red",lwd=2,lty=1,...) {
  old.par <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
  if (is.null(xlab)) {
    if (x$type=="Leslie") { xlab <- "Cumulative Catch" } else { xlab <- "Cumulative Effort" }
  }
  if (is.null(ylab)) {
    if (x$type=="Leslie") { ylab <- "CPE" } else { ylab <- "log(CPE)" }
  }
  if (x$type=="Leslie") plot(x$K,x$cpe,pch=pch,col=col.pt,xlab=xlab,ylab=ylab,...)
    else plot(x$E,log(x$cpe),pch=pch,col=col.pt,xlab=xlab,ylab=ylab,...)
  abline(x$lm,col=col.mdl,lwd=lwd,lty=lty)
  if (!is.null(pos.est)) legend(pos.est,legend=paste("No=",round(x$est["No","Estimate"],0),"\nq=",round(x$est["q","Estimate"],3),sep=""),bty="n")
}

#'@rdname depletion
#'@method coef depletion
#'@S3method coef depletion
coef.depletion <- function(object,type=c("params","lm"),...) {
  type <- match.arg(type)
  if(type=="lm") coef(object$lm,...)
    else t(object$est[,"Estimate"])
}

#'@rdname depletion
#'@method summary depletion
#'@S3method summary depletion
summary.depletion <- function(object,type=c("params","lm"),...) {
  cat("The",object$type,"method was used.\n")
  type <- match.arg(type)
  if(type=="lm") summary(object$lm,...)
    else object$est
}

#'@rdname depletion
#'@method anova depletion
#'@S3method anova depletion
anova.depletion <- function(object,...) {
  anova(object$lm,...)
}

#'@rdname depletion
#'@method confint depletion
#'@S3method confint depletion
confint.depletion <- function(object,parm=c("both","all","q","No","lm"),level=conf.level,conf.level=0.95,...) {
  parm <- match.arg(parm)
  if (parm=="lm") confint(object$lm,level=conf.level)
  else {
    t <- c(-1,1)*qt(1-(1-conf.level)/2,summary(object$lm)$df[2])
    Nores <- rbind(No=object$est["No","Estimate"]+t*object$est["No","Std. Err."])
    qres <- rbind(q=object$est["q","Estimate"]+t*object$est["q","Std. Err."])
    if (parm=="all" | parm=="both") res <- rbind(No=Nores,q=qres)
    else if (parm=="No") res <- Nores
      else res <- qres
    colnames(res) <- ciLabel(conf.level)
    res
  }
}
