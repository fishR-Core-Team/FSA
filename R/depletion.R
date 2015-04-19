#' @title Computes the Leslie or Delury population estimate from catch and effort data.
#'
#' @description Computes the Leslie or Delury estimates of population size and catchability coefficient from paired catch and effort data.  The Ricker modification may also be used.
#'
#' @details For the Leslie method, a linear regression model of catch-per-unit-effort on cumulative catch prior to the sample is fit.  The catchability coefficient (q) is estimated from the negative of the slope and the initial population size (No) is estimated by dividing the intercept by the catchability coefficient.  If \code{Ricker.mod=TRUE} then the cumulative catch is modified to be the cumulative catch prior to the sample plus half of the catch of the current sample.
#' 
#' For the DeLury method, a linear regression model of log (catch-per-unit-effort) on cumulative effort is fit.  The catchability coefficient (q) is estimated from the negative of the slope and the initial population size (No) is estimated by dividing the intercept as an exponent of e by the catchability coefficient.  If \code{Ricker.mod=TRUE} then the cumulative effort is modified to be the cumulative effort prior to the sample plus half of the effort of the current sample.
#'
#' Standard errors for the catchability and population size estimates are computed fronm formulas on page 298 (for Leslie) and 303 (for DeLury) from Seber (2002).  Confidence intervals are computed using standard large-sample normal distribution theory with the regression error df.
#' 
#' @param catch A numeric vector of catches of fish at each time.
#' @param effort A numeric vector of efforts expended at each time.
#' @param method A single string that indicates which depletion method to use
#' @param type A string that indicates the type of summary or coefficients to extract.  If \code{type="params"} (the default) then results for No and q are returned.  If \code{type="lm"} then results for the underlying linear model are returned.
#' @param Ricker.mod A single logical that indicates whether to use the modification proposed by Ricker (=TRUE) or not (=FALSE, default).
#' @param object An object saved from the \code{removal} call (i.e., of class \code{depletion}).
#' @param x An object saved from the \code{depletion} call (i.e., of class \code{depletion}).
#' @param verbose A logical that indicates whether a reminder of the method used should be printed with the summary results.
#' @param parm A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names.  If missing, all parameters are considered.
#' @param conf.level A single number that represents the level of confidence to use for constructing confidence intervals.
#' @param level Same as \code{conf.level} but used for compatability with generic \code{confint} function.
#' @param digits A single numeric indicating the number of digits to round the output.
#' @param ylab A label for the y-axis.
#' @param xlab A label for the x-axis.
#' @param pch A numeric that indicates the type of plotting character.
#' @param col.pt A string that indicates the color of the plotted points.
#' @param col.mdl A string that indicates the color of the fitted line.
#' @param lwd A numeric that indicates the line width of the fitted line.
#' @param lty A numeric that indicates the type of line used for the fitted line.
#' @param pos.est A single string to identify where to place the estimated population estimate and catchability on the plot.  Can be set to one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"} for positioning the estimated mortality rates on the plot.  Typically \code{"bottomleft"} (DEFAULT) and \code{"topright"} will be \dQuote{out-of-the-way} placements.  Set \code{pos.est} to \code{NULL} to remove the estimated population size and catchability coefficient from the plot.
#' @param cex.est A single numeric that identifies the character expansion value for the estimated population estimate and catchability placed on the plot.
#' @param \dots Additional arguments for methods.
#'
#' @return A list with the following items:
#'  \itemize{
#'    \item method A string that indicates whether the \code{"Leslie"} or \code{"Delury"} model was used.
#'    \item catch The original vector of catches.
#'    \item effort The orginal vector of efforts.
#'    \item cpe A computed vector of catch-per-unit-effort for each time.
#'    \item KorE A computed vector of cumulative catch (K; Leslie method) or effort (E; Delury method).
#'    \item lm The \code{lm} object from the fit of CPE on K (Leslie method) or log(CPE) on E (Delury method).
#'    \item est A 2x2 matrix that contains the estimates and standard errors for No and q.
#'  }
#'
#' @section testing:  The Leslie method without the Ricker modification and the DeLury method with the Ricker modification matches the results from \code{\link[fishmethods]{deplet}} in \pkg{fishmethods} for the \code{\link[fishmethods]{darter}} (from \pkg{fishmethods}), \code{\link[FSAdata]{LobsterPEI}} and \code{\link[FSAdata]{BlueCrab}} from \pkg{FSAdata}, and \code{\link{SMBassLS}} for N0 to whole numbers, the SE for No to one decimal, q to seven decimals, and the SE of q to at least five decimals.
#' 
#' The Leslie method matches the results of Seber (2002) for N0, q, and the CI for Q but not the CI for N (which was so far off that it might be that Seber's result is incorrect) for the lobster data and the q and CI for q but the NO or its CI (likely due to lots of rounding in Seber 2002) for the Blue Crab data.
#' 
#' The Leslie and DeLury methods match the results of Ricker (1975) for No and Q but not for the CI of No (Ricker used a very different method to compute CIs).
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{7-Abundance}.
#'
#' @seealso See \code{\link{removal}} for related functionality and \code{\link[fishmethods]{deplet}} in \pkg{fishmethods} for similar functionality.
#'
#' @references Ogle, D.H.  2016.  Introductory Fisheries Analyses with R.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Ricker, W.E. 1975. \href{http://www.dfo-mpo.gc.ca/Library/1485.pdf}{Computation and interpretation of biological statistics of fish populations}. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.
#'
#' Seber, G.A.F. 2002. The Estimation of Animal Abundance. Edward Arnold, Second edition (reprinted).
#'
#' @keywords hplot manip
#'
#' @aliases depletion plot.depletion summary.depletion coef.depletion anova.depletion confint.depletion
#'
#' @examples
#' data(SMBassLS)
#'
#' ## Leslie model examples
#' # no Ricker modification
#' l1 <- depletion(SMBassLS$catch,SMBassLS$effort,method="Leslie")
#' coef(l1)
#' summary(l1)
#' summary(l1,verbose=TRUE)
#' confint(l1)
#' summary(l1,type="lm")
#' plot(l1)
#' 
#' # with Ricker modification
#' l2 <- depletion(SMBassLS$catch,SMBassLS$effort,method="Leslie",Ricker.mod=TRUE)
#' summary(l2)
#' confint(l2)
#' plot(l2)
#'
#' ## Delury model examples
#' # no Ricker modification
#' d1 <- depletion(SMBassLS$catch,SMBassLS$effort,method="Delury")
#' coef(d1)
#' summary(d1)
#' summary(d1,verbose=TRUE)
#' confint(d1)
#' summary(d1,type="lm")
#' plot(d1)
#' 
#' # with Ricker modification
#' d2 <- depletion(SMBassLS$catch,SMBassLS$effort,method="Delury",Ricker.mod=TRUE)
#' summary(d2)
#' confint(d2)
#' plot(d2)
#'
#' @rdname depletion
#' @export
depletion <- function(catch,effort,method=c("Leslie","DeLury","Delury"),Ricker.mod=FALSE) {
  # check method, change bad spelling of DeLury if necessary
  method <- match.arg(method)
  if (method=="Delury") method <- "DeLury"
  # some initial checks
  iCheckCatchEffort(catch,effort)
  # goto internal function depending on method
  switch(method,
    Leslie= { d <- iLeslie(catch,effort,Ricker.mod) },
    DeLury= { d <- iDeLury(catch,effort,Ricker.mod) }
  )
  class(d) <- "depletion"
  d  
}

##############################################################
# INTERNAL function to check Catch and Effort vectors
##############################################################
iCheckCatchEffort <- function(catch,effort) {
  if (!is.numeric(catch)) stop("'catch' must be a numeric vector.",call.=FALSE)
  if (!is.vector(catch)) stop("'catch' must be a vector.",call.=FALSE)
  if (any(catch<0)) stop("All 'catch' must be non-negative.",call.=FALSE)
  if (!is.numeric(effort)) stop("'effort' must be a numeric vector.",call.=FALSE)
  if (!is.vector(effort)) stop("'effort' must be a vector.",call.=FALSE)
  if (any(effort<=0)) stop("All 'effort' must be positive.",call.=FALSE)
  if (length(catch)!=length(effort)) stop("'catch' and 'effort' must be same length.",call.=FALSE)
  if (length(catch)<3) stop("Must have at least 3 values in 'catch'.",call.=FALSE)
}

##############################################################
# INTERNAL function for compute the Leslie estimates
##############################################################
iLeslie <- function(catch,effort,Ricker.mod) {
  cpe <- catch/effort
  ifelse(!Ricker.mod,K <- cumsum(catch)-catch,K <- cumsum(catch)-(catch/2))
  n <- length(catch)
  # main regression
  lm1 <- lm(cpe~K)
  tmp <- summary(lm1)
  # extract results for calculations
  s <- tmp$sigma
  q <- -tmp$coef[2,"Estimate"]
  q.SE <- tmp$coef[2,"Std. Error"]
  N0 <- tmp$coef[1,"Estimate"]/q
  # This is Seber (2002) variance equation on page 298 where
  #    (Seber's symbol listed first) sigma=s, K=q, s=n, N=N0, and x=K
  ss.K <- var(K)*(n-1)
  N0.SE <- s/q*sqrt(1/n + ((N0-mean(K))^2)/ss.K)    
  # Create matrix of parameter estimates
  mres <- cbind(c(N0,q),c(N0.SE,q.SE))
  rownames(mres) <- c("No","q")
  colnames(mres) <- c("Estimate","Std. Err.")
  # check significance of slopes before leaving
  iCheckRegSig(lm1)
  # put together a return list
  list(method="Leslie",catch=catch,effort=effort,cpe=cpe,K=K,lm=lm1,est=mres)
}

##############################################################
# INTERNAL function for compute the DeLury estimates
##############################################################
iDeLury <- function(catch,effort,Ricker.mod) {
  if (any(catch==0)) stop("Can't have zero catches with 'DeLury' method.",call.=FALSE)
  cpe <- catch/effort
  ifelse(!Ricker.mod,E <- cumsum(effort)-effort,E <- cumsum(effort)-(effort/2))
  n <- length(effort)
  # main regression
  lm1 <- lm(log(cpe)~E)
  tmp <- summary(lm1)
  # extract results for caculations
  s <- tmp$sigma
  q <- -tmp$coef[2,"Estimate"]
  q.SE <- tmp$coef[2,"Std. Error"]
  N0 <- exp(tmp$coef[1,"Estimate"])/q
  # This is Seber (2002) variance equation on page 303 where
  #    (Seber's symbol listed first) sigma=s, k=q, s=n, N=N0, and x=E
  ss.E <- var(E)*(n-1)
  N0.SE <- s*N0*sqrt(1/n + (((q*mean(E)-1)/q)^2)*(1/ss.E))  
  # Create matrix of parameter estimates
  mres <- cbind(c(N0,q),c(N0.SE,q.SE))
  rownames(mres) <- c("No","q")
  colnames(mres) <- c("Estimate","Std. Err.")
  # check significance of slopes before leaving
  iCheckRegSig(lm1)
  # put together a return list
  list(method="DeLury",catch=catch,effort=effort,cpe=cpe,E=E,lm=lm1,est=mres)
}

##############################################################
# INTERNAL function for compute the Leslie estimates
##############################################################
iCheckRegSig <- function(tmp) {
  tmp.slope <- coef(tmp)[2]
  if (tmp.slope>0) warning("Estimates are suspect as model did not exhibit a negative slope.",call.=FALSE)
  tmp.slope.p <- anova(tmp)[1,"Pr(>F)"]
  if (tmp.slope.p>0.05 & tmp.slope<0) warning("Estimates are suspect as model did not exhibit a significantly (p>0.05) negative slope.", call.=FALSE)
}

#' @rdname depletion
#' @export
summary.depletion <- function(object,type=c("params","lm"),verbose=FALSE,
                              digits=getOption("digits"),...) {
  if (verbose) cat("The",object$method,"method was used.\n")
  type <- match.arg(type)
  if(type=="lm") summary(object$lm,...)
    else round(object$est,digits)
}

#' @rdname depletion
#' @export
coef.depletion <- function(object,type=c("params","lm"),
                           digits=getOption("digits"),...) {
  type <- match.arg(type)
  if(type=="lm") coef(object$lm,...)
    else t(round(object$est[,"Estimate"],digits))
}

#' @rdname depletion
#' @export
confint.depletion <- function(object,parm=c("No","q","lm"),
                              level=conf.level,conf.level=0.95,
                              digits=getOption("digits"),...) {
  parm <- match.arg(parm,several.ok=TRUE)
  ## only print lm confidence intervals if that is the only parm chosen
  if (length(parm)==1 & "lm" %in% parm) confint(object$lm,level=conf.level)
  else {
    # remove "lm" if in parm with q or No
    parm <- parm[-which(parm=="lm")]
    t <- c(-1,1)*qt(1-(1-conf.level)/2,summary(object$lm)$df[2])
    tmp <- summary(object,parm="params")
    t <- matrix(rep(t,nrow(tmp)),nrow=nrow(tmp),byrow=TRUE)
    res <- tmp[,"Estimate"]+t*tmp[,"Std. Err."]
    rownames(res) <- parm
    colnames(res) <- iCILabel(conf.level)
    round(res,digits)
  }
}

#' @rdname depletion
#' @export
anova.depletion <- function(object,...) {
  anova(object$lm,...)
}

#' @rdname depletion
#' @export
plot.depletion <- function(x,xlab=NULL,ylab=NULL,
                           pch=19,col.pt="black",
                           col.mdl="gray70",lwd=1,lty=1,
                           pos.est="topright",cex.est=0.95,...) {
  # make base plot
  if (x$method=="Leslie") {
    if (is.null(xlab)) xlab <- "Cumulative Catch"
    if (is.null(ylab)) ylab <- "CPE"
    plot(x$K,x$cpe,pch=pch,col=col.pt,xlab=xlab,ylab=ylab,...)
  } else {
    if (is.null(xlab)) xlab <- "Cumulative Effort"
    if (is.null(ylab)) ylab <- "log(CPE)"
    plot(x$E,log(x$cpe),pch=pch,col=col.pt,xlab=xlab,ylab=ylab,...)
  }
  # add best-fit line
  abline(x$lm,col=col.mdl,lwd=lwd,lty=lty)
  # add values to plot
  if (!is.null(pos.est)) {
    legend(pos.est,legend=paste("No=",round(x$est["No","Estimate"],0),
                                "\nq=",round(x$est["q","Estimate"],4),sep=""),
           cex=cex.est,bty="n")
  }
}
