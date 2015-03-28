#' @title Mortality estimates from the descending limb of a catch curve.
#'
#' @description Fits a linear model to the user-defined descending limb of a catch curve.  Method functions extract estimates of the instantaneous (Z) and total annual (A) mortality rates with associated standard errors and confidence intervals.  A plot method highlights the descending-limb, shows the linear model on the descending limb, and, optionally, prints the estimated Z and A.
#'
#' @details The default is to use all ages in the age vector.  This is appropriate only when the age and catch vectors contain only the ages and catches on the descending limb of the catch curve.  Use \code{ages2use} to isolate only the catch and ages on the descending limb.
#'
#' If \code{weighted=TRUE} then a weighted regression is used where the weights are the log(number) at each age predicted from the unweighted regression of log(number) on age (as proposed by Maceina and Bettoli (1998)).
#'
#' @aliases catchCurve catchCurve.default catchCurve.formula plot.catchCurve summary.catchCurve
#'coef.catchCurve anova.catchCurve confint.catchCurve
#'
#' @param x A numerical vector of the assigned ages in the catch curve or a formula of the form \code{catch~age} when used in \code{catchCurve}.  An object saved from \code{catchCurve} (i.e., of class \code{catchCurve}) when used in the methods.
#' @param object An object saved from the \code{catchCurve} call (i.e., of class \code{catchCurve}).
#' @param catch A numerical vector of the catches or CPUEs for the ages in the catch curve.  Not used if \code{x} is a formula.
#' @param data A data frame from which the variables in the \code{x} formula can be found.  Not used if \code{x} is not a formula.
#' @param ages2use A numerical vector of the ages that define the descending limb of the catch curve.
#' @param weighted A logical that indicates whether a weighted regression should be used.  See details.
#' @param pos.est A string to identify where to place the estimated mortality rates on the plot.  Can be set to one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"} for positioning the estimated mortality rates on the plot.  Typically \code{"bottomleft"} (DEFAULT) and \code{"topright"} will be \dQuote{out-of-the-way} placements.  Set \code{pos.est} to \code{NULL} to remove the estimated mortality rates from the plot.
#' @param cex.est A single numeric characther expansion value for the estimated mortaliry rates on the plot.
#' @param ylab A label for the y-axis (\code{"log(Catch)"} is the default).
#' @param xlab A label for the x-axis (\code{"Age"} is the default).
#' @param col.pt A string that indicates the color of the plotted points.
#' @param col.mdl A string that indicates the color of the fitted line.
#' @param lwd A numeric that indicates the line width of the fitted line.
#' @param lty A numeric that indicates the type of line used for the fitted line.
#' @param type A string that indicates what type of summary should be returned.  If \code{type="lm"} then summaries of the underlying linear model are returned.  If \code{type="params"} then summaries of the Z and A parameters are returned.
#' @param parm A numeric or string (of parameter names) vector that specifies which parameters are to be given confidence intervals  If missing, all parameters are considered.
#' @param conf.level A number representing the level of confidence to use for constructing confidence intervals.
#' @param level Same as \code{conf.level}.  Used for compatability with the generic \code{confint} function.
#' @param \dots Additional arguments for methods.
#'
#' @return A list that contains the following items:
#'  \itemize{
#'    \item age The original vector of assigned ages.
#'    \item catch The original vector of observed catches or CPUEs.
#'    \item age.e A vector of assigned ages for which the catch curve was fit.
#'    \item log.catch.e A vector of log catches or CPUEs for which the catch curve was fit.
#'    \item W A vector of weights used in the catch curve fit.  Will be \code{NULL} unless \code{weighted=TRUE}.
#'    \item lm An \code{lm} object from the fit to the ages and log catches or CPUEs on the descending limb (i.e., in age.e and log.catch.e).
#'  }
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @seealso See \code{agesurv} in \pkg{fishmethods} for similar functionality.  See \code{\link{chapmanRobson}} and \code{agesurvcl} in \pkg{fishmethods} for alternative methods to estimate mortality rates.  See \code{\link{metaM}} for empirical methods to estimate natural mortality.
#' 
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/CatchCurve.pdf}
#' 
#' @section Testing: Tested the results of catch curve, both unweighted and weighted, against the results in Miranda and Bettoli (2007).  Results for Z and the SE of Z matched perfectly.  Tested the unweighted results against the results from \code{agesurv} in \pkg{fishmethods} using the \code{rockbass} data.frame in \pkg{fishmethods}.  Results for Z and the SE of Z matched perfectly.
#' 
#' @references Maceina, M.J., and P.W. Bettoli.  1998.  Variation in largemouth bass recruitment in four mainstream impoundments on the Tennessee River. North American Journal of Fisheries Management 18:998-1003.
#' 
#' Ricker, W.E. 1975. \href{http://www.dfo-mpo.gc.ca/Library/1485.pdf}{Computation and interpretation of biological statistics of fish populations}. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.
#' 
#' @keywords hplot htest manip
#' 
#' @examples
#' data(BrookTroutTH)
#' plot(catch~age,data=BrookTroutTH,pch=19)
#' 
#' ## demonstration of formula notation
#' cc1 <- catchCurve(catch~age,data=BrookTroutTH,ages2use=2:6)
#' summary(cc1)
#' plot(cc1)
#' coef(cc1)
#' confint(cc1)
#' 
#' ## demonstration of excluding ages2use
#' cc2 <- catchCurve(catch~age,data=BrookTroutTH,ages2use=-c(0,1))
#' summary(cc2)
#' plot(cc2)
#' 
#' ## demonstration of using weights
#' cc3 <- catchCurve(catch~age,data=BrookTroutTH,ages2use=2:6,weighted=TRUE)
#' summary(cc3)
#' plot(cc3)
#'
#' ## demonstration of returning the linear model results
#' summary(cc3,type="lm")
#' coef(cc3,type="lm")
#' confint(cc3,type="lm")
#' 
#' ## demonstration of ability to work with missing age classes
#' df <- data.frame(age=c(  2, 3, 4, 5, 7, 9,12),
#'                  ct= c(100,92,83,71,56,35, 1))
#' cc4 <- catchCurve(ct~age,data=df,ages2use=4:12)
#' summary(cc4)
#' plot(cc4)
#' 
#' ## demonstration of ability to work with missing age classes
#' ## evein if catches are recorded as NAs
#' df <- data.frame(age=c(  2, 3, 4, 5, 6, 7, 8, 9,10,11,12),
#'                  ct= c(100,92,83,71,NA,56,NA,35,NA,NA, 1))
#' cc5 <- catchCurve(ct~age,data=df,ages2use=4:12)
#' summary(cc5)
#' plot(cc5)
#'
#' @rdname catchCurve
#' @export
catchCurve <- function (x,...) {
  UseMethod("catchCurve") 
}

#' @rdname catchCurve
#' @export
catchCurve.default <- function(x,catch,ages2use=age,weighted=FALSE,...) {
  ## Put x into age variable for rest of function
  age <- x
  
  ## Some Checks
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  if (!is.numeric(catch)) stop("'catch' must be numeric.",call.=FALSE)
  if (length(age)!=length(catch)) stop("'age' and 'catch' have different lenghts.",call.=FALSE)
  # Check to make sure enough ages and catches exist
  if (length(age)<2) stop("Fewer than 2 data points.",call.=FALSE)

  ## Isolate the ages and catches to be used  
  # Find rows to use according to ages to use
  rows2use <- iCheck_ages2use(ages2use,age)
  # Create new vectors with just the data to use
  age.e <- age[rows2use]
  catch.e <- catch[rows2use]
  # Check to make sure enough ages and catches exist
  if (length(age.e)<2) stop("Fewer than 2 data points after applying 'ages2use'.",call.=FALSE)
  
  ## Fit the model to descending limb
  log.catch.e <- log(catch.e)
  cclm <- lm(log.catch.e~age.e)
  if (weighted) {
    # if asked to fit weighted regression then find weights as
    #   the predicted values from the raw regression
    W <- predict(cclm)
    # if any weights are zero or negative then replace with the
    # minimum of positive weights.  Send a warning.
    if (any(W<=0)) {
      warning("Some weights were non-positive and were changed to minimum of positive weights.",call.=TRUE)
      W[W<=0] <- min(W[W>0])
    } 
    # and then fit the weighted regression
    cclm <- lm(log.catch.e~age.e,weights=W)
  } else {
    # if not asked to fit weighted regression then fill weights
    #   with NULL for return in the list below.
    W <- NULL
  }
  ## Prepare the list of results to return
  cc <- list(age=age,catch=catch,age.e=age.e,log.catch.e=log.catch.e,weights.e=W,lm=cclm)
  class(cc) <- "catchCurve"
  cc
}

#' @rdname catchCurve
#' @export
catchCurve.formula <- function(x,data,ages2use=age,weighted=FALSE,...) {
  ## Handle the formula and perform some checks
  tmp <- iHndlFormula(x,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) stop("'catchCurve' must have only one LHS variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'catchCurve' must have only one RHS variable.",call.=FALSE)
  if (!tmp$Eclass %in% c("numeric","integer")) stop("RHS variable must be numeric.",call.=FALSE)
  ## Get variables from model frame
  age <- tmp$mf[,tmp$Enames]
  catch <- tmp$mf[,tmp$Rname]
  ## Call the default function
  catchCurve.default(age,catch,ages2use=ages2use,weighted=weighted,...)
}

#' @rdname catchCurve
#' @export
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

#' @rdname catchCurve
#' @export
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

#' @rdname catchCurve
#' @export
anova.catchCurve <- function(object,...) {
  anova(object$lm,...)
}

#' @rdname catchCurve
#' @export
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
    colnames(res) <- iCILabel(conf.level)
  }
  res
}

#' @rdname catchCurve
#' @export
plot.catchCurve <- function(x,pos.est="topright",cex.est=0.95,
                            ylab="log(Catch)",xlab="Age",
                            col.pt="gray30",col.mdl="black",lwd=2,lty=1,...) {
  # Find the range of the y-axis
  yrng <- c(min(0,min(log(x$catch),na.rm=TRUE)),max(log(x$catch),na.rm=TRUE))
  # Plot raw data
  plot(log(x$catch)~x$age,col=col.pt,xlab=xlab,ylab=ylab,ylim=yrng,...)
  # Highlight descending limb portion
  points(x$age.e,x$log.catch.e,col=col.pt,pch=19)
  # Put model on descending limb
  lines(x$age.e,predict(x$lm,data.frame(x$age.e)),lwd=lwd,lty=lty,col=col.mdl)
  # Put mortality values on the plot
  if (!is.null(pos.est)) {
    Z <- -coef(x$lm)[2]
    A <- 100*(1-exp(-Z))
    legend(pos.est,legend=paste("Z=",round(Z,3),"\nA=",round(A,1),"%",sep=""),bty="n",cex=cex.est)
  }
}


##############################################################
# INTERNAL FUNCTIONS
##############################################################
#=============================================================
# A check on appropriateness for ages2 use.  Will handle if 
# negative ages2use are supplied.  Will return in row2use the
# rows in ages that were asked for in ages2use.
#
# Also called by chapmanRobson.
#=============================================================
iCheck_ages2use <- function(ages2use,ages) {
  ## Can't have both positive and negative ages
  if (any(ages2use<0) & any(ages2use>0)) stop("'ages2use' must be all positive or negative.",call.=FALSE)
  ## If all negative then those are ages not to use
  ##   create a vector of ages to use
  if (all(ages2use<=0)) {
    agesnot2use <- -1*ages2use
    rowsnot2use <- match(agesnot2use,ages)
    ages2use <- ages[-rowsnot2use]
  }
  ## Find rows to use based on matching ages2use and ages
  rows2use <- match(ages2use,ages)
  ## Send a warning if the user asked for ages that don't exist
  if (!all(ages[rows2use] %in% ages)) {
    warning("Some 'ages2use' not in observed ages.",call.=FALSE)
    rows2use <- rows2use[!is.na(rows2use)]
  }
  ## return the ROWS (not the ages) to use
  rows2use
}