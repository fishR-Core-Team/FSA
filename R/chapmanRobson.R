#' @title Computes Chapman-Robson estimates of S and Z.
#'
#' @description Computes the Chapman-Robson estimates of annual survival rate (S) and instantaneous mortality rate (Z), along with associated standard errors, from catch-at-age data on the descending limb of a catch-curve.
#'
#' @details The default is to use all ages in the age vector.  This is only appropriate if the age and catch vector contain only the ages and catches on the descending limb of the catch curve.  Use \code{ages2use} to isolate only the catch and ages on the descending limb.
#'
#' @details The Chapman-Robson method provides an estimate of the annual survival rate, with the annual mortality rate (A) determined by 1-S.  The instantaneous mortality rate is often computed as -log(S).  However, Hoenig et al. (1983) showed that this produced a biased (over)estimate of Z and provided a correction.  The correction is applied by setting \code{zmethod="Hoenigetal"} (which is the default behavior).
#'
#' @aliases chapmanRobson chapmanRobson.default chapmanRobson.formula plot.chapmanRobson summary.chapmanRobson confint.chapmanRobson
#' 
#' @param x A numerical vector of the assigned ages in the catch curve or a formula of the form \code{catch~age} when used in \code{chapmanRobson}.  An object saved from \code{chapmanRobson} (i.e., of class \code{chapmanRobson}) when used in the methods.
#' @param object An object saved from the \code{chapmanRobson} call (i.e., of class \code{chapmanRobson}).
#' @param catch A numerical vector of the catches or CPUEs for the ages in the catch curve.  Not used if \code{x} is a formula.
#' @param data A data frame from which the variables in the \code{x} formula can be found.  Not used if \code{x} is not a formula.
#' @param ages2use A numerical vector of the ages that define the descending limb of the catch curve.
#' @param zmethod  A string that indicates the method to use for estimating Z.  See details.
#' @param pos.est A string to identify where to place the estimated mortality rates on the graph.  Can be set to one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"} for positioning the estimated mortality rates on the plot.  Typically \code{"bottomleft"} (DEFAULT) and \code{"topright"} will be \dQuote{out-of-the-way} placements.  Set \code{pos.est} to \code{NULL} to remove the estimated mortality rates from the plot.
#' @param ylab A label for the y-axis (\code{"Catch"} is the default).
#' @param xlab A label for the x-axis (\code{"Age"} is the default).
#' @param col.pt a string that indicates the color of the plotted points.
#' @param parm A numeric or string (of parameter names) vector that specifies which parameters are to be given confidence intervals  If missing, all parameters are considered.
#' @param conf.level A number representing the level of confidence to use for constructing confidence intervals.
#' @param level Same as \code{conf.level}.  Used for compatability with the generic \code{confint} function.
#' @param \dots Additional arguments for methods.
#'
#' @return A list with the following items:
#'  \itemize{
#'    \item age the original vector of assigned ages.
#'    \item catch the original vector of observed catches or CPUEs.
#'    \item age.e a vector of assigned ages used to estimate mortalities.
#'    \item catch.e a vector of catches or CPUEs used to estimate mortalities.
#'    \item age.r a vector of recoded ages used to estimate mortalities.  See references.
#'    \item n a numeric holding the intermediate calcualtion of n.  See references.
#'    \item T a numeric holding the intermediate calcualtion of T.  See references.
#'    \item est A 2x2 matrix that contains the estimates and standard errors for S and Z.
#'  }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{catchCurve}}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/CatchCurve.pdf}
#'
#' @references D.G. Chapman and D.S. Robson. 1960. The analysis of a catch curve. Biometrics. 16:354-368.
#'
#' D.S. Robson and D.G. Chapman. 1961. Catch curves and mortality rates.  Transactions of the American Fisheries Society. 90:181-189.
#'
#' Hoenig, J.M. and W.D. Lawing, and N.A. Hoenig.  1983.  Useing mean age, mean  length and median length data to estimate the total mortality rate.  International Council for the Exploration of the Sea, CM 1983/D:23, Copenhagen.
#'
#' @keywords htest manip
#'
#' @examples
#' data(BrookTroutTH)
#' cr <- with(BrookTroutTH,chapmanRobson(age,catch,2:6))
#' summary(cr)
#' confint(cr)
#'
#' ## demonstration of formula notation
#' cr2 <- chapmanRobson(catch~age,BrookTroutTH,2:6)
#' summary(cr2)
#'
#' ## demonstration of ability to work with missing age classes
#' age <- c(  2, 3, 4, 5, 7, 9,12)
#' ct  <- c(100,92,83,71,56,35, 1)
#' cr3 <- chapmanRobson(age,ct,4:12)
#' summary(cr3)
#'
#' @rdname chapmanRobson
#' @export
chapmanRobson <- function (x,...) {
  UseMethod("chapmanRobson") 
}

#' @rdname chapmanRobson
#' @export
chapmanRobson.default <- function(x,catch,ages2use=age,zmethod=c("Hoenigetal","original"),...) {
  zmethod <- match.arg(zmethod)
  age <- x
  rows2use <- match(ages2use,age)                                     # Find rows to use according to ages to use
  rows2use <- rows2use[!is.na(rows2use)]                              # Allows for missing ages in data
  age.e <- age[rows2use]                                              # Ages on the descending limb
  catch.e <- catch[rows2use]                                          # Catches on the descending limb
  age.r <- age.e-min(age.e)                                           # Find re-coded ages
  n <- sum(catch.e)                                                   # Statistics
  T <- sum(age.r*catch.e)
  S.est <- T/(n+T-1)                                                  # Estimate S
  S.SE <- sqrt((T/(n+T-1))*(T/(n+T-1)-(T-1)/(n+T-2)))                 # Estimate SE of S
  if (zmethod=="Hoenigetal") {
    Z.est <- -log(S.est) - ((n-1)*(n-2))/(n*(T+1)*(n+T-1))            # Estimate Z
    Z.SE <- (1-exp(-Z.est))/sqrt(n*exp(-Z.est))                       # Estimated SE of Z
  } else {
    Z.est <- -log(S.est)                                              # Estimate Z
    Z.SE <- S.SE/S.est                                                # Estimated SE of Z
  }
  mres <- cbind(c(100*S.est,Z.est),c(100*S.SE,Z.SE))
  rownames(mres) <- c("S","Z")
  colnames(mres) <- c("Estimate","Std. Err.")
  cr <- list(age=age,catch=catch,age.e=age.e,catch.e=catch.e,age.r=age.r,n=n,T=T,est=mres)
  class(cr) <- "chapmanRobson"
  cr
}

#' @rdname chapmanRobson
#' @export
chapmanRobson.formula <- function(x,data,ages2use=age,zmethod=c("Hoenigetal","original"),...) {
  zmethod <- match.arg(zmethod)
  mf <- model.frame(x,data=data)
  age <- mf[,2]
  catch <- mf[,1]
  chapmanRobson.default(age,catch,ages2use=ages2use,zmethod=zmethod,...)
}

#' @rdname chapmanRobson
#' @export
summary.chapmanRobson <- function(object,...) {
  cat("Intermediate Statistics\n")
  cat("n=",object$n,"; T=",object$T,"\n\n",sep="")
  cat("Estimates with Standard Errors\n")
  object$est
}

#' @rdname chapmanRobson
#' @export
confint.chapmanRobson <- function(object,parm=c("all","both","S","Z"),level=conf.level,conf.level=0.95,...) {
  parm <- match.arg(parm)
  z <- c(-1,1)*qnorm((1-(1-conf.level)/2))
  Sres <- rbind(S=object$est["S","Estimate"]+z*object$est["S","Std. Err."])    # compute S results
  Zres <- rbind(Z=object$est["Z","Estimate"]+z*object$est["Z","Std. Err."])    # compute Z results
  if (parm=="all" | parm=="both") res <- rbind(Sres,Zres)                      # Create output matrix
    else if (parm=="S") res <- Sres
      else res <- Zres
  colnames(res) <- iCILabel(conf.level)
  res
}

#' @rdname chapmanRobson
#' @export
plot.chapmanRobson <- function(x,pos.est="bottomleft",ylab="Catch",xlab="Age",col.pt="black",...) {
  old.par <- par(mar=c(6,3.5,1,1),mgp=c(2,0.5,0)); on.exit(par(old.par))
  yrng <- c(min(0,min(x$catch)),max(x$catch))
  plot(x$catch~x$age,col=col.pt,xlab="",ylab=ylab,ylim=yrng,xaxt="n",...)  # Plot raw data
  points(x$age.e,x$catch.e,col=col.pt,pch=19,cex=1.25)                     # Highlight descending limb portion
  axis(1,at=x$age,labels=x$age,line=0)                                     # Put age (original) axis on plot
  mtext(xlab,side=1,line=1.5)
  axis(1,at=x$age.e,labels=x$age.r,line=3)                                 # Put recoded age axis on plot
  mtext(paste("Recoded",xlab),side=1,line=4.5)
  if (!is.null(pos.est)) {                                                 # Put mortality val on graph
    Z <- x$est["Z","Estimate"]                                             #   Find mortality values
    S <- x$est["S","Estimate"]
    legend(pos.est,legend=paste("Z=",round(Z,3),"\nS=",round(S,1),"%",sep=""),bty="n")
  }
}
