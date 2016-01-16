#' @title Computes Chapman-Robson estimates of S and Z.
#'
#' @description Computes the Chapman-Robson estimates of annual survival rate (S) and instantaneous mortality rate (Z) from catch-at-age data on the descending limb of a catch-curve.  Method functions extract estimates with associated standard errors and confidence intervals.  A plot method highlights the descending-limb, shows the linear model on the descending limb, and, optionally, prints the estimated Z and A.
#'
#' @details The default is to use all ages in the age vector.  This is only appropriate if the age and catch vectors contain only the ages and catches on the descending limb of the catch curve.  Use \code{ages2use} to isolate only the catch and ages on the descending limb.
#'
#' The Chapman-Robson method provides an estimate of the annual survival rate, with the annual mortality rate (A) determined by 1-S.  The instantaneous mortality rate is often computed as -log(S).  However, Hoenig et al. (1983) showed that this produced a biased (over)estimate of Z and provided a correction.  The correction is applied by setting \code{zmethod="Hoenigetal"}.  Smith et al. (2012) showed that the Hoenig et al. method should be corrected for a variance inflation factor.  This correction is applied by setting \code{zmethod="Smithetal"} (which is the default behavior).  Choose \code{zmethod="original"} to use the original estimates for Z and it's SE as provided by Chapman and Robson.
#' 
#' @param x A numerical vector of the assigned ages in the catch curve or a formula of the form \code{catch~age} when used in \code{chapmanRobson}.  An object saved from \code{chapmanRobson} (i.e., of class \code{chapmanRobson}) when used in the methods.
#' @param object An object saved from the \code{chapmanRobson} call (i.e., of class \code{chapmanRobson}).
#' @param catch A numerical vector of the catches or CPUEs for the ages in the catch curve.  Not used if \code{x} is a formula.
#' @param data A data frame from which the variables in the \code{x} formula can be found.  Not used if \code{x} is not a formula.
#' @param ages2use A numerical vector of the ages that define the descending limb of the catch curve.
#' @param zmethod  A string that indicates the method to use for estimating Z.  See details.
#' @param verbose A logical that indicates whether the method should return just the estimate (\code{FALSE}; default) or a more verbose statement.
#' @param pos.est A string to identify where to place the estimated mortality rates on the plot.  Can be set to one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"} for positioning the estimated mortality rates on the plot.  Typically \code{"bottomleft"} (DEFAULT) and \code{"topright"} will be \dQuote{out-of-the-way} placements.  Set \code{pos.est} to \code{NULL} to remove the estimated mortality rates from the plot.
#' @param cex.est A single numeric characther expansion value for the estimated mortaliry rates on the plot.
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
#' @section Testing: Tested the results of chapmanRobson against the results in Miranda and Bettoli (2007).  The point estimates of S matched perfectly but the SE of S did not because Miranda and Bettoli used a rounded estimate of S in the calculation of the SE of S but chapmanRobson does not.
#' 
#' Tested the results against the results from \code{agesurv} in \pkg{fishmethods} using the \code{rockbass} data.frame in \pkg{fishmethods}.  Results for Z and the SE of Z matched perfectly for non-bias-corrected results.  The estimate of Z, but not the SE of Z, matched for the bias-corrected (following Smith et al. (2012)) results.  \pkg{FSA} uses equation 2 from Smith et al. (2012) whereas \pkg{fishmethods} appears to use equation 5 from the same source to estimate the SE of Z.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 11-Mortality.
#'
#' @seealso See \code{\link[fishmethods]{agesurv}} in \pkg{fishmethods} for similar functionality.  See \code{\link{catchCurve}} and \code{\link[fishmethods]{agesurvcl}} in \pkg{fishmethods} for alternative methods.  See \code{\link{metaM}} for empirical methods to estimate natural mortality.
#' 
#' @references Ogle, D.H.  2016.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Chapman, D.G. and D.S. Robson. 1960. The analysis of a catch curve. Biometrics. 16:354-368.
#'
#' Hoenig, J.M. and W.D. Lawing, and N.A. Hoenig.  1983.  Using mean age, mean length and median length data to estimate the total mortality rate.  International Council for the Exploration of the Sea, CM 1983/D:23, Copenhagen.
#'
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.  [Was (is?) from http://www.dfo-mpo.gc.ca/Library/1485.pdf.]
#'
#' Robson, D.S. and D.G. Chapman. 1961. Catch curves and mortality rates.  Transactions of the American Fisheries Society. 90:181-189.
#' 
#' Smith, M.W., A.Y. Then, C. Wor, G. Ralph, K.H. Pollock, and J.M. Hoenig. 2012. Recommendations for catch-curve analysis. North American Journal of Fisheries Management. 32:956-967.
#'
#' @keywords htest manip
#'
#' @aliases chapmanRobson chapmanRobson.default chapmanRobson.formula plot.chapmanRobson summary.chapmanRobson confint.chapmanRobson
#'
#' @examples
#' data(BrookTroutTH)
#' plot(catch~age,data=BrookTroutTH,pch=19)
#' 
#' ## demonstration of formula notation
#' cr1 <- chapmanRobson(catch~age,data=BrookTroutTH,ages2use=2:6)
#' summary(cr1)
#' summary(cr1,verbose=TRUE)
#' confint(cr1)
#' plot(cr1)
#' 
#' ## demonstration of excluding ages2use
#' cr2 <- chapmanRobson(catch~age,data=BrookTroutTH,ages2use=-c(0,1))
#' summary(cr2)
#' plot(cr2)
#' 
#' ## demonstration of ability to work with missing age classes
#' age <- c(  2, 3, 4, 5, 7, 9,12)
#' ct  <- c(100,92,83,71,56,35, 1)
#' cr3 <- chapmanRobson(age,ct,4:12)
#' summary(cr3)
#' plot(cr3)
#'
#' @rdname chapmanRobson
#' @export
chapmanRobson <- function (x,...) {
  UseMethod("chapmanRobson") 
}

#' @rdname chapmanRobson
#' @export
chapmanRobson.default <- function(x,catch,ages2use=age,zmethod=c("Smithetal","Hoenigetal","original"),...) {
  ## Put x into age variable for rest of function
  age <- x
  
  ## Some Checks
  zmethod <- match.arg(zmethod)
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  if (!is.numeric(catch)) stop("'catch' must be numeric.",call.=FALSE)
  if (length(age)!=length(catch)) stop("'age' and 'catch' have different lengths.",call.=FALSE)
  # Check to make sure enough ages and catches exist
  if (length(age)<2) stop("Fewer than 2 data points.",call.=FALSE)

  ## Isolate the ages and catches to be used
  # Find rows to use according to ages to use, adjust if missing values occur
  rows2use <- iCheck_ages2use(ages2use,age)
  # Create new vectors with just the data to use
  age.e <- age[rows2use]
  catch.e <- catch[rows2use]
  # Check to make sure enough ages and catches exist
  if (length(age.e)<2) stop("Fewer than 2 data points after applying 'ages2use'.",call.=FALSE)
  # Create re-coded ages
  age.r <- age.e-min(age.e,na.rm=TRUE)
  
  ## Compute intermediate statistics
  n <- sum(catch.e,na.rm=TRUE)
  T <- sum(age.r*catch.e,na.rm=TRUE)
  ## Estimate S and SE (eqns 6.4 & 6.5 from Miranda & Bettoli (2007))
  S.est <- T/(n+T-1)
  S.SE <- sqrt(S.est*(S.est-((T-1)/(n+T-2))))
  ## Estimate Z and SE
  switch(zmethod,
         original= {
           Z.est <- -log(S.est)
           # from Jensen (1985)
           Z.SE <- S.SE/S.est },
         Hoenigetal= {
           # From eqn 1 in Smith et al. (2012) but noting that their
           #   Tbar is T/n, N is n, and Tc is ignored b/c of the re-coding
           Z.est <- -log(S.est) - ((n-1)*(n-2))/(n*(T+1)*(n+T-1))
           # Square root of eqn 2 in Smith et al. (2012)
           Z.SE <- (1-exp(-Z.est))/sqrt(n*exp(-Z.est)) },
         Smithetal= {
           # Same as for Hoenig et al. but including the chi-square
           #   VIF noted on last full paragraph in the left column
           #   on page 960 of Smith et al.
           Z.est <- -log(S.est) - ((n-1)*(n-2))/(n*(T+1)*(n+T-1))
           Z.SE <- (1-exp(-Z.est))/sqrt(n*exp(-Z.est))
           # Create the VIF as the usual chi-square GOF test 
           #   statistics divided by number of age-groups-1
           exp <- catch.e[1]*S.est^age.r
           chi <- sum(((catch.e-exp)^2)/exp,na.rm=TRUE)
           VIF <- chi/(length(catch[!is.na(catch)])-1)
           # Adjust the Z SE by the square root of the VIF
           Z.SE <- Z.SE*sqrt(VIF) }
  ) # end switch
  ## Prepare result to return
  mres <- cbind(c(100*S.est,Z.est),c(100*S.SE,Z.SE))
  rownames(mres) <- c("S","Z")
  colnames(mres) <- c("Estimate","Std. Error")
  cr <- list(age=age,catch=catch,age.e=age.e,catch.e=catch.e,age.r=age.r,n=n,T=T,est=mres)
  class(cr) <- "chapmanRobson"
  cr
}

#' @rdname chapmanRobson
#' @export
chapmanRobson.formula <- function(x,data,ages2use=age,zmethod=c("Smithetal","Hoenigetal","original"),...) {
  ## Handle the formula and perform some checks
  tmp <- iHndlFormula(x,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) stop("'chapmanRobson' must have only one LHS variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'chapmanRobson' must have only one RHS variable.",call.=FALSE)
  if (!tmp$Eclass %in% c("numeric","integer")) stop("RHS variable must be numeric.",call.=FALSE)
  ## Get variables from model frame
  age <- tmp$mf[,tmp$Enames]
  catch <- tmp$mf[,tmp$Rname]
  ## Call the default function
  chapmanRobson.default(age,catch,ages2use=ages2use,zmethod=zmethod,...)
}

#' @rdname chapmanRobson
#' @export
summary.chapmanRobson <- function(object,verbose=FALSE,...) {
  if (verbose) {
    message("Intermediate statistics: ",
            "n=",object$n,"; T=",object$T)
  }
  object$est
}

#' @rdname chapmanRobson
#' @export
confint.chapmanRobson <- function(object,parm=c("all","both","S","Z"),level=conf.level,conf.level=0.95,...) {
  parm <- match.arg(parm)
  z <- c(-1,1)*stats::qnorm((1-(1-conf.level)/2))
  # compute S results
  Sres <- rbind(S=object$est["S","Estimate"]+z*object$est["S","Std. Error"])
  # compute Z results
  Zres <- rbind(Z=object$est["Z","Estimate"]+z*object$est["Z","Std. Error"])
  # Create output matrix
  if (parm=="all" | parm=="both") res <- rbind(Sres,Zres)
    else if (parm=="S") res <- Sres
      else res <- Zres
  colnames(res) <- iCILabel(conf.level)
  res
}

#' @rdname chapmanRobson
#' @export
plot.chapmanRobson <- function(x,pos.est="topright",cex.est=0.95,ylab="Catch",xlab="Age",col.pt="gray30",...) {
  # Need to make area below x-axis larger to hold re-coded ages scale
  opar <- npar <- graphics::par("mar")
  npar[1] <- 6
  graphics::par(mar=npar)
  # Find range for y-axis
  yrng <- c(min(0,min(x$catch)),max(x$catch))
  # Plot raw data
  graphics::plot(x$catch~x$age,col=col.pt,xlab="",ylab=ylab,ylim=yrng,xaxt="n",...)
  # Highlight descending limb portion
  graphics::points(x$age.e,x$catch.e,col=col.pt,pch=19)
  # Put age (original) axis on plot
  graphics::axis(1,at=x$age,labels=x$age,line=0)
  graphics::mtext(xlab,side=1,line=1.5)
  # Put recoded age axis on plot
  graphics::axis(1,at=x$age.e,labels=x$age.r,line=3)
  graphics::mtext(paste("Recoded",xlab),side=1,line=4.5)
  # Put mortality values on plot
  if (!is.null(pos.est)) {
    Z <- x$est["Z","Estimate"]
    S <- x$est["S","Estimate"]
    graphics::legend(pos.est,legend=paste("Z=",round(Z,3),"\nS=",round(S,1),"%",sep=""),
                     bty="n",cex=cex.est)
  }
  graphics::par(mar=opar)
}

##############################################################
# INTERNAL FUNCTIONS
##############################################################
# Note that iCheck_age2use() is in catchCurve()