#' @title Estimate initial population size for single or multiple census mark-recapture data.
#'
#' @description Estimates of the initial population size, along with associated confidence intervals, are constructed from single or multiple census mark-recapture data using a variety of methods.  For single census data, the initial population size (N) is estimated from the number of marked animals from a first sample (M), number of captured animals in a second sample (n), and the number of recaptured marked animals in the second sample (m) using either the \sQuote{naive} Petersen method or Chapman, Ricker, or Bailey modifications of the Petersen method.  Single census data can also be separated by group (e.g., size class) to estimate the initial population size by class and for the overall population size.  For multiple census data, the initial population size is estimated from the number of captured animals (n), number of recaptured marked animals (m), the number of marked animals that are marked and returned to the population (R), or the number of extant marked animals prior to the sample (M) on each of several samples using either the Schnabel (1938) or Schumacher-Eschmeyer (1943) method.
#'
#' @details For single census data, the following methods can be used:
#' \itemize{
#'   \item{\code{method="Petersen"}.  The \sQuote{naive} Petersen as computed using equation 2.1 from Krebs (1989).}
#'   \item{\code{method="Chapman"}.  The Chapman (1951) modification of the Petersen method as computed using equation 2.2 from Krebs (1989).}
#'   \item{\code{method="Ricker"}.  The Ricker (1975) modification of the Petersen as computed using equation 3.7 from Ricker (1975).  This is basically the same \code{method="Chapman"} except that Ricker (1975) did NOT subtract a 1 from the answer in the final step.  Thus, the estimate from \code{method="Chapman"} will always be one less than the estimate from \code{method="Ricker"}.}
#'   \item{\code{method="Bailey"}.  The Bailey (1951, 1952) modification of the Petersen as computed using equation 2.3 from Krebs (1989).}
#' }
#'
#' If \code{M} contains an object from \code{\link{capHistSum}} and one of Petersen, Chapman, Ricker, or Bailey methods has been selected with \code{method=} then \code{n=} and \code{m=} can be left missing or will be ignored and the needed data will be extracted from the \code{sum} portion of the \code{CapHist} class object.  If the data were not summarized with \code{\link{capHistSum}} then all of \code{M=}, \code{n=}, and \code{m=} must be supplied by the user.
#' 
#' The population estimate (as computed with the formulas noted in the table above) is extracted with \code{summary}.  In addition, the standard error of the population estimate (SE) can be extracted by including \code{incl.SE=TRUE}.  The SE is from equation 3.6 (p. 78) in Ricker (1975) for the Petersen method, from p. 60 (near bottom) of Seber (2002) for the Chapman method, from p. 61 (middle) of Seber (2002) (and as noted on p. 79 of Ricker (1975)) for the Bailey method, and from equation 3.8 (p. 78) in Ricker (1975) for the Ricker method.
#' 
#' Confidence intervals for the initial population size from the single census methods can be constructed using four different distributions as chosen with \code{type=} in \code{confint}.  If \code{type="suggested"} then the type of confidence interval suggested by the rules on p. 18 in Krebs (1989) are used.  The general methods for constructing confidence intervals for N are described below
#' 
#' \itemize{
##'  \item{\code{type="hypergeometric"}.  Uses \code{\link{hyperCI}}.  This is experimental at this point.}
##'  \item{\code{type="binomial"}.  Use \code{\link{binCI}} to construct a confidence interval for m/n (Petersen method) or (m+1)/(n+1) (Chapman, Bailey, Ricker methods), divides M or (M+1) by the CI endpoints, and substract 1 (for the Chapman method).}
##'  \item{\code{type="Poisson"}.  Use \code{\link{poiCI}} to construct a confidence interval for m (Petersent method) or (m+1) (Chapman, Bailey, Ricker methods), substitute the CI endpoints into the appropriate equation for estimating N, and subtract 1 (for the Chapman method).}
##'  \item{\code{type="normal"}.  Used equation 2.4 (p.20) from Krebs (2002) for the Petersen method.  For the other methods, used N+/- Z(0.975)*SE, where the SE was computed as noted above.}
##' }
#'
#' If \code{incl.all=TRUE} in \code{summary} and population estimates have been constructed for multiple sub-groups then an overall population estimate is included by summing the population estimates for the multiple sub-groups.  If \code{incl.SE=TRUE}, then an overall SE is computed by taking the square root of the summed VARIANCES for the multiple sub-groups.
#'
#' For multiple census data, the following methods can be declared for use with the \code{method=} argument:
#' \itemize{
#'   \item{\code{method="Schnabel"}.  The Schnabel (1938) method as computed with equation 3.15 from Ricker (1975).}
#'   \item{\code{method="SchumacherEschmeyer"}.  The Schumacher and Eschmeyer (1943) method as computed with equation 3.12 from Ricker (1975) eqn 3.12.}
#' }
#'
#' If \code{M} contains an object from \code{\link{capHistSum}} and the Schnabel or Schumacher-Eschmeyer methods has been chosen then \code{n}, \code{m} and \code{R} can be left missing or will be ignored.  In this case, the needed data is extracted from the \code{sum} portion of the \code{CapHist} class object.  Otherwise, the user must supply vectors of results in \code{n}, \code{m}, and \code{R} or \code{M}.
#' 
#' The population estimate for each method is extracted with \code{summary}.  Standard errors for the population estimate can NOT be computed for the Schnabel or Schumacher-Eschmeyer methods (a warning will be produced if \code{incl.SE=TRUE} is used).
#'
#' Confidence intervals for the initial population size using multiple census methods can be constructed using the normal or Poisson distributions for the Schnabel method or the normal distribution for the Shumacher-Eschmeyer method as chosen with \code{type=}.  If \code{type="suggested"} then the type of confidence interval suggested by the rule on p. 32 of Krebs (1989) is used (for the Schnabel method).  If \code{type="Poisson"} for the Schnabel method then a confidence interval for the sum of m is computed with \code{\link{poiCI}} and the end points are substituted into the Schnabel equation to produce a CI for the population size.  If \code{type="normal"} for the Schnabel method then the standard error for the \emph{inverse} of the population estimate is computed as the square root of equation 2.11 from Krebs (1989) or equation 3.16 from Ricker (1975).  The standard error for the Schumacher-Eschmeyer method is for the \emph{inverse} of the population estimate and is computed with equation 2.14 from Krebs (1989) [Note that the divisor in Krebs (1989) is different than the divisor in equation 3.12 in Ricker (1975), but is consistent with equation 4.17 in Seber (2002).]  The confidence interval for the \emph{inverse} population estimate is constructed from the inverse population estimate plus/minus a t critical value times the standard error for the inverse population estimate.  The t critical value uses the number of samples minus 1 for the Schnabel method and the number of samples minus 2 when for the Schumacher-Eschmeyer method according to p. 32 of Krebs (1989) (note that this is different than what Ricker (1975) does).  Finally, the confidence interval for the population estimate is obtained by inverting the confidence interval for the inverse population estimate.  Note that confidence intervals for the population size when \code{type="normal"} may contain negative values (for the upper value) when the population estimate is relatively large and the number of samples is small (say, three) because the intervals are orginally constructed on the inverted population estimate and they use the t-distribution.
#' 
#' The \code{plot} can be used to identify assumption violations in the Schnabel and Schumacher-Eschmeyer methods (an error will be returned if used with any of the other methods).  If the assumptions ARE met then the plot of the proportion of marked fish in a sample versus the cumulative number of marked fish should look linear.  A loess line (with approximate 95\% confidence bands) can be added to aid interpretation with \code{loess=TRUE}.  Note, however, that adding the loess line may return a number of warning or produce a non-informative if the number of samples is small (<8).
#'
#' @param M A numeric representing the number of marked fish from the first sample (single-census), an object from \code{capHistSum()} (single- or multiple-census), or numeric vector of marked fish prior to ith samples (multiple-census).
#' @param n A numeric representing the number of captured fish in the second sample (single-census) or numeric vector of captured fish in ith sample (multiple-census).
#' @param m A numeric representing the number of recaptured (marked) fish in the second sample (single-census) or numeric vector of recaptured (marked) fish in ith sample (multiple-census).
#' @param R A numeric vector representing the number of marked fish returned to the population (multiple-census).  Note that several references use the number of \dQuote{new} marks returned to the population rather than the \dQuote{total} number of marks returned to the population that is used here.
#' @param method A single string that identifies the type of calculation method to use in the main function.
#' @param type A single string that identifies the distribution to use when constructing confidence intervals in \code{confint}. See details.
#' @param labels A character or character vector used to label the rows of the resulting output matrix when using a single census method separated by groups.  Must be the same length as \code{M}, \code{n}, and \code{m}.  Defaults to upper-case letters if no values are given.
#' @param chapman.mod A logical that represents whether the Chapman modification should be used (\code{=TRUE}, default) or not (\code{=FALSE}) when performing the Schnabel multiple census method.
#' @param object,x An \code{mrClosed1} or \code{mrClosed2} object.
#' @param digits The number of decimal digits to round the population estimates to.  If \code{incl.SE=TRUE} then SE will be rounded to one more decimal place then given in \code{digits}.
#' @param incl.SE A logical that indicates whether the results should include the calculated SE value.  See details.
#' @param incl.all A logical that indicates whether an overall population estimate should be computed when using a single census method that has been separated into sub-groups.  See details.
#' @param verbose A logical that indicates whether a reminder of the inputted values and what type of method was used should be printed with the summary and confidence interval results.
#' @param parm Not used here (included in \code{confint} generic).
#' @param level Same as \code{conf.level} but used for compatability with \code{confint} generic.
#' @param conf.level A numeric representing the level of confidence to use for confidence intervals.
#' @param bin.type A string that identifies the method used to construct binomial confidence intervals (default is \code{"wilson"}).  This is only used if \code{type="binomial"} in \code{confint}.  See details of \code{\link{binCI}}.
#' @param poi.type A string that identifies the method used to construct Poisson confidence intervals (default is \code{"exact"}).  This is only used if \code{type="Poisson"} in \code{confint}.  See details of \code{\link{poiCI}}.
#' @param pch A numeric used to indicate the type of plotting character.
#' @param col.pt a string used to indicate the color of the plotted points.
#' @param xlab A label for the x-axis.
#' @param ylab A label for the y-axis.
#' @param loess A logical that indicates if a loess smoother line (and approximate 95\% confidence band) is fit to and shown on plot.
#' @param lty.loess A single numeric used to indicate the type of line used for the loess line.
#' @param lwd.loess A single numeric used to indicate the line width of the loess line.
#' @param col.loess A single string used to indicate the color of the loess line.
#' @param trans.loess A single numeric that indicates how transparent the loess band should be (larger numbers are more transparent).
#' @param span A single numeric that controls the degree of smoothing.  Values closer to 1 are more smooth.
#' @param \dots Additional arguments for methods.
#'
#' @return A list with the following items
#'  \itemize{
#'    \item M The number of marked fish from the first sample that was provided.
#'    \item n The number of captured fish in the second sample that was provided.
#'    \item m The number of recaptured (marked) fish in the second sample that was provided.
#'    \item M1 The adjusted (depending on \code{type}) number of marked fish from the first sample.
#'    \item n1 The adjusted (depending on \code{type}) number of captured fish in the second sample.
#'    \item m1 The adjusted (depending on \code{type}) number of recaptured (marked) fish in the second sample.
#'    \item cf A correction factor for the population estimate that depends on \code{type}.
#'    \item method The type of method used (provided by the user).
#'    \item methodLbl A label for the type of method used.
#'    \item N The estimated initial population size.
#'    \item labels Labels for the rows of summary matrix.
#'  }
#' 
#' @section Testing: The results from the single census methods have had the following checks.  The population estimates for all methods match reputable sources.  The SE for the Chapman and Bailey methods match the results from \code{\link[fishmethods]{mrN.single}} in \pkg{fishmethods},  The CI for the Petersen, Chapman, and Bailey methods partially match (are within 1% when they do not match) the CIs from reputable sources.
#' 
#' The results for the multiple census methods have had the following checks.  The population estimates for both methods match reputable sources.  The intermediate calculations for both methods match those in Krebs (1989).  The confidence interval for the Schnabel method using the Poisson distribution does NOT match Krebs (1989).  This appears to be a difference in the use \code{\link{poiCI}} here versus distributional tables in Krebs (i.e., the difference appears to be completely in the critical values from the Poisson distribution).  The confidence interval for the Schnabel method using the normal or the Poission distribution do NOT match Ricker (1975), but there is not enough information in Ricker to determine why (it is likely due to numerical differences on the inverse scale).  The confidence interval for the Schumacher-Eschmeyer method do match Krebs (1989) but not Ricker (1975).  The Ricker result may be due to different df as noted above.
#'  
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @section IFAR Chapter: 9-Abundance from Capture-Recapture Data.
#' 
#' @seealso See \code{\link{capHistSum}} for generating input data from capture histories.  See \code{\link{poiCI}}, \code{\link{binCI}}, and \code{\link{hyperCI}} for specifics on functions used in confidence interval constructuion.  See \code{\link{mrOpen}} for handling mark-recapture data in an open population.  See \code{\link[FSAdata]{SunfishIN}} in \pkg{FSAdata} for an example to test matching of results with Ricker (1975)'  See \code{\link[fishmethods]{mrN.single}} and \code{\link[fishmethods]{schnabel}} in \pkg{fishmethods} for similar functionality.
#' 
#' @references Ogle, D.H.  2016.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Krebs, C.J.  1989.  Ecological Methodology.  Addison-Welsey Educational Publishing.
#'
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.  [Was (is?) from http://www.dfo-mpo.gc.ca/Library/1485.pdf.]
#'
#' Seber, G.A.F. 2002. The Estimation of Animal Abundance and Related Parameters. Edward Arnold, second edition.
#'
#' Schnabel, Z.E.  1938. The estimation of the total fish population of a lake. American Mathematician Monthly, 45:348-352.
#'
#' Schumacher, F.X. and R.W. Eschmeyer. 1943.  The estimation of fish populations in lakes and ponds.  Journal of the Tennessee Academy of Sciences, 18:228-249.
#'
#' @keywords manip
#'
#' @aliases mrClosed summary.mrClosed1 confint.mrClosed1 summary.mrClosed2 confint.mrClosed2 plot.mrClosed2
#'
#' @examples
#' ### Single census with no sub-groups
#' ## Petersen estimate -- the default
#' mr1 <- mrClosed(346,184,49)
#' summary(mr1)
#' summary(mr1,verbose=TRUE)
#' summary(mr1,incl.SE=TRUE)
#' summary(mr1,incl.SE=TRUE,digits=1)
#' confint(mr1)
#' confint(mr1,verbose=TRUE)
#' confint(mr1,type="hypergeometric")
#'
#' ## Chapman modification of the Petersen estimate
#' mr2 <- mrClosed(346,184,49,method="Chapman")
#' summary(mr2,incl.SE=TRUE)
#' summary(mr2,incl.SE=TRUE,verbose=TRUE)
#'
#' ### Single census, using capHistSum() results
#' ## data in capture history format
#' data(BluegillJL)
#' str(BluegillJL)
#' ch1 <- capHistSum(BluegillJL)
#' mr3 <- mrClosed(ch1)
#' summary(mr3,verbose=TRUE)
#' confint(mr3,verbose=TRUE)
#'
#' ### Single census with sub-groups
#' marked <- c(93,35,72,16,46,20)
#' captured <- c(103,30,73,17,39,18)
#' recaps <- c(20,23,52,15,35,16)
#' lbls <- c("YOY","Juvenile","Stock","Quality","Preferred","Memorable")
#' mr4 <- mrClosed(marked,captured,recaps,method="Ricker",labels=lbls)
#' summary(mr4)
#' summary(mr4,incl.SE=TRUE)
#' summary(mr4,incl.SE=TRUE,verbose=TRUE)
#' summary(mr4,incl.SE=TRUE,incl.all=FALSE,verbose=TRUE)
#' confint(mr4)
#' confint(mr4,verbose=TRUE)
#' confint(mr4,incl.all=FALSE,verbose=TRUE)
#'
#' ### Multiple Census
#' ## Data in summarized form
#' data(PikeNY)
#'
#' ## Schnabel method
#' mr5 <- with(PikeNY,mrClosed(n=n,m=m,R=R,method="Schnabel"))
#' plot(mr5)
#' plot(mr5,loess=TRUE)
#' summary(mr5)
#' summary(mr5,verbose=TRUE)
#' confint(mr5)
#' confint(mr5,verbose=TRUE)
#'
#' ## Schumacher-Eschmeyer method
#' mr6 <- with(PikeNY,mrClosed(n=n,m=m,R=R,method="Schumacher"))
#' summary(mr6)
#' confint(mr6)
#'
#' ### Capture history data summarized by capHistSum()
#' data(PikeNYPartial1)
#' # ignore first column of ID numbers
#' ch2 <- capHistSum(PikeNYPartial1,cols2ignore="id")
#'
#' ## Schnabel method
#' mr7 <- mrClosed(ch2,method="Schnabel")
#' plot(mr7)
#' summary(mr7)
#' confint(mr7)
#'
#' @rdname mrClosed
#' @export
mrClosed <- function(M=NULL,n=NULL,m=NULL,R=NULL,
            method=c("Petersen","Chapman","Ricker","Bailey","Schnabel","SchumacherEschmeyer"),
            labels=NULL,chapman.mod=TRUE) {
  method <- match.arg(method)
  if (method %in% c("Petersen","Chapman","Ricker","Bailey")) {
    if (!is.null(R)) {
      ## R not in single methods.  If nothing else, throw error
      if (is.null(c(M,n,m))) STOP("'R' not used in single census methods;\n must supply 'M', 'n', and 'm'.")
      ## Otherwise warn that it will be ignored
      WARN("'R' not used in single census methods and will be ignored.")
    }
    iMRCSingle(M,n,m,method,labels)
  } else iMRCMultiple(M,n,m,R,method,chapman.mod)
}

############################################################################
## Methods related to SINGLE CENSUS (Petersen, Chapman, Ricker, or Bailey)
############################################################################
#===========================================================================
## SINGLE -- Main calculations
#===========================================================================
iMRCSingle <- function(M,n,m,method,labels) {
  # initial checks
  if (is.null(M)) STOP("'M' is missing; must be from 'capHistSum()' or value(s).")
  if (class(M)=="CapHist") {
    if (!is.null(m) | !is.null(n)) WARN("'m' and 'n' ignored when 'M' from capHistSum().")
    m <- M$sum$m
    n <- M$sum$n
    M <- M$sum$M    
  } else {
    if (is.null(n) | is.null(m)) STOP("One or both of 'n' or 'm' is missing without 'M' from capHistSum().")
    # Make sure that the vectors are of the same size
    lengths <- c(length(M),length(n),length(m))
    if (any(diff(lengths)!=0)) STOP("'M', 'n', or 'm' vectors must be same length.")
  }
  # Make sure that recapture number makes sense relative to sample size
  if (any((n-m)<0)) {
    if (length(n)==1) STOP("Can't have more recaptures (m) then total individuals (n).")
    else STOP("Row ",which((n-m)<0)," has more recaptures (m) then total individuals (n).")
  } 
  # If no labels then assign letters, if labels then make sure size is correct
  if (is.null(labels)) {
    if (length(M)>1) labels=LETTERS[1:length(M)]
  } else {
    if (length(M) != length(labels)) STOP("'labels' must be same length as 'M', 'n', and 'm'.")
  }
  # handle modifications for simplicity of calculation below  
  switch(method,
         Petersen={methodLbl="the 'naive' Petersen method"
                   M1 <- M; n1 <- n; m1 <- m; cf <- rep(0,length(M)) },
         Chapman={methodLbl="Chapman's modification of the Petersen method"
                  M1 <- M+1; n1 <- n+1; m1 <- m+1; cf <- rep(1,length(M)) },
         Ricker={methodLbl="Ricker's modification of the Petersen method"
                 M1 <- M+1; n1 <- n+1; m1 <- m+1; cf <- rep(0,length(M)) },
         Bailey={methodLbl="Bailey's modification of the Petersen method"
                 M1 <- M; n1 <- n+1; m1 <- m+1; cf <- rep(0,length(M)) }
  ) # end switch
  # create list of inputs, intermediate results, and calculations
  res <- list(M=M,n=n,m=m,M1=M1,n1=n1,m1=m1,cf=cf,N=M1*n1/m1-cf,
              labels=labels,method=method,methodLbl=methodLbl)
  class(res) <- "mrClosed1"
  res
}

#===========================================================================
# SINGLE -- Print PE and SE
#===========================================================================
#' @rdname mrClosed
#' @export
summary.mrClosed1 <- function(object,digits=0,incl.SE=FALSE,incl.all=TRUE,verbose=FALSE,...) {
  # Put descriptive label of input values at top of output if the user asked for it.
  if(verbose) {
    if (is.null(object$labels)) message("Used ",object$methodLbl," with M=",
                                        object$M,", n=",object$n,
                                        ", and m=",object$m,".\n")
    else {
      message("Used ",object$methodLbl," with observed inputs (by group) of:")
      tmp <- paste0("  ",object$labels,": M=",object$M,", n=",object$n,", and m=",object$m)
      for (i in tmp) message(i)
    }
  }
  # Put the PE into a vector to return
  res <- cbind(round(object$N,digits))
  colnames(res) <- "N"
  # Add the SE to the return vector if the user asked for it (and it can be calculated)
  if (incl.SE) {
    res <- cbind(res,round(sqrt(iMRCSingleVar(object)),digits+1))
    colnames(res)[2] <- "SE"
  }
  # Label rows if labels provided
  rownames(res) <- object$labels
  # Include an overall PE if the user asked for it
  if (incl.all & length(object$N)>1) {
    N <- sum(res[,"N"])
    if (incl.SE) {
      SE <- round(sqrt(sum(res[,"SE"]^2)),digits+1)
      res <- rbind(res,cbind(N,SE))
    } else res <- rbind(res,N)
    rownames(res)[dim(res)[1]] <- "All"
  }
  # Return the result
  res
}


#===========================================================================
# SINGLE -- Internal helper for compute the SE
#===========================================================================
iMRCSingleVar <- function(object) {
  if (object$method == "Petersen") {
    ##  From equation 3.6 (p. 78) in Ricker (1975)
    V <- with(object, (M^2)*n*(n-m)/(m^3) )
  } else if (object$method == "Chapman") {
    ## From p. 60 (near bottom) of Seber (2002)
    V <- with(object, M1*n1*(M1-m1)*(n1-m1)/((m1^2)*(m1+1)) )
  } else if (object$method=="Bailey") {
    ## From p. 61 (middle) of Seber (2002) and as noted on p. 79 of Ricker (1975)
    V <- with(object, ((M^2)*n1*(n-m))/((m1^2)*(m1+1)) )
  } else if (object$method=="Ricker") {
    ## From equation 3.8 in Ricker (1975)
    V <- with(object, ((M1^2)*n1*(n-m))/((m1^2)*(m1+1)) )
  }
  V
}

#===========================================================================
# SINGLE -- Confidence intervals
#===========================================================================
#' @rdname mrClosed
#' @export
confint.mrClosed1 <- function(object,parm=NULL,level=conf.level,conf.level=0.95,digits=0,
                              type=c("suggested","binomial","hypergeometric","normal","Poisson"),
                              bin.type=c("wilson","exact","asymptotic"),
                              poi.type=c("exact","daly","byar","asymptotic"),
                              incl.all=TRUE,verbose=FALSE,...) {
  # Initial checks
  type <- match.arg(type)
  bin.type <- match.arg(bin.type)
  poi.type <- match.arg(poi.type)
  parm <- iCI.CheckParm(parm)
  if (conf.level<=0 | conf.level>=1) STOP("'conf.level' must be between 0 and 1")
  # Construct the CIs, loop is for handling multiple groups
  ci <- NULL
  for (i in 1:length(object$N)) {
    temp <- with(object,
                 list(M=M[i],n=n[i],m=m[i],M1=M1[i],n1=n1[i],m1=m1[i],cf=cf[i],
                      method=method,methodLbl=methodLbl,N=N[i],labels=labels[i])
    )
    ci <- rbind(ci,iCI.MRCSingle(temp,conf.level,type,bin.type,poi.type,verbose,...))
  }
  # Add labels to the matrix
  rownames(ci) <- object$labels
  colnames(ci) <- iCILabel(conf.level)
  # Include a CI for the overall CI if asked for
  if (incl.all & length(object$N)>1) {
    if (verbose) message("All - The normal distribution was used.")
    # get Ns and SEs from summary
    smry <- summary(object,incl.SE=TRUE,incl.all=TRUE)
    zalpha <- c(-1,1)*stats::qnorm(0.5+conf.level/2)
    ci.all <- smry["All","N"]+zalpha*smry["All","SE"] 
    ci <- rbind(ci,ci.all)
    rownames(ci)[nrow(ci)] <- "All"
  }
  round(ci,digits)
}

#===========================================================================
# SINGLE -- Internal helper for computing the CIs
#===========================================================================
iCI.CheckParm <- function(parm) { # also used for multiple
  if(!is.null(parm)) {
    WARN("'parm' is meaningless for this class of object; reset to NULL.\n\n")
    parm <- NULL
  }
  parm
}

iCI1.HandleSuggested <- function(object) {
  if ((object$m/object$n) > 0.10) type <- "binomial"
  else if (object$m > 50) type <- "normal"
  else type <- "Poisson"
  type
}

iCI1.HandleVerbose <- function(object,type,bin.type,poi.type) {
  if (type=="binomial") type <- paste0("binomial (",bin.type," method)")
  if (type=="Poisson") type <- paste0("Poisson (",poi.type," method)")
  msg <- paste("The",type,"distribution was used.")
  if (!is.null(object$labels)) msg <- paste(object$labels,"-",msg)
  message(msg)  
}

iCI.MRCSingle <- function(object,conf.level,type,bin.type,poi.type,verbose,...) {
  # Follow Sebers' suggestions if asked to
  if (type=="suggested") type <- iCI1.HandleSuggested(object)
  # Put message at top of output if asked for
  if (verbose) iCI1.HandleVerbose(object,type,bin.type,poi.type)
  # Construct CIs according to type=
  switch(type,
         hypergeometric={
           ci <- hyperCI(object$M,object$n,object$m,conf.level)
         }, 
         binomial={
           # Binomial CI for phat
           ci1 <- binCI(object$m1,object$n1,conf.level,bin.type)
           # Convert to CI for N
           N.bin <- (object$M1/ci1)-object$cf
           ci <- N.bin[2:1]
         },
         Poisson={
           # Poisson CI for m
           m.ci <- poiCI(object$m,conf.level,type=poi.type)
           # Convert to CI for m1
           if (object$method!="Petersen") m.ci <- m.ci+1
           # Put endpoints back in N formula to get CI for N
           N.poi <- (object$M1*object$n1)/m.ci-object$cf    
           ci <- N.poi[,2:1]
         }, 
         normal={
           # Find +/- Z for normal CI
           zalpha <- c(-1,1)*stats::qnorm(0.5+conf.level/2)
           if (object$method=="Petersen") {
             ## Krebs eqn 2.4 (p.20), built in parts
             # Find phat
             phat <- object$m/object$n
             # Find finite population correction factor
             fpc <- 1-object$m/object$M
             # Correction for continuity
             cc <- 1/(2*object$n)
             # SE for phat
             SE <- sqrt(fpc*phat*(1-phat)/(object$n-1))
             # CI for phat
             ci <- phat-zalpha*SE+cc
             # CI for N
             ci <- rbind(object$M/ci)
           } else { ## get SE from summary method
             ci <- object$N+zalpha*sqrt(iMRCSingleVar(object))
           }
         }
  )
  ci
}


############################################################################
## Methods related to MULTIPLE CENSUS (Schnabel, Schumacher-Eschmeyer)
############################################################################
#===========================================================================
## MULTIPLE -- Main calculations
#===========================================================================
iMRCMultiple <- function(M,n,m,R,method,chapman.mod) {
  # Initial Checks
  if (!is.null(M)) {
    if (class(M)=="CapHist") {
      ## Results come from capHistSum
      n <- M$sum$n
      m <- M$sum$m
      R <- M$sum$R
      M <- cumsum(R-m)-(R-m)
    } else {
      ## Results not from capHistSum and values of M provided.
      # check if M is a vector
      if (!is.vector(M)) STOP("If given, 'M' must be a vector or from 'capHistSum()`.")
      # check if M has an NA in first position
      if (is.na(M[1])) {
        WARN("NA for first sample of 'M' was ignored.")
        M[1] <- 0
      }
      if (is.null(n) | is.null(m)) STOP("One or both of 'n' or 'm' is missing without 'M' from capHistSum().")
      else if (!is.null(R)) WARN("Only need one of 'M' or 'R'.  'R' is ignored.")
      R <- n
      R[length(R)] <- 0
    }
  } else {
    ## M not provided and results not from capHistSum
    if (is.null(R)) STOP("One of 'M' or 'R' must be supplied by user")
    if (!is.vector(R)) STOP("If given, 'R' must be a vector.")
    if (is.null(n) | is.null(m)) STOP("One or both of 'n' or 'm' is missing.")
    # check if R has NA in last position
    if (is.na(R[length(R)])) {
      WARN("NA for last sample of 'R' was ignored.")
      R[length(R)] <- 0
    }
    # check if m has NA in first position
    if (is.na(m[1])) {
      WARN("NA for first sample of 'm' was ignored.")
      m[1] <- 0
    }
    # find M from R and m
    M <- cumsum(R-m)-(R-m)
  }
  # calculate intermediate values
  sum.m <- sum(m)
  sum.nM <- sum(n*M)
  sum.nM2 <- sum(n*(M^2))
  sum.mM <- sum(m*M)
  sum.m2dn <- sum((m^2)/n)
  # perform the estimates
  switch(method,
         Schnabel={
           methodLbl <- "the Schnabel method"     
           ifelse(chapman.mod, N <- sum.nM/(sum.m+1), N <- sum.nM/sum.m) },
         Schumacher=,SchumacherEschmeyer={
           methodLbl <- "the Schumacher-Eschmeyer method"
           chapman.mod <- FALSE
           N <- sum.nM2/sum.mM }
  ) # end switch
  # List of input values, intermediate and final results
  res <- list(n=n,m=m,R=R,M=M,N=N,sum.m=sum.m,sum.nM=sum.nM,sum.nM2=sum.nM2,
              sum.mM=sum.mM,sum.m2dn=sum.m2dn,labels=NULL,method=method,
              methodLbl=methodLbl,chapman.mod=chapman.mod)
  class(res) <- "mrClosed2"
  res
}

#===========================================================================
# MULTIPLE -- Print PE
#===========================================================================
#' @rdname mrClosed
#' @export
summary.mrClosed2 <- function(object,digits=0,verbose=FALSE,...) {
  # Put descriptive label of input values at top of output if the user asked for it.
  if(verbose) {
    msg <- paste0("Used ",object$methodLbl)
    ifelse(object$chapman.mod,msg <- paste(msg,"with Chapman modification.\n"),msg <- paste0(msg,".\n"))
    message(msg)
  }
  # Put the PE into a matrix to return
  res <- cbind(round(object$N,digits))
  colnames(res) <- "N"
  res
}

#===========================================================================
# MULTIPLE -- Confidence intervals
#===========================================================================
#' @rdname mrClosed
#' @export
confint.mrClosed2 <- function(object,parm=NULL,level=conf.level,conf.level=0.95,digits=0,
                              type=c("suggested","normal","Poisson"),
                              poi.type=c("exact","daly","byar","asymptotic"),
                              verbose=FALSE,...) {
  # Initial Checks
  type <- match.arg(type)
  if (type=="suggested") type <- iCI2.HandleSuggested(object)
  if (verbose) message("The ",type," distribution was used.")
  parm <- iCI.CheckParm(parm)
  if (conf.level<=0 | conf.level>=1) STOP("'conf.level' must be between 0 and 1")
  # Construct the confidence intervals
  switch(object$method,
         Schnabel= { ci <- iCI2.MRCSchnabel(object,conf.level,type,poi.type,verbose,...) },
         SchumacherEschmeyer= { ci <- iCI2.MRCSchumacher(object,conf.level,type,verbose,...) }
         ) # end switch
  # CI labels for the materix
  colnames(ci) <- iCILabel(conf.level)
  # print out the CIs
  round(ci,digits)
}

iCIt <- function(est,SE,obsdf,conf.level) {
  ## Internal function for computing normal theory CIs -- from NCStats
  hw <- stats::qt(1-(1-conf.level)/2,obsdf)*SE
  res <- cbind(est-hw,est+hw)
  colnames(res) <- iCILabel(conf.level)
  res
}

iCI2.HandleSuggested <- function(object) {
  if (object$sum.m < 50) type <- "Poisson"
  else type <- "normal"
  type
}

iCI2.MRCSchnabel <- function(object,conf.level,type,poi.type,...) {
  if (type=="normal") {
    # Get df (from Krebs p. 32)
    df <- length(object$n)-1
    # Compute SE for inverse of N (from Krebs 2.11, Ricker 3.16)
    ifelse(object$chapman.mod, invN.SE <- with(object, sqrt((sum.m+1)/(sum.nM^2)) ),
           invN.SE <- with(object, sqrt(sum.m/(sum.nM^2)) ) )
    # Compute CI for inverse of N
    invN.ci <- iCIt(1/object$N,invN.SE,df,conf.level)
    # Invert to get CI for N
    ci <- rbind((1/invN.ci)[2:1])
  } else {
    # Get Poisson CI for sum m
    ci1 <- poiCI(object$sum.m,conf.level,poi.type)
    # Change if chapman modification was used
    ifelse(object$chapman.mod,N.poi <- object$sum.nM/(ci1+1),
           N.poi <- object$sum.nM/ci1)
    ci <- rbind(N.poi[2:1])
  }
  ci
}

iCI2.MRCSchumacher <- function(object,conf.level,type,...) {
  # check type
  if (object$method=="SchumacherEschmeyer" & type!="normal") {
    WARN("'type' changed to 'normal' for the Schumacher-Eschmeyer method.")
  }
  # Get df (from from Krebs p. 32)
  df <- length(object$n)-2
  # Compute SE for inverse of N (from Krebs 2.14)
  invN.SE <- with(object, sqrt(((sum.m2dn-(sum.mM^2)/sum.nM2)/df)/(sum.nM2)) )
  # Compute CI for inverse of N
  invN.ci <- iCIt(1/object$N,invN.SE,df,conf.level)
  # Invert to get CI for N
  ci <- rbind((1/invN.ci)[2:1])
  ci
}

#===========================================================================
# MULTIPLE -- Plot for assumption violation detection
#===========================================================================
#' @rdname mrClosed
#' @export
plot.mrClosed2 <- function(x,pch=19,col.pt="black",
                           xlab="Marked in Population",
                           ylab="Prop. Recaptures in Sample",
                           loess=FALSE,lty.loess=2,lwd.loess=1,
                           col.loess="gray20",trans.loess=10,span=0.9,...) {
  graphics::plot(x$M,x$m/x$n,pch=pch,col=col.pt,xlab=xlab,ylab=ylab,...)
  # add loess line if asked for
  if (loess) iAddLoessLine(x$m/x$n,x$M,lty.loess,lwd.loess,col.loess,trans.loess,span=span)
}
