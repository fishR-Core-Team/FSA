#' @title Estimate initial population size for single or multiple census mark-recapture data.
#'
#' @description Estimates of the initial population size, along with associated confidence intervals, are constructed from single or multiple census mark-recapture data using a variety of methods.  For single census data, the initial population size is estimated from the number of marked animals from a first sample, number of captured animals in a second sample, and the number of recaptured marked animals in the second sample using either the \sQuote{naive} Petersen method or Chapman, Ricker, or Bailey modifications of the Petersen method.  Single census data can also be separated by group (e.g., size class) to estimate the initial population size by class and for the overall population size.  For multiple census data, the initial population sizes is estimated from the number of captured animals, number of recaptured marked animals, the number of marked animals that are marked and returned to the population, or the number of extant marked animals prior to the sapmle on each of several samples using either the Schnabel (1938) or Schumacher-Eschmeyer (1943) method.
#'
#' @details The main \code{mrClosed} function computes a variety of values that are intermediate to the final population estimate and its standard error and confidence intervals.  For single census data, the following methods can be declared for use with the \code{type=} argument:
#'
#' \tabular{ll}{
#'  \code{type="Petersen"} \tab naive Petersen (from Krebs (1989) eqn 2.1) .\cr
#'  \code{type="Chapman"} \tab Chapman (1951) modification of the Petersen (from Krebs (1989) eqn 2.2).\cr
#'  \code{type="Ricker"} \tab Ricker (1975) modification of the Chapman modification (from Ricker (1975) eqn 3.7).\cr
#'  \code{type="Bailey"} \tab Bailey (1951,1952) modification of the Petersen (Krebs (1989) eqn 2.3).
#' }
#'
#' If \code{M} contains an object from the \code{capHistSum} function and one of Petersen, Chapman, Ricker, or Bailey methods has been chosen then \code{n} and \code{m} can be left missing or will be ignored.  In this case, the function will extract the needed data from the \code{sum} portion of the \code{CapHist} class object.  If the data were not summarized with \code{\link{capHistSum}} then all of \code{M}, \code{n}, and \code{m} must be supplied by the user.  The popuation estimate for each method is returned from \code{summary}.
#'
#' Confidence intervals for the initial population size estimated with the single census methods can be constructed using four different distributions as chosen with \code{type=} in \code{confint}.  If \code{citype="suggested"} then the type of confidence interval suggested by the rules in Seber (2002) is used.  The hypergeometric method should be exact (with sampling without replacement) but is experimental at this point.  The other three types of confidence intervals are constructed according to Seber (2002) but using computer algorithms to estimate the distributions rather than tables and graphs as in Seber (2002).  If \code{type="normal"} then the confidence interval is constructed from normal theory (i.e., plus/minus z critical value times the standard error).  In this case, the standard error for the Petersen method is the square root of eqn 3.6 in Ricker(1975), the standard error for the Chapman method uses square root of the unnumbered equation on page 60 of Seber(2002), the standard error for the Ricker method uses the square root of eqn 3.8 in Ricker(1975), and the standard error for the Bailey method uses the square root of the unnumbered equation on page 61 of Seber (2002).  The standard error for the single census population estimate will be returned, primarily for historical purposes, if \code{incl.SE=TRUE} is used in \code{summary}.
#'
#' If \code{incl.all=TRUE} in the \code{summary} function and population estimates have been constructed for multiple sub-groups then an overall population estimate will be included by summing the population estimates for the multiple sub-groups.  In this case, if \code{incl.SE=TRUE}, then an overall SE will be computed by taking the square root of the summed VARIANCES for the multiple sub-groups.
#'
#' For multiple census data, the following methods can be declared for use with the \code{type=} argument:
#'
#' \tabular{ll}{
#'  \code{type="Schnabel"} \tab Schnabel (1938) method (from Ricker (1975) eqn 3.15).\cr
#'  \code{type="SchumacherEschmeyer"} \tab Schumacher and Eschmeyer (1943) method (from Ricker (1975) eqn 3.12).
#' }
#'
#' If \code{M} contains an object from \code{\link{capHistSum}} and the Schnabel or Schumacher-Eschmeyer methods has been chosen then \code{n}, \code{m} and \code{R} can be left missing or will be ignored.  In this case, the function will extract the needed data from the \code{sum} portion of the \code{CapHist} class object.  Otherwise, the user must supply vectors of results in \code{n}, \code{m}, and \code{R} or \code{M}.  The popuation estimate for each method is returned from \code{summary}.
#'
#' Confidence intervals for the initial population size using multiple census methods can be constructed using two different distributions (normal, Poisson) for the Schnabel method or the normal distribution for the Shumacher-Eschmeyer method as chosen with \code{type=}.  If \code{citype="suggested"} then the type of confidence interval suggested by the rules in Seber (2002) is used.  If \code{type="Schnabel"} and \code{type="normal"} then the standard error for the inverse of the population estimate is compute as the square root of eqn 2.11 from Krebs (1989) or eqn 3.16 from Ricker (1975).  If \code{type="SchumacherEschmeyer"} then the standard error for the inverse of the population estimate is computed with eqn 2.14 from Krebs (1989) (Note that the divisor in Krebs (1989) is different than the divisor in Ricker (1975) eqn 3.13, but consistent with eqn 4.17 in Seber (2002)).  The confidence intervals when \code{type="normal"} are constructed by inverting a confidence interval for the inverse population estimate.  The confidence interval for the inverse population estimate is constructed from the inverse population estimate plus/minus a t critical value times the standard error for the inverse population estimate.  The t critical value uses the number of samples minus 1 when \code{type="Schnabel"} and the number of samples minus 2 when \code{type="SchumacherEschmeyer"} according to p. 32 of Krebs (1989) (note that this is different than what Ricker (1975) does).  Note that confidence intervals for the population size when \code{type="normal"} may contain negative values (for the upper value) when the population estimate is relatively large and the number of samples is small (say, three) because the intervals are orginally constructed on the inverted population estimate and they use the t-distribution. 
#' 
#' The \code{plot} can be used to identify assumption violations in the Schnabel and Schumacher-Eschmeyer methods (an error will be returned if used with any of the other methods).  If the assumptions ARE met then the plot of the proportion of marked fish in a sample versus the cumulative number of marked fish should look linear.  A loess line (with approximate 95% confidence bands) can be added to aid interpretation with \code{loess=TRUE}.  Note, however, that adding the loess line may return a number of warning or produce a non-informative if the number of samples is small (<8).
#'
#' @aliases mrClosed summary.mrClosed confint.mrClosed plot.mrClosed
#'
#' @param M A numeric representing the number of marked fish from the first sample (single-census), an object from \code{capHistSum()} (single- or multiple-census), or numeric vector of marked fish prior to ith samples (multiple-census).
#' @param n A numeric representing the number of captured fish in the second sample (single-census) or numeric vector of captured fish in ith sample (multiple-census).
#' @param m A numeric representing the number of recaptured (marked) fish in the second sample (single-census) or numeric vector of recaptured (marked) fish
#'in ith sample (multiple-census).
#' @param R A numeric vector representing the number of marked fish returned to the population (multiple-census).  Note that several references use the number of \dQuote{new} marks returned to the population rather than the \dQuote{total} number of marked returned to the population that is used here.
#' @param type A single string that identifies the type of calculation method to use in the main function or the method to use when constructing confidence intervals in \code{confint}. See details.
#' @param labels A character or character vector used to label the rows of the resulting output matrix when using a single census method separated by groups.  Must be the same length as \code{M}, \code{n}, and \code{m}.  Defaults to upper-case letters if no values are given.
#' @param chapman.mod A logical that represents whether the Chapman modification method should be used (\code{=TRUE}, default) or not (\code{=FALSE}) when performing the Schnabel multiple census method.
#' @param object An object saved from the \code{mrClosed} call (i.e., of class \code{mrClosed}).
#' @param x An object saved from the \code{mrClosed} call when using \code{type="Schnabel"} or \code{type="SchumacherEschmeyer"} (i.e., of class \code{mrClosed}).
#' @param digits The number of decimal digits to round the population estimates to.  If \code{incl.SE=TRUE} then SE will be rounded to one more decimal place then given in \code{digits}.
#' @param incl.SE A logical that indicates whether the results should include the SE calculation.  See details.
#' @param incl.all A logical that indicates whether an overall population estimate should be computed when using a single census method that has been separated into sub-groups.  See details.
#' @param incl.inputs A logical that indicates whether a reminder of the inputted values and what type of method was used should be printed with the summary and confidence interval results.
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names.  If missing, all parameters are considered.
#' @param level Same as \code{conf.level} but used for compatability with generic \code{confint} function.
#' @param conf.level A numeric representing the level of confidence to use for constructing confidence intervals.
#' @param bin.type A string that identifies the method used to construct binomial confidence intervals (default is \code{"wilson"}).  This is only used if \code{citype="binomial"}.  See details of \code{binCI}
#' @param pch A numeric used to indicate the type of plotting character.
#' @param col.pt a string used to indicate the color of the plotted points.
#' @param xlab A label for the x-axis (\code{"Age"} is the default).
#' @param ylab A label for the y-axis (\code{"log(Catch)"} is the default).
#' @param loess A logical that indicates if a loess smoother line is fit to and shown on plot.
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
#'    \item type The type of method used (provided by the user).
#'    \item meth A label for the type of method used.
#'    \item N The estimated initial population size.
#'    \item labels Labels for the rows of summary matrix.
#'  }
#'  
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @seealso \code{\link{capHistSum}}, \code{\link{poiCI}}, \code{\link{binCI}}, \code{\link{hyperCI}}, and \code{\link{mrOpen}}; See \code{SunfishIN} in \pkg{FSAdata} for an example to test matching of results with Ricker (1975)'  See \code{mrN.single} and \code{schnabel} in \pkg{fishmethods} for similar functionality.
#' 
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/MRClosed.pdf}
#' 
#' @references Krebs, C.J.  1989.  Ecological Methodology.  Addison-Welsey Educational Publishing.
#'
#' Ricker, W.E. 1975. \href{http://www.dfo-mpo.gc.ca/Library/1485.pdf}{Computation and interpretation of biological statistics of fish populations}. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.
#'
#' Seber, G.A.F. 2002. The Estimation of Animal Abundance and Related Parameters. Edward Arnold, second edition.
#'
#' Schnabel, Z.E.  1938. The estimation of the total fish population of a lake. American Mathematician Monthly, 45:348-352.
#'
#' Schumacher, F.X. and R.W. Eschmeyer. 1943.  The estimation of fish populations in lakes and ponds.  Journal of the Tennessee Academy of Sciences, 18:228-249.
#'
#' @keywords manip
#'
#' @examples
#' ## Single census with no sub-groups
#' # Petersen estimate -- the default
#' mr1 <- mrClosed(346,184,49)
#' summary(mr1)
#' summary(mr1,incl.inputs=TRUE)
#' confint(mr1)
#' confint(mr1,incl.inputs=TRUE)
#' confint(mr1,type="hypergeometric")
#'
#' # Chapman modification of the Petersen estimate
#' mr2 <- mrClosed(346,184,49,type="Chapman")
#' confint(mr2)
#'
#' # Chapman estimate showing the SE
#' summary(mr2,incl.SE=TRUE)
#' summary(mr2,incl.SE=TRUE,digits=1)
#'
#' ## Single census, using capHistSum() results
#' # data in capture history format
#' data(BluegillJL)
#' str(BluegillJL)
#' ch1 <- capHistSum(BluegillJL)
#' mr3 <- mrClosed(ch1)
#' summary(mr3)
#' confint(mr3)
#'
#' ## Single census with sub-groups
#' marked <- c(93,35,72,16,46,20)
#' captured <- c(103,30,73,17,39,18)
#' recaps <- c(20,23,52,15,35,16)
#' lbls <- c("YOY","Juvenile","Stock","Quality","Preferred","Memorable")
#' mr4 <- mrClosed(marked,captured,recaps,type="Ricker",labels=lbls)
#' summary(mr4)
#' summary(mr4,incl.SE=TRUE)
#' summary(mr4,incl.SE=TRUE,incl.all=TRUE)
#' summary(mr4,incl.SE=TRUE,incl.all=TRUE,incl.inputs=FALSE)
#' confint(mr4)
#'
#' ## Multiple Census
#' #Data in summarized form
#' data(PikeNY)
#'
#' # Schnabel method
#' mr5 <- with(PikeNY,mrClosed(n=n,m=m,R=R,type="Schnabel"))
#' plot(mr5)
#' plot(mr5,loess=TRUE)
#' summary(mr5)
#' confint(mr5)
#'
#' # Schumacher-Eschmeyer method
#' mr6 <- with(PikeNY,mrClosed(n=n,m=m,R=R,type="Schumacher"))
#' summary(mr6)
#' confint(mr6)
#'
#' ## Capture history data summarized by capHistSum()
#' data(PikeNYPartial1)
#' ch2 <- capHistSum(PikeNYPartial1,cols=-1)  # ignore first column of ID numbers
#'
#' # Schnabel method
#' mr7 <- mrClosed(ch2,type="Schnabel")
#' plot(mr7,loess=TRUE)
#' summary(mr7)
#' confint(mr7)
#'
#' @rdname mrClosed
#' @export
mrClosed <- function(M=NULL,n=NULL,m=NULL,R=NULL,
            type=c("Petersen","Chapman","Ricker","Bailey","Schnabel","SchumacherEschmeyer"),
            labels=NULL,chapman.mod=TRUE) {
  type <- match.arg(type)
  if (type %in% c("Petersen","Chapman","Ricker","Bailey")) {
    if (!is.null(R)) warning("'R' not used in single census methods.  It will be ignored.",call.=FALSE)
    iMRCSingle(M,n,m,type,labels)
  } else iMRCMultiple(M,n,m,R,type,chapman.mod)
}

iMRCSingle <- function(M,n,m,type,labels) { ## INTERNAL Single Census (Petersen, Chapman, Ricker, or Bailey)
  # initial checks
  if (is.null(M)) stop("Missing 'M'.",call.=FALSE)
  if (class(M)=="CapHist") {
    if (!is.null(m) | !is.null(n)) warning("'m' and 'n' ignored when 'M' from capHistSum().",call.=FALSE)
    m <- M$sum$m
    n <- M$sum$n
    M <- M$sum$M    
  } else {
    if (is.null(n) | is.null(m)) stop("One or both of 'n' or 'm' is missing without 'M' from capHistSum().",call.=FALSE)
    # Make sure that the vectors are of the same size
    lengths <- c(length(M),length(n),length(m))
    if (any(diff(lengths)!=0)) stop("'M', 'n', or 'm' vectors must have same length.",call.=FALSE)
  }
  # Make sure that recapture number makes sense relative to sample size
  if (any((n-m)<0)) {
    if (length(n)==1) stop ("Can't have more recaptures (m) then total individuals (n).",call.=FALSE)
    else stop(paste("Row",which((n-m)<0),"has more recaptures (m) then total individuals (n)."),call.=FALSE)
  } 
  # If no labels provided then assign letters as labels, if labels provided then make sure size is correct
  if (is.null(labels)) {
    if (length(M)>1) labels=LETTERS[1:length(M)]
  } else {
    if (length(M) != length(labels)) stop("'labels' must have same length as 'M', 'n', and 'm'.")
  }
  # handle modifications for simplicity of calculation below  
  switch(type,
         Petersen={meth="the 'naive' Petersen method"
                   M1 <- M; n1 <- n; m1 <- m; cf <- rep(0,length(M)) },
         Chapman={meth="Chapman's modification of the Petersen method"
                  M1 <- M+1; n1 <- n+1; m1 <- m+1; cf <- rep(1,length(M)) },
         Ricker={meth="Ricker's modification of the Petersen method"
                 M1 <- M+1; n1 <- n+1; m1 <- m+1; cf <- rep(0,length(M)) },
         Bailey={meth="Bailey's modification of the Petersen method"
                 M1 <- M; n1 <- n+1; m1 <- m+1; cf <- rep(0,length(M)) }
  ) # end switcc
  # perform calculations and save all of the intermediate results to return
  res <- list(M=M,n=n,m=m,M1=M1,n1=n1,m1=m1,cf=cf,N=M1*n1/m1-cf,labels=labels,type=type,meth=meth)
  class(res) <- "mrClosed"
  res
} ## end iMRCSingle

iMRCMultiple <- function(M,n,m,R,type,chapman.mod) { ## INTERNAL Multiple Census (Schnabel or Shumacher-Eschmeyer)
  # Initial Checks
  if (!is.null(M)) {
    if (class(M)=="CapHist") {
      n <- M$sum$n
      m <- M$sum$m
      R <- M$sum$R
      M <- cumsum(R-m)-(R-m)
    } else {
      if (is.null(n) | is.null(m)) stop("One or both of 'n' or 'm' is missing without 'M' from capHistSum().",call.=FALSE)
      else if (!is.null(R)) warning("Only need one of 'M' or 'R'.  'R' is ignored.",call.=FALSE)
      R <- n
      R[length(R)] <- 0
    }
  } else {
    if (is.null(R)) stop("One of 'M' or 'R' must be supplied by user",call.=FALSE)
    if (is.null(n) | is.null(m)) stop("One or both of 'n' or 'm' is missing.",call.=FALSE)
    M <- cumsum(R-m)-(R-m)
  }
  # calculate intermediate values
  sum.m <- sum(m)
  sum.nM <- sum(n*M)
  sum.nM2 <- sum(n*(M^2))
  sum.mM <- sum(m*M)
  sum.m2dn <- sum((m^2)/n)
  # perform the estimates
  switch(type,
         Schnabel={
           meth <- "the Schnabel method"     
           ifelse(chapman.mod, N <- sum.nM/(sum.m+1), N <- sum.nM/sum.m) },
         Schumacher=,SchumacherEschmeyer={
           meth <- "the Schumacher-Eschmeyer method"
           chapman.mod <- FALSE
           N <- sum.nM2/sum.mM }
  ) # end switch
  # return the results and intermediate calculations
  res <- list(n=n,m=m,R=R,M=M,N=N,sum.m=sum.m,sum.nM=sum.nM,sum.nM2=sum.nM2,sum.mM=sum.mM,sum.m2dn=sum.m2dn,labels=NULL,type=type,meth=meth,chapman.mod=chapman.mod)
  class(res) <- "mrClosed"
  res
} ## end iMRCMultiple


#' @rdname mrClosed
#' @export
summary.mrClosed <- function(object,digits=0,incl.SE=FALSE,incl.all=FALSE,incl.inputs=FALSE,...) {
  # Put descriptive label of input values at top of output if the user asked for it.
  if(incl.inputs) {
    if (object$type %in% c("Petersen","Chapman","Ricker","Bailey")) {
      if (is.null(object$labels)) message("Used ",object$meth," with M=",object$M,", n=",object$n,", and m=",object$m,".\n",sep="")
      else {
        message("Used ",object$meth," with observed inputs of:\n",sep="")
        message(paste(object$labels,"- M=",object$M,", n=",object$n,", and m=",object$m,".\n",sep=""))
      }
    } else {
      msg <- paste("Used ",object$meth,sep="")
      ifelse(object$chapman.mod,msg <- paste(msg,"with Chapman modification.\n"),msg <- paste(msg,".\n",sep=""))
      message(msg)
    }
  }
  # Put the PE into a vector to return
  res <- cbind(round(object$N,digits))
  colnames(res) <- "N"
  # Add the SE to the return vector if the user asked for it (and it can be calculated)
  if (incl.SE) {
    if (object$type == "Petersen") V <- with(object, (M^2)*n*(n-m)/(m^3) )
    else if (object$type == "Chapman") V <- with(object, M1*n1*(M1-m1)*(n1-m1)/((m1^2)*(m1+1)) )
    else if (object$type %in% c("Ricker","Bailey")) V <- with(object, ((M1^2)*n1*(n-m))/((m1^2)*(m1+1)) )
    else warning(paste("A standard error for N cannot be computed with the",object$type,"method."),call.=FALSE)
    res <- cbind(res,round(sqrt(V),digits+1))
    colnames(res)[2] <- "SE"
  }
  # Label rows if labels provided
  rownames(res) <- object$labels
  # Included an overall PE if the user asked for it
  if (incl.all) {
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


#' @rdname mrClosed
#' @export
plot.mrClosed <- function(x,pch=19,col.pt="black",
                          xlab=expression(M[i]),ylab=expression(m[i]%/%n[i]),
                          loess=FALSE,lty.loess=2,lwd.loess=1,
                          col.loess="gray20",trans.loess=10,span=0.9,...) {
  if (!(x$type %in% c("Schnabel","SchumacherEschmeyer"))) stop("Plot only appropriate for 'Schnabel' or 'SchumacherEschmeyer' methods.",call.=FALSE)
  else {
    plot(x$M,x$m/x$n,pch=pch,col=col.pt,xlab=xlab,ylab=ylab,...)
    # add loess line
    if (loess) iAddLoessLine(x$m/x$n,x$M,lty.loess,lwd.loess,col.loess,trans.loess,span=span)
  }
}


#' @rdname mrClosed
#' @export
confint.mrClosed <- function(object,parm=NULL,level=conf.level,conf.level=0.95,digits=0,
                         type=c("suggested","binomial","hypergeometric","normal","Poisson"),
                         bin.type=c("wilson","exact","asymptotic"),incl.inputs=FALSE,...) {
  if(!is.null(parm)) {
    warning("parm argument is meaningless for this class of object; it has been reset to NULL.\n\n",call.=FALSE)
    parm <- NULL
  }
  type <- match.arg(type)
  bin.type <- match.arg(bin.type)
  if (object$type=="Schnabel" & type %in% c("binomial","hypergeometric")) stop("The CI type must be 'suggested', 'normal', or 'Poisson' when using the Schnabel method.",call.=FALSE)
  if (object$type=="SchumacherEschmeyer" & type %in% c("binomial","hypergeometric","Poisson")) stop("The CI type must be 'normal' when using the Schumacher-Eschmeyer method.",call.=FALSE)
  if (object$type %in% c("Petersen","Chapman","Ricker","Bailey")) {
    ci <- NULL
    for (i in 1:length(object$N)) {
      temp <- with(object, list(M=M[i],n=n[i],m=m[i],M1=M1[i],n1=n1[i],m1=m1[i],cf=cf[i],type=type,meth=meth,N=N[i],labels=labels[i]) )
      ci <- rbind(ci,iCI.MRCSingle(temp,conf.level,type,bin.type,incl.inputs,...))
    }
  } else ci <- iCI.MRCMultiple(object,conf.level,type,incl.inputs,...)
  rownames(ci) <- object$labels
  colnames(ci) <- iCILabel(conf.level)
  round(ci,digits)
}


iCIt <- function(est,SE,obsdf,conf.level=0.95) {
  ## Internal function for computing normal theory CIs -- from NCStats
  hw <- qt(1-(1-conf.level)/2,obsdf)*SE
  res <- cbind(est-hw,est+hw)
  colnames(res) <- iCILabel(conf.level)
  res
}


# M/R Closed, Single Census, Only One Population CI
iCI.MRCSingle <- function(object,conf.level,type,bin.type,incl.inputs,...) {
  # Follow Sebers' suggestions if asked to
  if (type=="suggested") {
    if ((object$m/object$n) > 0.10) type <- "binomial"
    else if (object$m > 50) type <- "normal"
    else type <- "Poisson"
  }
  switch(type,
         hypergeometric={
           ci <- hyperCI(object$M,object$n,object$m,conf.level)
         }, 
         binomial={
           # Binomial CI for phat
           ci1 <- binCI(object$m,object$n,conf.level,bin.type)
           # Convert to CI for m1
           ifelse(object$type=="Petersen",m.ci <- ci1*object$n,
                  m.ci <- ci1*object$n+1)
           # Put endpoints back into N formula to get CI for N
           N.bin <- (object$M1*object$n1)/m.ci-object$cf
           ci <- N.bin[2:1]
         }, 
         normal={
           # Find +/- Z for normal CI
           zalpha <- c(-1,1)*abs(qnorm((1-conf.level)/2))
           if (object$type=="Petersen") {
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
           } else {
             # SE from Seber p.60 for Chapman
             # SE from Ricker, eqn 3.8, p.78 for Ricker and Seber p.61 for Bailey
             ifelse(object$type=="Chapman",SE <- with(object, sqrt(M1*n1*(M1-m1)*(n1-m1)/((m1^2)*(m1+1))) ),  
                    SE <- with(object, sqrt(((M1^2)*n1*(n-m))/((m1^2)*(m1+1))) ) )  
             # CI for N
             ci <- rbind(object$N+zalpha*SE)
           }
         },
         Poisson={
           # Poisson CI for m
           m.ci <- poiCI(object$m,conf.level)
           # Convert to CI for m1
           if (object$type!="Petersen") m.ci <- m.ci+1
           # Put endpoints back in N formula to get CI for N
           N.poi <- (object$M1*object$n1)/m.ci-object$cf    
           ci <- N.poi[,2:1]
         }
  )
  # Put message at top of output if asked for
  if (incl.inputs) {
    msg <- paste("The ",type," method was used.\n")
    if (!is.null(object$labels)) msg <- paste(object$labels,"-",msg)
    message(msg)
  }
  # Show the CIs
  ci
} # end iCI.MRCSingle internal function

# M/R Closed, Multiple Census CI
iCI.MRCMultiple <- function(object,conf.level,type,incl.inputs,...) {  
  # Follow Seber's suggestions if asked for
  if (type=="suggested") {
    if (object$type=="SchumacherEschmeyer") type <- "normal"
    else if (object$sum.m < 50) type <- "Poisson"
    else type <- "normal"
  }
  if (object$type=="Schnabel") {
    if (type=="normal") {
      # Get df (from Krebs p. 32)
      df <- length(object$n)-1
      # Compute SE for inverse of N (from Krebs 2.11, Ricker 3.16)
      ifelse(object$chapman.mod, invN.SE <- with(object, sqrt((sum.m+1)/(sum.nM^2)) ),
             invN.SE <- with(object, sqrt(sum.m/(sum.nM^2)) ) )
      # Compute CI for inverse of N
      invN.ci <- iCIt(1/object$N,invN.SE,df)
      # Invert to get CI for N
      ci <- rbind((1/invN.ci)[2:1])
    } else {
      # Get Poisson CI
      ci1 <- poiCI(object$sum.m,conf.level)
      # Coeffect if chapman modification was used
      ifelse(object$chapman.mod,N.poi <- object$sum.nM/(ci1+1),
             N.poi <- object$sum.nM/ci1)
      ci <- rbind(N.poi[2:1])
    }
  } else {
    # Get df (from from Krebs p. 32)
    df <- length(object$n)-2
    # Compute SE for inverse of N (from Krebs 2.14)
    invN.SE <- with(object, sqrt(((sum.m2dn-(sum.mM^2)/sum.nM2)/df)/(sum.nM2)) )
    # Compute CI for inverse of N
    invN.ci <- iCIt(1/object$N,invN.SE,df)
    # Invert to get CI for N
    ci <- rbind((1/invN.ci)[2:1])
  }
  # Put message at top of output if asked for
  if (incl.inputs) message("The ",type," method was used.\n")
  # Show the CIs
  ci
} # end iCI.MRCMultiple internal function

