#' @title Population estimates for k-, 3-, or 2-pass removal data.
#'
#' @description Computes estimates, with confidence intervals, of the population size and probability of capture from the number of fish removed in k-, 3-, or 2-passes in a closed population.
#'
#' @details The main function computes the estimates and associated standard errors, if possible, for the initial population size, No, and probability of capture, p, for seven methods chosen with \code{method=}.  The possible methods are:
#'  \itemize{
#'    \item \code{method="CarleStrub"}: The general weighted k-pass estimator proposed by Carle and Strub (1978).  This function iteratively solves for No in equation 7 of Carle and Strub (1978).
#'    \item \code{method="Zippin"}: The general k-pass estimator generally attributed to Zippin.  This function iteratively solves for No in bias corrected version of equation 3 (page 622) of Carle and Strub (1978).  These results are not yet trustworthy.
#'    \item \code{method="Seber3"}: The special case for k=3 estimator shown in equation 7.24 of Seber(2002).
#'    \item \code{method="Seber2"}: The special case for k=2 estimator shown on page 312 of Seber(2002).
#'    \item \code{method="RobsonRegier2"}: The special case for k=2 estimator shown by Robson and Regier (1968).
#'    \item \code{method="Moran"}: The likelihood method of Moran (1951) as implemented by Schnute (1983).
#'    \item \code{method="Schnute"}: The likelihood method of Schnute (1983) for the model that has a different probability of capture for the first sample but a constant probability of capture for all ensuing samples..
#'  }
#'
#' Confidence intervals for the first five methods are computed using standard large-sample normal distribution theory.  Note that the confidence intervals for the 2- and 3-pass special cases are only approximately correct if the estimated population size is greater than 200.  If the estimated population size is between 50 and 200 then a 95\% CI behaves more like a 90\% CI.
#' 
#' Confidence intervals for the last two methods use likelihood ratio theory as described in Schnute (1983) and are only produced for the No parameter.  Standard errors are not produced with the Moran or Schnute methods..
#'
#' In the Carle Strub method, if the resultant No estimate is equal to the sum of the catches (T) then the estimate of No that is returned will be the sum of the catches.  In this instance, and if the \dQuote{Seber} method of computing the standard error is used, then the SE will not be estimable and the confidence intervals can not be constructed.
#'
#' @param catch A numerical vector of catch at each pass.
#' @param method A single string that identifies the removal method to use.  See details.
#' @param alpha A single numeric value for the alpha parameter in the CarleStrub method (default is \code{1}).
#' @param beta A single numeric value for the beta parameter in the CarleStrub method (default is \code{1}).
#' @param CS.se A single string that identifies whether the SE in the CarleStrub method should be computed according to Seber or Zippin.
#' @param object An object saved from \code{removal()}.
#' @param parm A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names.  If missing, all parameters are considered.
#' @param level Note used, but her for compatability with generic \code{confint} function.
#' @param conf.level A single number representing the level of confidence to use for constructing confidence intervals.  This is sent in the main \code{removal} function rather than \code{confint}.
#' @param just.ests A logical that indicates whether just the estimates (\code{=TRUE}) or the return list (\code{=FALSE}; default; see below) is returned.
#' @param verbose A logical that indicates whether descriptive labels should be printed from \code{summary} and if certain warnings are shown with \code{confint}.
#' @param digits A single numeric that controls the number of decimals in the output from \code{summary} and \code{confint}.
#' @param Tmult A single numeric that will be multiplied by the total catch in all samples to set the upper value for the range of population sizes when minimizing the log-likelihood and creating confidence intervals for the Moran and Schnute method.  Large values are much slower to compute, but too low of value can result in missing the best estimate.
#' @param \dots Additional arguments for methods.
#'
#' @return A vector that contains the estimates and standard errors for No and p if \code{just.ests=TRUE} or (default) a list with at least the following items:
#'  \itemize{
#'    \item catch The original vector of observed catches.
#'    \item method The method used (provided by the user).
#'    \item lbl A descriptive label for the method used.
#'    \item est A matrix that contains the estimates and standard errors for No and p.
#'  }
#'  
#' In addition, if the Moran or Schnute methods are used the list will also contain
#'  \itemize{
#'    \item min.nlogLH The minimum value of the negative log-likelihood function.
#'    \item Tmult The Tmult value sent by the user.
#'  }
#' 
#' @section testing: The Carle-Strub method matches the examples in Carle and Strub (1978) for No, p, and the variance of No.  The Carle-Strub estimates of No and p match the examples in Cowx (1983) but the SE of No does not.  The Carle-Strub estimates of No match the results (for estimates that they did not reject) from Jones and Stockwell (1995) to within 1 individual in most instances and within 1\% for all other instances (e.g., off by 3 individuals when the esitmate was 930 individuals).
#' 
#' The Seber3 results for No match the results in Cowx (1983).
#' 
#' The Seber2 results for No, p, and the SE of No match the results in example 7.4 of Seber (2002) and in Cowx (1983).
#' 
#' The RobsonRegier2 results for No and the SE of NO match the resultsin Cowx (1983)
#' 
#' The Zippin method results do not match the examples in Seber (2002) or Cowx (1983) because \code{removal} uses the bias-corrected version from Carle and Strub (1978) and does not use the tables in Zippin (1958).  The Zippin method is not yet trustworthy.
#' 
#' The Moran and Schnute methods match the examples in Schnute (1983) perfectly for all point estimates and within 0.1 units for all confidence intervals.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 10-Abundance from Depletion Data.
#'
#' @seealso See \code{\link{depletion}} for related functionality.
#' 
#' @references Ogle, D.H.  2016.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Carle, F.L. and M.R. Strub. 1978. A new method for estimating population size from removal data.  Biometrics, 34:621-630.
#'
#' Cowx, I.G.  1983.  Review of the methods for estimating fish population size from survey removal data.  Fisheries Management, 14:67-82.
#' 
#' Moran, P.A.P.  1951.  A mathematical theory of animal trapping.  Biometrika 38:307-311.
#'
#' Robson, D.S., and H.A. Regier.  1968.  Estimation of population number and mortality rates.  pp. 124-158 in Ricker, W.E. (editor) Methods for Assessment of Fish Production in Fresh Waters.  IBP Handbook NO. 3 Blackwell Scientific Publications, Oxford.
#' 
#' Schnute, J.  1983.  A new approach to estimating populations by the removal method.  Canadian Journal of Fisheries and Aquatic Sciences, 40:2153-2169.
#'
#' Seber, G.A.F. 2002. The Estimation of Animal Abundance. Edward Arnold, second edition (Reprint).
#' 
#' van Dishoeck, P.  2009.  Effects of catchability variation on performance of depletion estimators: Application to an adaptive management experiment.  Masters Thesis, Simon Fraser University.  [Was (is?) from http://rem-main.rem.sfu.ca/theses/vanDishoeckPier_2009_MRM483.pdf.]
#' 
#' @keywords manip
#'
#' @aliases removal summary.removal confint.removal
#'
#' @examples
#' ## First example -- 3 passes
#' ct3 <- c(77,50,37)
#'
#' # Carle Strub (default) method
#' p1 <- removal(ct3)
#' summary(p1)
#' summary(p1,verbose=TRUE)
#' summary(p1,parm="No")
#' summary(p1,parm="p")
#' confint(p1)
#' confint(p1,parm="No")
#' confint(p1,parm="p")
#' 
#' # Moran method
#' p2 <- removal(ct3,method="Moran")
#' summary(p2,verbose=TRUE)
#' confint(p2,verbose=TRUE)
#'#'
#' # Schnute method
#' p3 <- removal(ct3,method="Schnute")
#' summary(p3,verbose=TRUE)
#' confint(p3,verbose=TRUE)
#'
#'
#' ## Second example -- 2 passes
#' ct2 <- c(77,37)
#'
#' # Seber method
#' p4 <- removal(ct2,method="Seber2")
#' summary(p4,verbose=TRUE)
#' confint(p4)
#'
#'
#' ### Test if catchability differs between first sample and the other samples
#' # chi-square test statistic from  negative log-likelihoods
#' #   from Moran and Schnute fits (from above)
#' chi2.val <- 2*(p2$min.nlogLH-p3$min.nlogLH)
#' # p-value ... no significant difference
#' pchisq(chi2.val,df=1,lower.tail=FALSE)
#'
#' # Another LRT example ... sample 1 from Schnute (1983)
#' ct4 <- c(45,11,18,8)
#' p2a <- removal(ct4,method="Moran")
#' p3a <- removal(ct4,method="Schnute")
#' chi2.val <- 2*(p2a$min.nlogLH-p3a$min.nlogLH)  # 4.74 in Schnute(1983)
#' pchisq(chi2.val,df=1,lower.tail=FALSE)         # significant difference (catchability differs)
#' summary(p3a)
#' 
#' 
#' ### Using lapply() to use removal() on many different groups
#' ###   with the removals in a single variable ("long format")
#' ## create a dummy data frame
#' lake <- factor(rep(c("Ash Tree","Bark","Clay"),each=5))
#' year <- factor(rep(c("2010","2011","2010","2011","2010","2011"),times=c(2,3,3,2,2,3)))
#' pass <- factor(c(1,2,1,2,3,1,2,3,1,2,1,2,1,2,3))
#' catch <- c(57,34,65,34,12,54,26,9,54,27,67,34,68,35,12)
#' d <- data.frame(lake,year,pass,catch)
#'
#' ## create a variable that indicates each different group
#' d$group <- with(d,interaction(lake,year))
#' d
#' ## split the catch by the different groups (creates a list of catch vectors)
#' ds <- split(d$catch,d$group)
#' ## apply removal() to each catch vector (i.e., different group)
#' res <- lapply(ds,removal,just.ests=TRUE)
#' res <- data.frame(t(data.frame(res,check.names=FALSE)))
#' ## get rownames from above and split into separate columns
#' nms <- t(data.frame(strsplit(rownames(res),"\\.")))
#' attr(nms,"dimnames") <- NULL
#' fnl <- data.frame(nms,res)
#' ## put names together with values
#' rownames(fnl) <- NULL
#' colnames(fnl)[1:2] <- c("Lake","Year")
#' fnl
#'
#'
#' ### Using apply() to use removal() on many different groups
#' ###   with the removals in several variables ("wide format")
#' ## create a dummy data frame (just reshaped from above as
#' ## an example; -5 to ignore the group variable from above)
#' d1 <- reshape(d[,-5],timevar="pass",idvar=c("lake","year"),direction="wide")
#' ## apply restore() to each row of only the catch data
#' res1 <- apply(d1[,3:5],MARGIN=1,FUN=removal,method="CarleStrub",just.ests=TRUE)
#' res1 <- data.frame(t(data.frame(res1,check.names=FALSE)))
#' ## add the grouping information to the results
#' fnl1 <- data.frame(d1[,1:2],res1)
#' ## put names together with values
#' rownames(fnl1) <- NULL
#' fnl1
#' 
#' @rdname removal
#' @export
removal <- function(catch,
                    method=c("CarleStrub","Zippin","Seber3","Seber2",
                             "RobsonRegier2","Moran","Schnute"),
                    alpha=1,beta=1,CS.se=c("Zippin","alternative"),
                    conf.level=0.95,just.ests=FALSE,Tmult=3) {
  # some initial checks
  method <- match.arg(method)
  if (conf.level<=0 | conf.level>=1) stop("'conf.level' must be between 0 and 1",call.=FALSE)
  if (!is.vector(catch)) {
    # if a one row or column matrix then convert to a vector
    if ((is.matrix(catch) | is.data.frame(catch)) & (nrow(catch)==1 | ncol(catch)==1)) {
      catch <- as.numeric(catch)
    } else {
      # otherwise send an error
      stop("'catch' must be a vector.",call.=FALSE)
    }
  } 
  if (any(is.na(catch))) {
    warning("'NA's removed from 'catch' to continue.",call.=FALSE)
    catch <- catch[!is.na(catch)]
  }  
  if (length(catch)<2) stop("Cannot perform calculations with one catch value.",call.=FALSE)
  if (Tmult<1) stop("'Tmult' should be greater than 1.",call.=FALSE)
  # intermediate calculations
  # Different methods
  switch(method,
    Zippin=        { tmp <- iZippin(catch,conf.level) },
    CarleStrub=    { tmp <- iCarleStrub(catch,conf.level,alpha,beta,CS.se) },
    Seber3=        { tmp <- iSeber3(catch,conf.level) },
    Seber2=        { tmp <- iSeber2(catch,conf.level) },        
    RobsonRegier2= { tmp <- iRobsonRegier2(catch,conf.level) },
    Moran=         { tmp <- iMoran(catch,conf.level,Tmult) },
    Schnute=       { tmp <- iSchnute(catch,conf.level,Tmult) }
  )
  if (just.ests) { tmp <- tmp$est }
  else {
    tmp <- c(tmp,method=method,conf.level=conf.level)
    class(tmp) <- "removal"
  }
  # return object
  tmp
}

#=============================================================
# INTERNAL -- Calculate some intermediate values
#=============================================================
iRemovalKTX <- function(catch) {
  # number of samples
  k <- length(catch)
  # total catch
  T <- sum(catch)
  # cumulative catch prior to ith sample
  #   same as sum(cumsum(catch)[-k]) as used in Schnute (1983)
  i <- 1:k
  X <- sum((k-i)*catch)
  # return named vector
  c(k=k,T=T,X=X)
}

#=============================================================
# INTERNAL -- Calculate CIs with normal theory
#=============================================================
iRemovalNCI <- function(est,se,conf.level) {
  est+c(-1,1)*stats::qnorm(0.5+conf.level/2)*se
}

#=============================================================
# INTERNAL -- Calculate CIs with likelihood theory for the
#             Moran and Schnute methods.
#=============================================================
iRemovalLHCI <- function(method,catch,conf.level,k,T,X,min.nlogLH,Tmult){ 
  ## critical negative log-likelihood value
  nlogLHcrit <- min.nlogLH+stats::qchisq(conf.level,df=1)/2
  
  ## Determine if the upper limit is going to fail.
  if (method=="Moran") {
    # Modified catches to eliminates zero catches -- required
    #   because formulas below uses log()s.
    mod.catch <- catch[catch>0]
    # This is (Schnute (1983) equation 3.6
    nlogLHfail <- lfactorial(T)-T*log(T)+T*sum(mod.catch*log(k*mod.catch/T))
  } else {
    # Modified catches to eliminates first catch (per Schnute)
    #   and zero catches as descirbed above
    mod.catch <- catch[-1]
    mod.catch <- mod.catch[mod.catch>0]
    # This is (Schnute (1983) equation 3.11
    nlogLHfail <- lfactorial(T)-T*log(T)+T+sum(mod.catch*log(((k-1)*mod.catch)/(T-catch[1])))
  }
  # logical for whether the upper limit fails (TRUE) or not
  #   if it does then set UCI to infinity (per Schnute)
  UCIfail <- nlogLHfail<nlogLHcrit
  if (UCIfail) {
    UCImsg <- "An upper confidence value for 'No' cannot be determined."
    UCI <- Inf
  } else UCImsg=NA
  
  ## Determine if the lower limit should be the total catch
  # find negative log LH at total catch (internal functions are further below)
  if (method=="Moran") {
    tmp <- iLHMoran(T,catch,k=k,T=T,XX=X)
  } else {
    tmp <- iLHSchnute(T,catch,k=k,T=T,X=X)
  }
  # if nlLH is the less than critical value then fail (logical will be true)
  #   if it is then set LCI to total catch (per Schnute)
  LCIfail <- tmp<nlogLHcrit
  if (LCIfail) {
    LCImsg <- "The lower confidence value for 'No' has been set at the total catch."
    LCI <- T
  } else LCImsg <- NA
  
  ## Find LCI or UCI if need be (i.e., at least one did not fail above) 
  if (!LCIfail | !UCIfail) {
    # create a vector (made as a matrix to use apply() below)
    #   of N values to compute the negative log likelihood at
    Ntrys <- matrix(seq(T,ceiling(Tmult*T),0.02),ncol=1)
    # compute the nlLH at all those values
    nlogLHvals <- numeric(nrow(Ntrys))
    if (method=="Moran") { nlogLHvals <- apply(Ntrys,MARGIN=1,FUN=iLHMoran,catch=catch,k=k,T=T,XX=X) }
      else { nlogLHvals <- apply(Ntrys,MARGIN=1,FUN=iLHSchnute,catch=catch,k=k,T=T,XX=X) }
    # find which values are less than the critical negative log LH
    #   value and then find the first and last position
    tmp <- range(which(nlogLHvals<=nlogLHcrit))
    # If the last position in the last N tried then the Ns
    #   tried were probably not adequate.  Tell the user to
    #   up the Tmult value.
    if (max(tmp)==length(Ntrys) & !UCIfail) warning("Upper confidence value is ill-formed; try increasing 'Tmult' in 'removal()'.",call.=FALSE)
    # The LCI is the N at the lower position, UCI is the N at
    #   the higher position.
    if (!LCIfail) LCI <- Ntrys[tmp[1]]
    if (!UCIfail) UCI <- Ntrys[tmp[2]]
  }
  # return value rounded to one decimal place
  list(CI=round(c(No.LCI=LCI,No.UCI=UCI),1),LCImsg=LCImsg,UCImsg=UCImsg)
}

#=============================================================
# INTERNAL -- Calculate Moran estimates
#=============================================================
## Moran negative log-likelihood function
iLHMoran <- function(N,catch,k,T,XX) {
  # Estimated q (Schnute (1983) equation 3.2)
  #   Note sum(Ti[-k]) in Schnute is XX (to play well with apply()) here
  ##   Note q, Tk in Schnute are p, T here
  p_hat <- T/(k*N-XX)
  # Predicted catches and total catches
  i <- seq(1,k)
  ct_hat <- N*p_hat*(1-p_hat)^(i-1)                 # Schnute (1983) equation 1.8
  Ti_hat <- cumsum(ct_hat)                          # Schnute (1983) equation 1.1
  T_hat <- Ti_hat[k]
  # negative log-likelihood (Schnute (1983) G(Z) and H(Z) (no K)
  #   from equation 2.6).  The [catch>0] solves issues with
  #   when a catch=0
  N*log(N)-T*log(T)-(N-T)*log(N-T_hat)-log(choose(N,T))+sum(catch[catch>0]*log(catch[catch>0]/ct_hat[catch>0]))
}

iMoran <- function(catch,conf.level,Tmult) {
  ## Follows methodology described at the bottom of page 2157 in Schnute (1983)
  ##   Note sum(Ti[-k]) in Schnute is X here
  ##   Note q, Tk in Schnute are p, T here
  # Intermediate Calculations
  int <- iRemovalKTX(catch)
  k <- int[["k"]] 
  T <- int[["T"]]
  X <- int[["X"]]
  # A check
  if (k<3) stop("The Moran method requires at least three samples.",call.=FALSE)
  # optimize for N
  tmp <- stats::optimize(iLHMoran,c(T,ceiling(Tmult*T)),catch=catch,k=k,T=T,X=X)
  N0 <- tmp$minimum
  p <- T/(k*N0-X)
  # compute confidence intervals for No
  tmpci <- iRemovalLHCI("Moran",catch,conf.level,k,T,X,tmp$objective,Tmult)  
  # return list
  list(est=c(No=N0,No.LCI=tmpci$CI[[1]],No.UCI=tmpci$CI[[2]],p=p),
       catch=catch,min.nlogLH=tmp$objective,Tmult=Tmult,
       LCImsg=tmpci$LCImsg,UCImsg=tmpci$UCImsg,
       lbl="Moran (1951) K-Pass Removal Method")
}

#=============================================================
# INTERNAL -- Calculate Schnute estimates
#=============================================================
## Schnute negative log-likelihood function
iLHSchnute <- function(N,catch,k,T,XX) {
  ##   Note sum(Ti[-k]) in Schnute is XX (to play well with apply()) here
  ##   Note sum(Ti[-k]-catch[1]) in Schnute is XX-(k-1)*catch[1] here
  ##   Note q, Tk in Schnute are p, T here
  # Estimated p's
  p1_hat <- catch[1]/N                                            # Schnute (1983) equation 3.7
  p_hat <- (T-catch[1])/((k-1)*(N-catch[1])-(XX-(k-1)*catch[1]))  # Schnute (1983) equation 3.8
  # Predicted catches and total catches
  ct1_hat <- N*p1_hat                                             # Schnute (1983) equation 1.12
  i <- seq(2,k)
  ct_hat <- N*p_hat*(1-p1_hat)*(1-p_hat)^(i-2)                    # Schnute (1983) equation 1.12
  ct_hat <- c(ct1_hat,ct_hat)
  Ti_hat <- cumsum(ct_hat)
  T_hat <- Ti_hat[k]
  # log-likelihood (Schnute (1983) G(Z) and H(Z) (no K) from equation 2.6)
  #   the [catch>0] solves issues with when a catch=0
  N*log(N)-T*log(T)-(N-T)*log(N-T_hat)-log(choose(N,T))+sum(catch[catch>0]*log(catch[catch>0]/ct_hat[catch>0]))
}

iSchnute <- function(catch,conf.level,Tmult) {
  ## Follows methodology described in Schnute (1983)
  ##   Note sum(Ti[-k]) in Schnute is X here
  ##   Note sum(Ti[-k]-catch[1]) in Schnute is X-(k-1)*catch[1] here
  ##   Note q, Tk in Schnute are p, T here
  # Intermediate Calculations
  int <- iRemovalKTX(catch)
  k <- int[["k"]] 
  T <- int[["T"]]
  X <- int[["X"]]
  # A check
  if (k<3) stop("The Schnute method requires at least three samples.",call.=FALSE)
  # optimize for N
  tmp <- stats::optimize(iLHSchnute,c(T,ceiling(Tmult*T)),catch=catch,k=k,T=T,X=X)
  N0 <- tmp$minimum
  p1 <- catch[1]/N0
  p <- (T-catch[1])/((k-1)*(N0-catch[1])-(X-(k-1)*catch[1]))
  # compute confidence intervals for No
  tmpci <- iRemovalLHCI("Schnute",catch,conf.level,k=k,T=T,X=X,tmp$objective,Tmult)  
  # return list
  list(est=c(No=N0,No.LCI=tmpci$CI[[1]],No.UCI=tmpci$CI[[2]],p=p,p1=p1),
       catch=catch,min.nlogLH=tmp$objective,Tmult=Tmult,
       LCImsg=tmpci$LCImsg,UCImsg=tmpci$UCImsg,
       lbl="Schnute (1983) K-Pass Removal Method w/ Non-constant Initial Catchability")
}

#=============================================================
# INTERNAL -- Calculate Zippin estimates
#=============================================================
iZippinNoVar <- function(N0,p,k) {
  q <- 1-p
  # equation 7.23 (top) in Seber (2002) p.312 (note that k=s)
  (N0*(1-q^k)*(q^k))/(((1-q^k)^2)-((p*k)^2)*q^(k-1))
}

iZippinpVar <- function(N0,p,k) {
  q <- 1-p
  # equation 7.23 (top) in Seber (2002) p.312 (note that k=s)
  (((q*p)^2)*(1-q^k))/(N0*(q*((1-q^k)^2)-((p*k)^2)*(q^k)))
}

iZippin <- function(catch,conf.level) {
  # Get intermediate calculations
  int <- iRemovalKTX(catch)
  k <- int[["k"]]
  T <- int[["T"]]
  X <- int[["X"]]
  # This condition is from equation 6 in Carle & Strub (1978)
  if (X <= (((T-1)*(k-1))/2)-1) {
    warning("Catch data results in Zippin model failure.",call.=FALSE)
    # empty vector of estimates
    tmp <- rep(NA,8)
  } else {
    # Uses modified equation 3 from Carle & Strub (1978) in the while statement
    N0 <- T
    while ((N0+0.5)*((k*N0-X-T)^k) >= (N0-T+0.5)*((k*N0-X)^k) ) { N0 <- N0+1 }
    # capture probability formula from Zippin (1956) according to Sweka (2006)
    p <- T/(k*N0-X)
    # compute variances
    p.var <- iZippinpVar(N0,p,k) 
    N0.var <- iZippinNoVar(N0,p,k)
    # compute CIs
    N0.ci <- iRemovalNCI(N0,sqrt(N0.var),conf.level)
    p.ci <- iRemovalNCI(p,sqrt(p.var),conf.level)
    # vector of estimates
    tmp <- c(N0,sqrt(N0.var),N0.ci,p,sqrt(p.var),p.ci)
  }
  # names to vector of estimates
  names(tmp) <- c("No","No.se","No.LCI","No.UCI","p","p.se","p.LCI","p.UCI")
  # return list
  list(est=tmp,int=int,catch=catch,lbl="Zippin (1956,1958) K-Pass Removal Method")
}

#=============================================================
# INTERNAL -- Calculate Carle-Strub estimates
#=============================================================
iCarleStrub <- function(catch,conf.level,alpha,beta,CS.se=c("Zippin","alternative")) {
  # Some checks
  CS.se <- match.arg(CS.se)
  if (alpha<=0 | beta<=0) stop("'alpha' and 'beta' must be positive.",call.=FALSE)
  # Get intermediate calculations
  int <- iRemovalKTX(catch)
  k <- int[["k"]]
  T <- int[["T"]]
  X <- int[["X"]]
  i <- 1:k
  # Uses equation 7 from Carle & Strub (1978) in the while statement
  N0 <- T
  while (((N0+1)/(N0-T+1))*prod((k*N0-X-T+beta+k-i)/(k*N0-X+alpha+beta+k-i)) >= 1.0) { N0 <- N0+1 }
  p <- T/(k*N0-X)
  p.var <- iZippinpVar(N0,p,k) 
  if (CS.se=="Zippin") {
    # PE variance ... Seber notes that this works under certain conditions
    N0.var <- iZippinNoVar(N0,p,k)
  } else {  
    # Have yet to find a solid reference for this
    N0.var <- (N0*(N0-T)*T)/((T^2)-N0*(N0-T)*(((k*p)^2)/(1-p)))
  }
  # compute CIs
  N0.ci <- iRemovalNCI(N0,sqrt(N0.var),conf.level)
  p.ci <- iRemovalNCI(p,sqrt(p.var),conf.level)
  # vector of estimates
  tmp <- c(N0,sqrt(N0.var),N0.ci,p,sqrt(p.var),p.ci)
  names(tmp) <- c("No","No.se","No.LCI","No.UCI","p","p.se","p.LCI","p.UCI")
  # return list
  list(est=tmp,int=int,CS.prior=c(alpha=alpha,beta=beta),CS.se=CS.se,
       catch=catch,lbl="Carle & Strub (1978) K-Pass Removal Method")
}

#=============================================================
# INTERNAL -- Calculate Seber 3-pass estimates
#=============================================================
iSeber3 <- function(catch,conf.level) {
  # Get intermediate calculations
  int <- iRemovalKTX(catch)
  k <- int[["k"]]
  T <- int[["T"]]
  X <- int[["X"]]
  # Some checks
  if (k!=3) {
    stop("Seber (2002) 3-pass method can only be used with three samples.",call.=FALSE)
  } else if (catch[3] >= catch[1]) {
    warning("Catch data results in model failure for Seber (2002) 3-pass method.",call.=FALSE)
    # empty vector of estimates
    tmp <- rep(NA,8)
  } else {
    # PE estimate (equation 7.24 (top) (p.315) of Seber (2002) ... note that T=Y)
    #   also matches Cowx (1983) equation 4
    N0 <- (6*X^2 - 3*X*T - T^2 + T*sqrt(T^2 + 6*X*T - 3*X^2))/(18*(X-T))
    # capture probability estimate (equation 7.24 (bottom) (p.315) of Seber (2002) ... note that T=Y)
    p <- (3*X - T - sqrt(T^2 + 6*X*T - 3*X^2))/(2*X)
    p.var <- iZippinpVar(N0,p,k) 
    N0.var <- iZippinNoVar(N0,p,k)
    # compute CIs
    N0.ci <- iRemovalNCI(N0,sqrt(N0.var),conf.level)
    p.ci <- iRemovalNCI(p,sqrt(p.var),conf.level)
    # vector of estimates
    tmp <- c(N0,sqrt(N0.var),N0.ci,p,sqrt(p.var),p.ci)
  }
  # names to vector of estimates
  names(tmp) <- c("No","No.se","No.LCI","No.UCI","p","p.se","p.LCI","p.UCI")
  # return list
  list(est=tmp,int=int,catch=catch,lbl="Seber (2002) 3-Pass Removal Method")
}

#=============================================================
# INTERNAL -- Calculate Seber 2-pass estimates
#=============================================================
iSeber2 <- function(catch,conf.level) {
  if (length(catch)!=2) {
    stop("Seber (2002) 2-Pass method can only be used two samples.",call.=FALSE)
  } else if (catch[2] >= catch[1]) {
    warning("Catch data results in model failure for Seber (2002) 2-Pass method.",call.=FALSE)
    # empty vector of estimates
    tmp <- rep(NA,8)
  } else {
    # PE estimate from middle of page 312 in Seber (2002)
    N0 <- catch[1]^2/(catch[1]-catch[2])
    # PE variance from equation 7.30 in Seber (2002)
    N0.var <- ((catch[1]^2)*(catch[2]^2)*(catch[1]+catch[2]))/((catch[1]-catch[2])^4)
    # Capture probability from middle of page 312 in Seber (2002)
    p <- 1-catch[2]/catch[1]
    # Capture probability variance from equation 7.31 in Seber (2002)
    p.var <- catch[2]*(catch[1]+catch[2])/(catch[1]^3)
    # compute CIs
    N0.ci <- iRemovalNCI(N0,sqrt(N0.var),conf.level)
    p.ci <- iRemovalNCI(p,sqrt(p.var),conf.level)
    # vector of estimates
    tmp <- c(N0,sqrt(N0.var),N0.ci,p,sqrt(p.var),p.ci)
  }
  # names to vector of estimates
  names(tmp) <- c("No","No.se","No.LCI","No.UCI","p","p.se","p.LCI","p.UCI")
  # return list
  list(est=tmp,catch=catch,lbl="Seber (2002) 2-Pass Removal Method")
}

#=============================================================
# INTERNAL -- Calculate Robson-Regier 2-pass estimates
#=============================================================
iRobsonRegier2 <- function(catch,conf.level) {
  if (length(catch)!=2) {
    stop("Robson & Regier (1968) 2-pass method can only be used two samples.",call.=FALSE)
  } else if (catch[2] >= catch[1]) {
    warning("Catch data results in model failure for Robson & Regier (1968) 2-pass method.",call.=FALSE)
    # return empty vector
    tmp <- rep(NA,8)
  } else {
    N0 <- (catch[1]^2-catch[2])/(catch[1]-catch[2])
    N0.var <- ((catch[1]^2)*(catch[2]^2)*(catch[1]+catch[2]))/((catch[1]-catch[2])^4)
    p <- 1-(catch[2]/(catch[1]+1))
    p.var <- catch[2]*(catch[1]+catch[2])/(catch[1]^3)
    # compute CIs
    N0.ci <- iRemovalNCI(N0,sqrt(N0.var),conf.level)
    p.ci <- iRemovalNCI(p,sqrt(p.var),conf.level)
    # vector of estimates
    tmp <- c(N0,sqrt(N0.var),N0.ci,p,sqrt(p.var),p.ci)
  }
  # names to vector of estimates
  names(tmp) <- c("No","No.se","No.LCI","No.UCI","p","p.se","p.LCI","p.UCI")
  # return list
  list(est=tmp,catch=catch,lbl="Robson & Regier (1968) 2-Pass Removal Method")
}


#' @rdname removal
#' @export
summary.removal <- function(object,parm=c("No","p","p1"),digits=getOption("digits"),verbose=FALSE,...) {
  parm <- match.arg(parm,several.ok=TRUE)
  # send warning if chose 'p1' parameter but not Schnute method
  #   but don't warn if all parameters are chosen
  #   but stop if only p1 was chosen
  if (("p1" %in% parm) & object$method!="Schnute") {
    msg <- paste("'p1' parameter not relevant for the ",object$method," method.")
    if (length(parm)==1) stop(msg,call.=FALSE)
    if (length(parm)<3) warning(msg,call.=FALSE)
    parm <- parm[-which(parm=="p1")]
  }
  if (verbose) {
    if (object$method %in% c("Moran","Schnute")) message("The ",object$lbl," method was used (SEs not computed).")
    else message("The ",object$lbl," method was used.")
  }
  if (object$method %in% c("Zippin","CarleStrub","Seber3","Seber2","RobsonRegier2")) {
    res <- matrix(object$est[c("No","No.se","p","p.se")],nrow=2,byrow=TRUE)
    colnames(res) <- c("Estimate","Std. Error")
    rownames(res) <- c("No","p")
  } else if (object$method=="Moran") {
    res <- matrix(object$est[c("No","p")],nrow=2)
    colnames(res) <- c("Estimate")
    rownames(res) <- c("No","p")
  } else {
    res <- matrix(object$est[c("No","p","p1")],nrow=3)
    colnames(res) <- c("Estimate")
    rownames(res) <- c("No","p","p1")
  }
  res <- res[which(rownames(res) %in% parm),,drop=FALSE]
  round(res,digits)
}

#' @rdname removal
#' @export
confint.removal <- function(object,parm=c("No","p"),
                            level=conf.level,conf.level=NULL,
                            digits=getOption("digits"),verbose=FALSE,...) {
  if (!is.null(level)) warning("The confidence level is not set here, it is set with 'conf.level=' in 'removal()'.",call.=FALSE)
  parm <- match.arg(parm,several.ok=TRUE)
  if (object$method %in% c("Zippin","CarleStrub","Seber3","Seber2","RobsonRegier2")) {
    res <- matrix(object$est[c("No.LCI","No.UCI","p.LCI","p.UCI")],nrow=2,byrow=TRUE)
    rownames(res) <- c("No","p")
  } else {
    ## Handle some messaging
    if (object$method %in% c("Moran","Schnute")) {
      # warn about no CIs for p with Moran and Schnute but only if p is the only parm chosen
      if ("p" %in% parm) {
        if (length(parm)==1) stop("Confidence intervals for 'p' cannot be computed with ",object$method," method.",call.=FALSE)
        parm <- "No"
      }
      # print messages about CI fails if they exist
      if (!is.na(object$LCImsg) & verbose) message(object$LCImsg)
      if (!is.na(object$UCImsg) & verbose) message(object$UCImsg)
    }
    res <- matrix(object$est[c("No.LCI","No.UCI")],nrow=1)
    rownames(res) <- c("No")
  }  
  colnames(res) <- iCILabel(object$conf.level)
  round(res,digits)
}
