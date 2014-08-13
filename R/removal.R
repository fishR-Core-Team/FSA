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
#' @aliases removal summary.removal confint.removal
#'
#' @param catch A numerical vector of catch at each pass.
#' @param method A single string that identifies the removal method to use.  See details.
#' @param alpha A single numeric value for the alpha parameter in the CarleStrub method (default is \code{1}).
#' @param beta A single numeric value for the beta parameter in the CarleStrub method (default is \code{1}).
#' @param CS.se A single string that identifies whether the SE in the CarleStrub method should be computed according to Seber or Zippin.
#' @param object An object saved from \code{removal()}.
#' @param parm A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names.  If missing, all parameters are considered.
#' @param level Same as \code{conf.level} but used for compatability with generic \code{confint} function.
#' @param conf.level A single number representing the level of confidence to use for constructing confidence intervals.
#' @param just.ests A logical that indicates whether just the estimates (\code{=TRUE}) or the return list (\code{=FALSE}; default; see below) is returned.
#' @param verbose A logical that indicates whether descriptive labels should be printed from \code{summary} and if certain warnings are shown with \code{confint}.
#' @param Tkmult A single numeric that will be multiplied by the total catch in all samples to set the upper value for the range of population sizes when minimizing the log-likelihood and creating confidence intervals for the Moran and Schnute method.  Large values are much slower to compute, but too low of value can result in missing the best estimate.
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
#'    \item Tkmult The Tkmult value sent by the user.
#'  }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{depletion}}.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl//Depletion.pdf}
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
#' @references 
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
#' @keywords manip
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
#' # Carle-Strub but use alternative form of SE
#' p1a <- removal(ct3,CS.se="alternative")
#' summary(p1a)
#' confint(p1a)
#' 
#' # Carle-Strub but use alpha and beta
#' p1b <- removal(ct3,alpha=2,beta=2)
#' summary(p1b)
#' confint(p1b) 
#'
#' # Zippin method
#' p2 <- removal(ct3,method="Zippin")
#' summary(p2,verbose=TRUE)
#' confint(p2)
#'
#' # Moran method
#' p3 <- removal(ct3,method="Moran")
#' summary(p3,verbose=TRUE)
#' confint(p3,verbose=TRUE)
#'#'
#' # Schnute method
#' p4 <- removal(ct3,method="Schnute")
#' summary(p4,verbose=TRUE)
#' confint(p4,verbose=TRUE)
#'  
#' # Seber method
#' p5 <- removal(ct3,method="Seber3")
#' summary(p5,verbose=TRUE)
#' confint(p5)
#'
#' ## Second example -- 2 passes
#' ct2 <- c(77,37)
#'
#' # Seber method
#' p6 <- removal(ct2,method="Seber2")
#' summary(p6,verbose=TRUE)
#' confint(p6)
#'
#' # Robson-Regier method
#' p7 <- removal(ct2,method="RobsonRegier2")
#' summary(p7,verbose=TRUE)
#' confint(p7)
#'
#'
#' ### Test if catchability differs between first sample and the other samples
#' # chi-square test statistic from  negative log-likelihoods
#' #   from Moran and Schnute fits (from above)
#' chi2.val <- 2*(p3$min.nlogLH-p4$min.nlogLH)
#' # p-value ... no significant difference
#' pchisq(chi2.val,df=1,lower.tail=FALSE)
#'
#' # Another LRT example ... sample 1 from Schnute (1983)
#' ct4 <- c(45,11,18,8)
#' p3a <- removal(ct4,method="Moran")
#' p4a <- removal(ct4,method="Schnute")
#' chi2.val <- 2*(p3a$min.nlogLH-p4a$min.nlogLH)  # 4.74 in Schnute(1983)
#' pchisq(chi2.val,df=1,lower.tail=FALSE)         # significant difference (catchability differs)
#' summary(p4a)
#' 
#' 
#' ### Using lapply() to use removal on many different groups
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
#' ## append approx. 95% CIs
#' fnl$No.LCI <- fnl$No-1.96*fnl$No.se
#' fnl$No.UCI <- fnl$No+1.96*fnl$No.se
#' fnl
#'
#' @rdname removal
#' @export
removal <- function(catch,
                    method=c("CarleStrub","Zippin","Seber3","Seber2",
                             "RobsonRegier2","Moran","Schnute"),
                    alpha=1,beta=1,CS.se=c("Zippin","alternative"),
                    Tkmult=3,just.ests=FALSE) {
  # some initial checks
  method <- match.arg(method)
  if (!is.vector(catch)) stop("'catch' must be a vector.",call.=FALSE)
  if (!is.numeric(catch)) stop("'catch' must be a numeric vector.",call.=FALSE)
  if (length(catch)<2) stop("Cannot perform calculations with one catch value.",call.=FALSE)
  if (Tkmult<1) stop("'Tkmult' should be greater than 1.",call.=FALSE)
  # intermediate calculations
  k <- length(catch)
  i <- seq(1,k)
  T <- sum(catch)
  X <- sum((k-i)*catch)
  # Different methods
  switch(method,
    Zippin=        { tmp <- iZippin(X,T,k) },
    CarleStrub=    { tmp <- iCarleStrub(X,T,k,i,alpha,beta,CS.se) },
    Seber3=        { tmp <- iSeber3(catch,X,T,k) },
    Seber2=        { tmp <- iSeber2(catch) },        
    RobsonRegier2= { tmp <- iRobsonRegier2(catch) },
    Moran=         { tmp <- iMoran(catch,Tkmult) },
    Schnute=       { tmp <- iSchnute(catch,Tkmult) }
  )
  if (just.ests) { tmp <- tmp$est }
  else {
    if (method %in% c("Zippin","CarleStrub","Seber3")) {
      int <- c(k,T,X)
      names(int) <- c("k","T","X")
      tmp <- list(method=method,lbl=tmp$lbl,catch=catch,int=int,est=tmp$est)
    } else if (method %in% c("Moran","Schnute")) {
      tmp <- list(method=method,lbl=tmp$lbl,catch=catch,min.nlogLH=tmp$min.nlogLH,est=tmp$est,Tkmult=Tkmult)
    } else {
      tmp <- list(method=method,lbl=tmp$lbl,catch=catch,est=tmp$est)
    }
    class(tmp) <- "removal"
  }
  # return object
  tmp
}

#=============================================================
# INTERNAL -- Calculate Zippin estimates and SEs
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

iZippin <- function(X,T,k) {
  # This condition is from equation 6 in Carle & Strub (1978)
  if (X <= (((T-1)*(k-1))/2)-1) {
    warning("Catch data results in Zippin model failure.",call.=FALSE)
    # return empty vector
    tmp <- rep(NA,4)
  } else {
    # Uses modified equation 3 from Carle & Strub (1978) in the while statement
    N0 <- T
    while ((N0+0.5)*((k*N0-X-T)^k) >= (N0-T+0.5)*((k*N0-X)^k) ) { N0 <- N0+1 }
    # capture probability formula from Zippin (1956) according to Sweka (2006)
    p <- T/(k*N0-X)
    p.var <- iZippinpVar(N0,p,k) 
    N0.var <- iZippinNoVar(N0,p,k)
    # return vector
    tmp <- c(N0,p,sqrt(N0.var),sqrt(p.var))
  }
  names(tmp) <- c("No","p","No.se","p.se")
  list(lbl="Zippin (1956,1958) K-Pass Removal Method",est=tmp)
}

#=============================================================
# INTERNAL -- Calculate Carle-Strub estimates and SEs
#=============================================================
iCarleStrub <- function(X,T,k,i,alpha,beta,CS.se=c("Zippin","alternative")) {
  # Some checks
  CS.se <- match.arg(CS.se)
  if (alpha<=0 | beta<=0) stop("'alpha' and 'beta' must be positive.",call.=FALSE)
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
  # return vector
  tmp <- c(N0,p,sqrt(N0.var),sqrt(p.var))
  names(tmp) <- c("No","p","No.se","p.se")
  list(lbl="Carle & Strub (1978) K-Pass Removal Method",est=tmp)
}

#=============================================================
# INTERNAL -- Calculate Seber 3-pass estimates and SEs
#=============================================================
iSeber3 <- function(catch,X,T,k) {
  if (length(catch)!=3) {
    stop("Seber (2002) 3-pass method can only be used three samples.",call.=FALSE)
  } else if (catch[3] >= catch[1]) {
    warning("Catch data results in model failure for Seber (2002) 3-pass method.",call.=FALSE)
    # return empty vector
    tmp <- rep(NA,4)
  } else {
    # PE estimate (equation 7.24 (top) (p.315) of Seber (2002) ... note that T=Y)
    #   also matches Cowx (1983) equation 4
    N0 <- (6*X^2 - 3*X*T - T^2 + T*sqrt(T^2 + 6*X*T - 3*X^2))/(18*(X-T))
    # capture probability estimate (equation 7.24 (bottom) (p.315) of Seber (2002) ... note that T=Y)
    p <- (3*X - T - sqrt(T^2 + 6*X*T - 3*X^2))/(2*X)
    p.var <- iZippinpVar(N0,p,k) 
    N0.var <- iZippinNoVar(N0,p,k)
    # return vector
    tmp <- c(N0,p,sqrt(N0.var),sqrt(p.var))
  }
  names(tmp) <- c("No","p","No.se","p.se")
  list(lbl="Seber (2002) 3-Pass Removal Method",est=tmp)
}

#=============================================================
# INTERNAL -- Calculate Seber 2-pass estimates and SEs
#=============================================================
iSeber2 <- function(catch) {
  if (length(catch)!=2) {
    stop("Seber (2002) 2-Pass method can only be used two samples.",call.=FALSE)
  } else if (catch[2] >= catch[1]) {
    warning("Catch data results in model failure for Seber (2002) 2-Pass method.",call.=FALSE)
    # return empty vector
    tmp <- rep(NA,4)
  } else {
    # PE estimate from middle of page 312 in Seber (2002)
    N0 <- catch[1]^2/(catch[1]-catch[2])
    # PE variance from equation 7.30 in Seber (2002)
    N0.var <- ((catch[1]^2)*(catch[2]^2)*(catch[1]+catch[2]))/((catch[1]-catch[2])^4)
    # Capture probability from middle of page 312 in Seber (2002)
    p <- 1-catch[2]/catch[1]
    # Capture probability variance from equation 7.31 in Seber (2002)
    p.var <- catch[2]*(catch[1]+catch[2])/(catch[1]^3)
    # return vector
    tmp <- c(N0,p,sqrt(N0.var),sqrt(p.var))
  }
  names(tmp) <- c("No","p","No.se","p.se")
  list(lbl="Seber (2002) 2-Pass Removal Method",est=tmp)
}

#=============================================================
# INTERNAL -- Calculate Robson-Regier 2-pass estimates and SEs
#=============================================================
iRobsonRegier2 <- function(catch) {
  if (length(catch)!=2) {
    stop("Robson-Regier (1968) 2-pass method can only be used two samples.",call.=FALSE)
  } else if (catch[2] >= catch[1]) {
    warning("Catch data results in model failure for Robson-Regier (1968) 2-pass method.",call.=FALSE)
    # return empty vector
    tmp <- rep(NA,4)
  } else {
    N0 <- (catch[1]^2-catch[2])/(catch[1]-catch[2])
    N0.var <- ((catch[1]^2)*(catch[2]^2)*(catch[1]+catch[2]))/((catch[1]-catch[2])^4)
    p <- 1-(catch[2]/(catch[1]+1))
    p.var <- catch[2]*(catch[1]+catch[2])/(catch[1]^3)
    # return vector
    tmp <- c(N0,p,sqrt(N0.var),sqrt(p.var))
  }
  names(tmp) <- c("No","p","No.se","p.se")
  list(lbl="Robson-Regier (1968) 2-Pass Removal Method",est=tmp)
}

#=============================================================
# INTERNAL -- Calculate Moran estimates and CIs
#=============================================================
## Moran negative log-likelihood function
iLHMoran <- function(N,catch,k,Ti,Tk) {
  # Estimated q (Schnute (1983) equation 3.2)
  q_pred <- Tk/(k*N-sum(Ti[-k]))
  # Predicted catches and total catches
  i <- seq(1,k)
  ct_pred <- N*q_pred*(1-q_pred)^(i-1) # Schnute (1983) equation 1.8
  Ti_pred <- cumsum(ct_pred)           # Schnute (1983) equation 1.1
  Tk_pred <- Ti_pred[k]
  # log-likelihood (Schnute (1983) G(Z) and H(Z) (no K) from equation 2.6)
  #   the [catch>0] solves issues with when a catch=0
  N*log(N)-Tk*log(Tk)-(N-Tk)*log(N-Tk_pred)-log(choose(N,Tk))+sum(catch[catch>0]*log(catch[catch>0]/ct_pred[catch>0]))
}

iMoran <- function(catch,Tkmult) {
  ## Follows methodology described at the bottom of page 2157 in Schnute (1983)
  # Intermediate Calculations
  k <- length(catch)  
  # actual total catches
  Ti <- cumsum(catch)
  Tk <- Ti[k]
  # optimize for N
  tmp <- optimize(iLHMoran,c(Tk,ceiling(Tkmult*Tk)),catch=catch,k=k,Ti=Ti,Tk=Tk)
  N0 <- tmp$minimum
  p <- Tk/(k*N0-sum(Ti[-k]))
  # create return vector of estimates (no SEs)
  est <- c(N0,p,NA,NA)
  names(est) <- c("No","p","No.se","p.se")
  # return list
  list(lbl="Moran (1951) K-Pass Removal Method",est=est,min.nlogLH=tmp$objective)
}

#=============================================================
# INTERNAL -- Calculate Schnute estimates and CIs
#=============================================================
iLHSchnute <- function(N,catch,k,Ti,Tk) {
  # Estimated q's
  q1_pred <- catch[1]/N                                              # Schnute (1983) equation 3.7
  q_pred <- (Tk-catch[1])/((k-1)*(N-catch[1])-sum(Ti[-k]-catch[1]))  # Schnute (1983) equation 3.8
  # Predicted catches and total catches
  ct1_pred <- N*q1_pred
  i <- seq(2,k)
  ct_pred <- N*q_pred*(1-q1_pred)*(1-q_pred)^(i-2)
  ct_pred <- c(ct1_pred,ct_pred)
  Ti_pred <- cumsum(ct_pred)
  Tk_pred <- Ti_pred[k]
  # log-likelihood (Schnute (1983) G(Z) and H(Z) (no K) from equation 2.6)
  #   the [catch>0] solves issues with when a catch=0
  N*log(N)-Tk*log(Tk)-(N-Tk)*log(N-Tk_pred)-log(choose(N,Tk))+sum(catch[catch>0]*log(catch[catch>0]/ct_pred[catch>0]))
}

iSchnute <- function(catch,Tkmult) {
  # Intermediate Calculations
  k <- length(catch)  
  # actual total catches
  Ti <- cumsum(catch)
  Tk <- Ti[k]
  # optimize for N
  tmp <- optimize(iLHSchnute,c(Tk,ceiling(Tkmult*Tk)),catch=catch,k=k,Ti=Ti,Tk=Tk)
  N0 <- tmp$minimum
  p1 <- catch[1]/N0
  p <- (Tk-catch[1])/((k-1)*(N0-catch[1])-sum(Ti[-k]-catch[1]))
  # create return vector of estimates (no SEs)
  est <- c(N0,p1,p,NA,NA,NA)
  names(est) <- c("No","p1","p","No.se","p1.se","p.se")
  # return list
  list(lbl="Schnute (1983) K-Pass Removal Method w/ Non-constant Initial Catchability",est=est,min.nlogLH=tmp$objective)
}


#' @rdname removal
#' @export
summary.removal <- function(object,parm=c("No","p1","p"),verbose=FALSE,...) {
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
    cat("The",object$lbl,"method was used.\n")
    if (object$method %in% c("Moran","Schnute")) cat("SEs are not computed for this method.\n")
  }
  res <- matrix(object$est,nrow=ifelse(object$method=="Schnute",3,2),byrow=FALSE)
  colnames(res) <- c("Estimate","Std. Error")
  ifelse(object$method=="Schnute",rownames(res) <- c("No","p1","p"),
                                  rownames(res) <- c("No","p"))
  res[which(rownames(res) %in% parm),,drop=FALSE]
}

#' @rdname removal
#' @export
confint.removal <- function(object,parm=c("No","p"),
                            level=conf.level,conf.level=0.95,
                            verbose=FALSE,...) {
  parm <- match.arg(parm,several.ok=TRUE)
  if (object$method %in% c("Moran","Schnute")) { ## Likelihood methods
    # warn about no CIs for p with Moran and Schnute but only if p is the only parm chosen
    if ("p" %in% parm) {
      if (length(parm)==1) stop("Confidence intervals for 'p' cannot be computed with ",object$method," method.",call.=FALSE)
      parm <- "No"
    }
    # critical negative log-likelihood value for determining the CI
    nlogLHcrit <- object$min.nlogLH+qchisq(conf.level,df=1)/2
    # Intermediate calculations
    k <- length(object$catch)
    Ti <- cumsum(object$catch)
    Tk <- Ti[k]
    if (object$method=="Moran") {
      res <- iCIMoran(object$catch,k,Ti,Tk,nlogLHcrit,verbose,object$Tkmult)
    } else {
      res <- iCISchnute(object$catch,k,Ti,Tk,nlogLHcrit,verbose,object$Tkmult)
    }
  } else{
    ## Normal approximations
    z <- c(-1,1)*qnorm((1-(1-conf.level)/2))
    tmp <- summary(object,parm)
    z <- matrix(rep(z,nrow(tmp)),nrow=nrow(tmp),byrow=TRUE)
    res <- tmp[,1]+z*tmp[,2]    
  }
  rownames(res) <- parm
  colnames(res) <- iCILabel(conf.level)
  res
}

iCIMoran <- function(catch,k,Ti,Tk,nlogLHcrit,verbose,Tkmult) {
  # determine if the upper limit is going to fail (Schnute (1983) equation 3.6)
  # restrict to when the catch>0 to avoid error with log()
  mod.catch <- catch[catch>0]
  nlogLHfail <- lfactorial(Tk)-Tk*log(Tk)+Tk*sum(mod.catch*log(k*mod.catch/Tk))
  UCIfail <- nlogLHfail<nlogLHcrit
  if (UCIfail) {
    if (verbose) message("An upper confidence value for 'No' cannot be determined.")
    UCI <- Inf
  } 
  # determine if the lower limit should be the total catch
  tmp <- iLHMoran(Tk,catch,k=k,Ti=Ti,Tk=Tk)
  LCIfail <- tmp<nlogLHcrit
  if (LCIfail) {
    if (verbose) message("The lower confidence value for 'No' has been set at the total catch.")
    LCI <- Tk
  }
  if (!LCIfail | !UCIfail) {
    Ntrys <- matrix(seq(Tk,ceiling(Tkmult*Tk),0.02),ncol=1)
    nlogLHvals <- apply(Ntrys,1,iLHMoran,catch=catch,k=k,Ti=Ti,Tk=Tk)
    tmp <- range(which(nlogLHvals<=nlogLHcrit))
    if (max(tmp)==length(Ntrys) & !UCIfail) warning("Upper confidence value is ill-formed; try increasing 'Tkmult' in 'removal()'.",call.=FALSE)
    if (!LCIfail) LCI <- Ntrys[tmp[1]]
    if (!UCIfail) UCI <- Ntrys[tmp[2]]
  }
  res <- matrix(round(c(LCI,UCI),1),nrow=1)
}

iCISchnute <- function(catch,k,Ti,Tk,nlogLHcrit,verbose,Tkmult) {
  # determine if the upper limit is going to fail (Schnute (1983) equation 3.11)
  # must eliminate first catch (per Schnute) and ...
  mod.catch <- catch[-1]
  # restrict to when the catch>0 to avoid error with log()
  mod.catch <- mod.catch[mod.catch>0]
  nlogLHfail <- lfactorial(Tk)-Tk*log(Tk)+Tk+sum(mod.catch*log(((k-1)*mod.catch)/(Tk-catch[1])))
  UCIfail <- nlogLHfail<nlogLHcrit
  if (UCIfail) {
    if (verbose) message("An upper confidence value for 'No' cannot be determined.")
    UCI <- Inf
  } 
  # determine if the lower limit should be the total catch
  tmp <- iLHSchnute(Tk,catch,k=k,Ti=Ti,Tk=Tk)
  LCIfail <- tmp<nlogLHcrit
  if (LCIfail) {
    if (verbose) message("The lower confidence value for 'No' has been set at the total catch.")
    LCI <- Tk
  }
  if (!LCIfail | !UCIfail) {
    Ntrys <- matrix(seq(Tk,ceiling(Tkmult*Tk),0.02),ncol=1)
    nlogLHvals <- apply(Ntrys,1,iLHSchnute,catch=catch,k=k,Ti=Ti,Tk=Tk)
    tmp <- range(which(nlogLHvals<nlogLHcrit))
    if (max(tmp)==length(Ntrys) & !UCIfail) warning("Upper confidence value is ill-formed; try increasing 'Tkmult' in 'removal()'.",call.=FALSE)
    if (!LCIfail) LCI <- Ntrys[tmp[1]]
    if (!UCIfail) UCI <- Ntrys[tmp[2]]
  }
  res <- matrix(round(c(LCI,UCI),1),nrow=1)
}
