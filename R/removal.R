#'Computes population estimates for k-, 3-, or 2-pass removal data.
#'
#'Computes estimates, with confidence intervals, of the population size and
#'probability of capture from the number of fish removed in k-, 3-, or 2-passes
#'in a closed population.
#'
#'The main function computes the estimates and associated standard errors for
#'the initial population size, No, and probability of capture, p, for five
#'possible methods chosen with the \code{type} argument.  The types of methods
#'that can be used are listed below:
#'\itemize{
#'\item \code{type="Zippin"}: The general k-pass estimator generally attributed to
#'Zippin.  This function iteratively solves for No in equation 3 of Carle and Strub (1978).
#'\item \code{type="CarleStrub"}: The general weighted k-pass estimator proposed by
#'Carle and Strub (1978).  This function iteratively solves for No in equation
#'7 of Carle and Strub (1978).
#'\item \code{type="Seber3"}: The special case for k=3 estimator shown by Seber(1982).
#'\item \code{type="Seber2"}: The special case for k=2 estimator shown by Seber(1982).
#'\item \code{type="RobsonRegier2"}: The special case for k=2 estimator shown by
#'Robson and Regier (1968).
#'}
#'
#'Confidence intervals are computed using standard large-sample normal
#'distribution theory.  Note that the confidence intervals for the 2- and
#'3-pass special cases are only approximately correct if the estimated
#'population size is greater than 200.  If the estimated population size is
#'between 50 and 200 then a 95\% CI behaves more like a 90\% CI.
#'
#'Note that, in the Carle Strub method, that if the resultant No estimate is
#'equal to the sum of the catches (T) then the estimate of No that is returned
#'will be the sum of the catches.  In this instance and if the \dQuote{Seber}
#'method of computing the standard error is used then the SE will not be
#'estimable and the confidence intervals can not be constructed.
#'
#'@aliases removal summary.removal confint.removal
#'@param catch A numerical vector of catches of fish at each pass.
#'@param type A single string that identifies the type of removal method to use for
#'the calculations.  See details.
#'@param alpha A single numeric value for the alpha parameter in the CarleStrub method
#'(default is \code{1}).
#'@param beta A single numeric value for the beta parameter in the CarleStrub method
#'(default is \code{1}).
#'@param CS.se A single string that identifies whether the SE in the CarleStrub method
#'should be computed according to Seber or Zippin.
#'@param object An object saved from \code{removal()} (i.e., of class \code{removal}).
#'@param parm A specification of which parameters are to be given confidence
#'intervals, either a vector of numbers or a vector of names.  If missing, all
#'parameters are considered.
#'@param level Same as \code{conf.level} but used for compatability with
#'generic \code{confint} function.
#'@param conf.level A single number representing the level of confidence to use for
#'constructing confidence intervals.
#'@param just.ests A logical that indicates whether just the estimates
#'(\code{=TRUE}) or the return list (\code{=FALSE}; default; see below) is returned.
#'@param \dots Additional arguments for methods.
#'@return A vector that contains the estimaes and standard errors for No and p
#'if \code{just.ests=TRUE} or (default) a list with the following items:
#'\itemize{
#'\item catch the original vector of observed catches.
#'\item type The type of method used (provided by the user).
#'\item meth A label for the type of method used.
#'\item est A vector that contains the estimates and standard errors for No and p.
#'}
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{depletion}}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl//Depletion.pdf}
#'@references Carle, F.L. and M.R. Strub. 1978. A new method for estimating
#'population size from removal data.  Biometrics, 34:621-630.
#'
#'Seber, G.A.F. 1982. The Estimation of Animal Abundance. Edward Arnold, second edition.
#'
#'Robson, D.S., and H.A. Regier.  1968.  Estimation of population number and
#'mortality rates.  pp. 124-158 in Ricker, W.E. (editor) Methods for Assessment
#'of Fish Production in Fresh Waters.  IBP Handbook NO. 3 Blackwell Scientific
#'Publications, Oxford.
#'
#'Cowx, I.G.  1983.  Review of the methods for estimating fish population size
#'from survey removal data.  Fisheries Management, 14:67-82.
#'@keywords manip
#'@examples
#'## First example -- 3 passes
#'ct3 <- c(77,50,37)
#'
#'# Zippin (default) method
#'p1 <- removal(ct3)
#'summary(p1)
#'confint(p1)  
#'
#'# Carle Strub method
#'p2 <- removal(ct3,type="CarleStrub")
#'summary(p2)
#'confint(p2)
#'
#'# Seber method
#'p3 <- removal(ct3,type="Seber3")
#'summary(p3)
#'confint(p3)
#'
#'## Second example -- 2 passes
#'ct2 <- c(77,37)
#'
#'# Seber method
#'p4 <- removal(ct2,type="Seber2")
#'summary(p4)
#'confint(p4)
#'
#'# Robson-Regier method
#'p5 <- removal(ct2,type="RobsonRegier2")
#'summary(p5)
#'confint(p5)
#'
#'## Demonstration of situation where estimates cannot be made
#'bad <- c(34,23,45)
#'bad1 <- removal(bad)
#'
#'
#'### Using lapply() to use removal on many different groups
#'## create a dummy data frame
#'lake <- factor(rep(c("Ash Tree","Bark","Clay"),each=5))
#'year <- factor(rep(c("2010","2011","2010","2011","2010","2011"),times=c(2,3,3,2,2,3)))
#'pass <- factor(c(1,2,1,2,3,1,2,3,1,2,1,2,1,2,3))
#'catch <- c(57,34,65,34,12,54,26,9,54,27,67,34,68,35,12)
#'d <- data.frame(lake,year,pass,catch)
#'
#'## create a variable that indicates each different group
#'d$group <- with(d,interaction(lake,year))
#'d
#'## split the catch by the different groups (creates a list of catch vectors)
#'ds <- split(d$catch,d$group)
#'## apply removal() to each catch vector (i.e., different group)
#'res <- lapply(ds,removal,just.ests=TRUE)
#'res <- data.frame(t(data.frame(res,check.names=FALSE)))
#'## get rownames from above and split into separate columns
#'nms <- t(data.frame(strsplit(rownames(res),"\\.")))
#'attr(nms,"dimnames") <- NULL
#'fnl <- data.frame(nms,res)
#'## put names together with values
#'rownames(fnl) <- NULL
#'colnames(fnl)[1:2] <- c("Lake","Year")
#'fnl
#'## append approx. 95% CIs
#'fnl$No.LCI <- fnl$No-1.96*fnl$No.se
#'fnl$No.UCI <- fnl$No+1.96*fnl$No.se
#'fnl
#'
#'@rdname removal
#'@export removal
removal <- function(catch,type=c("Zippin","CarleStrub","Seber3","Seber2","RobsonRegier2"),alpha=1,beta=1,CS.se=c("Zippin","Alternative"),just.ests=FALSE) {
  #-----------------------------------------------------------------------------
  # The following is an internal function for Zippin variance calculations
  #-----------------------------------------------------------------------------
  var.Zippin <- function(N0,p,k,which=c("p","N0")) {
    q <- 1-p
    if (which=="N0") {
      # formula below is different then formula in cowx (1983) but same as
      #   formula in Sweka et al. (2006)
      (N0*(1-q^k)*(q^k))/(((1-q^k)^2)-((p*k)^2)*q^(k-1)) 
    } else {
      (((q*p)^2)*(1-q^k))/(N0*(q*((1-q^k)^2)-((p*k)^2)*(q^k)))
    }
  }

  #-----------------------------------------------------------------------------
  # The methods for each type of calculation are internal functions to removal
  #-----------------------------------------------------------------------------
  pr.Zippin <- function(X,T,k) {
    # Uses modified equation 3 from Carle & Strub (1978) in the while statement
    N0 <- T
    while ((N0+0.5)*((k*N0-X-T)^k)-(N0-T+0.5)*((k*N0-X)^k) >= 0) { N0 <- N0+1 }
    p <- T/(k*N0-X)
    p.var <- var.Zippin(N0,p,k,"p")
    N0.var <- var.Zippin(N0,p,k,"N0")
    c(N0,p,sqrt(N0.var),sqrt(p.var))
  }
  
  pr.CarleStrub <- function(X,T,k,alpha,beta,CS.se) {
    # Uses equation 7 from Carle & Strub (1978) in the while statement
    N0 <- T
    while (((N0+1)/(N0-T+1))*prod((k*N0-X-T+beta+k-i)/(k*N0-X+alpha+beta+k-i)) >= 1.0) { N0 <- N0+1 }
    p <- T/(k*N0-X)
    p.var <- var.Zippin(N0,p,k,"p")
    if (CS.se=="Zippin") {
      # Seber notes that this works unders certain conditions
      N0.var <- var.Zippin(N0,p,k,"N0")
    } else {  
      # Have yet to find a solid reference for this
      N0.var <- (N0*(N0-T)*T)/((T^2)-N0*(N0-T)*(((k*p)^2)/(1-p)))
    }
    c(N0,p,sqrt(N0.var),sqrt(p.var))    
  }
  
  pr.Seber3 <- function(X,T,k) {
    if (length(catch)!=3) {
      stop("3-Pass method can only be used three samples.",call.=FALSE)
    } else {
      N0 <- (6*X^2 - 3*X*T - T^2 +T*sqrt(T^2 + 6*X*T - 3*X^2))/(18*(X-T))
      p <- (3*X - T - sqrt(T^2 + 6*X*T - 3*X^2))/(2*X)
      p.var <- var.Zippin(N0,p,k,"p")
      N0.var <- var.Zippin(N0,p,k,"N0")
      c(N0,p,sqrt(N0.var),sqrt(p.var))
    }
  }
  
  pr.Seber2 <- function(catch) {
    if (length(catch)!=2) {
      stop("2-Pass method can only be used two samples.",call.=FALSE)
    } else {
      N0 <- catch[1]^2/(catch[1]-catch[2])
      N0.var <- ((catch[1]^2)*(catch[2]^2)*(catch[1]+catch[2]))/((catch[1]-catch[2])^4)
      p <- 1-catch[2]/catch[1]
      p.var <- catch[2]*(catch[1]+catch[2])/(catch[1]^3)
      c(N0,p,sqrt(N0.var),sqrt(p.var))
    }
  }

  pr.RobsonRegier2 <- function(catch) {
    if (length(catch)!=2) {
      stop("2-Pass method can only be used two samples.",call.=FALSE)
    } else {
      N0 <- (catch[1]^2-catch[2])/(catch[1]-catch[2])
      N0.var <- ((catch[1]^2)*(catch[2]^2)*(catch[1]+catch[2]))/((catch[1]-catch[2])^4)
      p <- 1-(catch[2]/(catch[1]+1))
      p.var <- catch[2]*(catch[1]+catch[2])/(catch[1]^3)
      c(N0,p,sqrt(N0.var),sqrt(p.var))
    }
  }
  
  k <- length(catch)
  i <- seq(1,k)
  T <- sum(catch)
  X <- sum((k-i)*catch)
  type <- match.arg(type)
  switch(type,
    Zippin={
      meth <- "Zippin's K-Pass Removal Method"
      if (X <= (((T-1)*(k-1))/2)-1) {
        warning("Catch data results in Zippin model failure.",call.=FALSE)
        res <- rep(NA,4)
      } else {
        res <- pr.Zippin(X,T,k)
      }
    },
    CarleStrub={
      meth <- "Carle & Strub's K-Pass Removal Method"
      res <- pr.CarleStrub(X,T,k,alpha,beta,match.arg(CS.se))
    },
    Seber3={
      meth <- "Seber's 3-Pass Removal Method"
      if (catch[3] >= catch[1]) {
        warning("Catch data results in 3-Pass Seber model failure.",call.=FALSE)
        res <- rep(NA,4)
      } else {
        res <- pr.Seber3(X,T,k)
      }
    },
    Seber2={
      meth <- "Seber's 2-Pass Removal Method"
      if (catch[2] >= catch[1]) {
        warning("Catch data results in 2-Pass Seber model failure.",call.=FALSE)
        res <- rep(NA,4)
      } else { 
        res <- pr.Seber2(catch)
      }
    },        
    RobsonRegier2={
      meth <- "Robson-Regier (1968) 2-Pass Removal Method"
      if (catch[2] >= catch[1]) {
        warning("Catch data results in 2-Pass Robson-Regier model failure.",call.=FALSE)
        res <- rep(NA,4)
      } else { 
        res <- pr.RobsonRegier2(catch)
      }
    }
  ) # end switch
  names(res) <- c("No","p","No.se","p.se")
  if (just.ests) { res }
  else {
    if (type %in% c("Zippin","CarleStrub","Seber3")) {
      int <- c(k,T,X)
      names(int) <- c("k","T","X")
      d <- list(type=type,meth=meth,catch=catch,int=int,est=res)
    } else {
      d <- list(type=type,meth=meth,catch=catch,est=res)
    }
    class(d) <- "removal"
    d
  }
}

#'@rdname removal
#'@method summary removal
#'@S3method summary removal
summary.removal <- function(object,...) {
  cat("The",object$meth,"method was used.\n")
  res <- matrix(object$est,nrow=2,byrow=FALSE)
  colnames(res) <- c("Estimate","Std. Error")
  rownames(res) <- c("No","p")
  res
}

#'@rdname removal
#'@method confint removal
#'@S3method confint removal
confint.removal <- function(object,parm=c("both","all","No","p"),level=conf.level,conf.level=0.95,...) {
  parm <- match.arg(parm)
  z <- c(-1,1)*qnorm((1-(1-conf.level)/2))               
  Nores <-rbind(No=object$est[1]+z*object$est[3])              # compute No results                  
  pres <- rbind(p=object$est[2]+z*object$est[4])               # compute p results
  if (parm=="all" | parm=="both") res <- rbind(Nores,pres)     # Create output matrix
    else if (parm=="No") res <- Nores
      else res <- pres
  colnames(res) <- ciLabel(conf.level)
  res
}
