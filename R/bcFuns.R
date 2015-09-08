#' @title Creates a function for a specific back-calculation model.
#'
#' @description Creates a function for a specific back-calculation model based on definitions in Vigloila and Meekan (2009).
#'
#' @details The following back-calculation models, based on definitions with abbreviations and model numbers from Vigloila and Meekan (2009), are supported.
#'
#' \tabular{ccl}{
#' \bold{Abbreviation} \tab \bold{Number} \tab \bold{Model} \cr
#' DALE \tab 1 \tab Dahl-Lea \cr
#' FRALE \tab 2 \tab Fraser-Lee \cr
#' BI, LBI \tab 3 \tab (Linear) Biological Intercept \cr
#' BPH, LBPH \tab 4 \tab (Linear) Body Proportional Hypothesis \cr
#' TVG \tab 5 \tab Time-Varying Growth \cr
#' SPH, LSPH \tab 6 \tab (Linear) Scale Proportional Hypothesis \cr
#' AE, AESPH \tab 7 \tab (Age Effect) Scale Proportional Hypothesis \cr
#' AEBPH \tab 8 \tab (Age Effect) Body Proportional Hypothesis \cr
#' MONA \tab 9 \tab Monastyrsky \cr
#' MONA-BPH \tab 10 \tab Monastyrsky Body Proportional Hypothesis \cr
#' MONA-SPH \tab 11 \tab Monastyrsky Scale Proportional Hypothesis \cr
#' WAKU \tab 12 \tab Watanabe and Kuroki \cr
#' FRY \tab 13 \tab Fry \cr
#' MF, ABI \tab 14 \tab Modified Fry, Allometric Biological Intercept \cr
#' FRY-BPH, ABPH \tab 15 \tab Fry, Allometric Body Proportional Hypothesis \cr
#' FRY-SPH, ASPH \tab 16 \tab Fry, Allometric Scale Proportional Hypothesis \cr
#' QBPH \tab 17 \tab Quadratic Body Proportional Hypothesis \cr
#' QSPH \tab 18 \tab Quadratic Scale Proportional Hypothesis \cr
#' PBPH \tab 19 \tab Polynomial Body Proportional Hypothesis \cr
#' PSPH \tab 20 \tab Polynomial Scale Proportional Hypothesis \cr
#' EBPH \tab 21 \tab Exponential Body Proportional Hypothesis \cr
#' ESPH \tab 22 \tab Exponential Scale Proportional Hypothesis \cr
#' }
#'
#' @param BCM A single numeric between 1 and 22 or a string that indicates which back-calculation model to use (based on numbers and names in Vigliola and Meekan (2009)).
#' @param msg A logical that indicates whether a message about the model and parameter definitions should be output.
#'
#' @return A function that can be used to predict length at previous age (Li) given length-at-capture (Lc), hard-part radius-at-age i (Ri), and hard-part radius-at-capture (Rc).  In addition, some functions/models may require the previous age (agei) and the age-at-capture (agec), certain parameters related to the biological intercept (R0p & L0p), or certain parameters estimated from various regression models (a,b,c,A,B,C).  See source for more information.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/Backcalculation.pdf},
#'\url{https://sites.google.com/site/fishrfiles/gnrl/BackcalculationExtra.pdf}
#'
#' @references
#' Vigliola, L. and M.G. Meekan.  2009.  The back-calculation of fish growth from otoliths.  pp. 174-211.  in B.S. Green et al. (editors).  \href{https://www.researchgate.net/publication/226394736_The_Back-Calculation_of_Fish_Growth_From_Otoliths}{Tropical Fish Otoliths: Information for Assessment, Management and Ecology.}  Review: Methods and Technologies in Fish Biology and Fisheries 11.  Springer.  
#'
#' @keywords manip
#'
#' @examples
#' ## Simple Examples
#' ( bcm1 <- bcFuns(1) )
#' bcm1(20,10,40)
#'
#' ## Example with dummy length-at-cap, radii-at-cap, and radii-at-age
#' lencap <- c(100,100,100,150,150)
#' radcap <- c(20,20,20,30,30)
#' rad    <- c( 5,10,15,15,25)
#' bcm1(lencap,rad,radcap)
#'
#' ( bcm2 <- bcFuns("FRALE") )
#' bcm2(lencap,rad,radcap,2)  # demonstrated with a=2
#'
#' @export
bcFuns <- function(BCM,msg=FALSE) {
  ## Do some checking
  if (missing(BCM)) stop("A back-calculation function must be chosen with 'BCM='",call.=FALSE)
  if (length(BCM)>1) stop("Only one back-caculation funcation can be given to 'BCM='",call.=FALSE)
  if (is.numeric(BCM)) {
    ## Function declared numerically
    if (BCM<1 | BCM>22) stop("BCM number must be between 1 and 22 inclusive.",call.=FALSE)
  } else {
    ## Function declared by name
    BCM.nms <- c("DALE","FRALE","BI","LBI","BPH","LBPH","TVG","SPH","LSPH",
               "AE","AESPH","AEBPH","MONA","MONA-BPH","MONA-SPH","WAKU",
               "FRY","MF","ABI","FRY-BPH","ABPH","FRY-SPH","ASPH",
               "QBPH","QSPH","PBPH","PSPH","EBPH","ESPH")
    BCM.nums <- c(1,2,3,3,4,4,5,6,6,7,7,8,9,10,11,12,13,14,14,15,15,16,16,17,18,19,20,21,22)
    BCM <- toupper(BCM)
    if (!(BCM %in% BCM.nms)) {
      msg <- paste(strwrap(paste("'type=' must be one of:",
                                 paste(BCM.nms,collapse=", ")),width=62),collapse="\n")
      stop(msg,call.=FALSE)
    } else BCM <- BCM.nums[which(BCM.nms %in% BCM)] # All is good ... convert 'type' to a BCM
  }

  ## identify the functions
  if (BCM==1) {
      if (msg) message("You have chosen the BCM1 or DALE back-calculation model.\n\n")
      function(Lc,Ri,Rc) { (Ri/Rc)*Lc }
  } else if (BCM==2) {
      if (msg) message("You have chosen the BCM2 or FRALE back-calculation model.\n\n")
      function(Lc,Ri,Rc,a) { a+(Lc-a)*(Ri/Rc) }
  } else if (BCM==3) {
      if (msg) message("You have chosen the BCM3, BI, or LBI back-calculation model.\n\n")
      function(Lc,Ri,Rc,L0p,R0p) { Lc+(Ri-Rc)*(Lc-L0p)/(Rc-R0p) }
  } else if (BCM==4) {
      if (msg) message("You have chosen the BCM4 or LBPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a,b) { (a+b*Ri)*Lc/(a+b*Rc) }
  } else if (BCM==5) {
      stop("The BCM5 (TVG) function is not yet implemented.",call.=FALSE)
  } else if (BCM==6) {
      if (msg) message("You have chosen the BCM6 or LSPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,A,B) { (Ri/Rc*(A+B*Lc)-A)/B }
  } else if (BCM==7) {
      if (msg) message("You have chosen the BCM7, AE, or AESPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,agei,agec,a,b,c) { -a/b+(Lc+a/b+c/b*agec)*Ri/Rc-c/b*agei }
  } else if (BCM==8) {
      if (msg) message("You have chosen the BCM8 or AEBPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,agei,agec,A,B,C) { (A+B*Ri+C*agei)/(A+B*Rc+C*agec)*Lc }
  } else if (BCM==9) {
      if (msg) message("You have chosen the BCM9 or MONA back-calculation model.\n\n")
      function(Lc,Ri,Rc,c) { Lc*((Ri/Rc)^c) }
  } else if (BCM==10) {
      if (msg) message("You have chosen the BCM10 or MONA-BPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,c) { Lc*((Ri/Rc)^c) }    # Same as BCM9 but uses nls results to estimate c
  } else if (BCM==11) {
      if (msg) message("You have chosen the BCM11 or MONA-SPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,C) { Lc*((Ri/Rc)^(1/C)) }
  } else if (BCM==12) {
      if (msg) message("You have chosen the BCM12 or WAKU back-calculation model.\n\n")
      function(Lc,Ri,Rc,L0p,R0) { exp(log(L0p) + ((log(Lc)-log(L0p))*(log(Ri)-log(R0)))/(log(Rc)-log(R0))) }
  } else if (BCM==13) {
      if (msg) message("You have chosen the BCM13 or FRY back-calculation model.\n\n")
      function(Lc,Ri,Rc,L0,R0,a) {
        a + exp(log(L0-a) + ((log(Lc-a)-log(L0-a))*(log(Ri)-log(R0)))/(log(Rc)-log(R0))) 
      }
  } else if (BCM==14) {
      if (msg) message("You have chosen the BCM14, MF, or ABI back-calculation model.\n\n")
      function(Lc,Ri,Rc,L0p,R0p,a) {
        a + exp(log(L0p-a) + ((log(Lc-a)-log(L0p-a))*(log(Ri)-log(R0p)))/(log(Rc)-log(R0p)))
      }
  } else if (BCM==15) {
      if (msg) message("You have chosen the BCM15, FRY-BPH, or ABPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a,b,c) { (a+b*Ri^c)/(a+b*Rc^c)*Lc }
  } else if (BCM==16) {
      if (msg) message("You have chosen the BCM16, FRY-SPH, or ASPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a,b,c) { a+(Lc-a)*((Ri/Rc)^c) }
  } else if (BCM==17) {
      if (msg) message("You have chosen the BCM17 or QBPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a,b,c) { (a+b*Ri+c*(Ri^2))/(a+b*Rc+c*(Rc^2))*Lc }
  } else if (BCM==18) {
      if (msg) message("You have chosen the BCM18 or QSPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a,b,c) { 
        cf1 <- b
        cf2 <- c
        Li <- numeric(length(Lc))
        for (i in 1:length(Li)) {
          cf0 <- a-((Ri[i]/Rc[i])*(a+b*Lc[i]+c*Lc[i]^2))
          roots <- Re(polyroot(c(cf0,cf1,cf2)))
          Li[i] <- roots[which(sign(roots)==1)]
        }
        Li
      }
  } else if (BCM==19) {
      if (msg) message("You have chosen the BCM19 or PBPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a) { # a must be a vector of coefficients from the polynomial regression
        exps <- 0:(length(a)-1)
        Li <- numeric(length(Lc))
        for (i in 1:length(Lc)) {
          num <- sum( a*Ri[i]^exps )
          denom <- sum( a*Rc[i]^exps )
          Li[i] <- num/denom*Lc[i]
        }
        Li
      }
  } else if (BCM==20) { 
      # Note that this is a function that should be used when finding a root, not to actually back-calculate
      if (msg) message("You have chosen the BCM20 or PSPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a) { # a must be a vector of coefficients from the polynomial regression
        exps <- 0:(length(a)-1)
        Li <- numeric(length(Lc))
        for (i in 1:length(Li)) {
          if (Ri[i]==Rc[i]) { Li[i] <- Lc[i] }
          else {
            cf <- a
            cf[1] <- cf[1] - Ri[i]/Rc[i]*sum(a*Lc[i]^exps)
            roots <- Re(polyroot(cf))
            roots <- roots[which(sign(roots)==1)]             # find only positive roots
            roots <- roots[which(roots<=Lc[i])]               # only find root less than lencap
            ifelse(length(roots)!=1,Li[i] <- NA,Li[i] <- roots)
          }
        }
        Li
      }      
  } else if (BCM==21) {
      if (msg) message("You have chosen the BCM21 or EBPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a,b) { exp(a+b*Ri)/exp(a+b*Rc)*Lc }
  } else if (BCM==22) {
      if (msg) message("You have chosen the BCM22 or ESPH back-calculation model.\n\n")
      function(Lc,Ri,Rc,a) { exp(a+(log(Lc)-a)*Ri/Rc) }
  } 
}
