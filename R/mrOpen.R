#' @title Jolly-Seber analysis from multiple mark-recapture events from an open population.
#'
#' @description This function takes the two parts of a Method B table and uses the Jolly-Seber method to estimate the population size at each possible sample period and the apparent survival rate and number of additional individuals added to the population between possible sample periods.  This method assumes that the population is open.
#'
#' @details \code{jolly} is just a convenience wrapper that produces the exact same results as \code{mrOpen}.
#' 
#' If \code{mb.top} contains an object from the \code{\link{capHistSum}} function then \code{mb.bot} can be left missing.  In this case, the function will extract the needed data from the \code{methodB.top} and \code{methodB.bot} portions of the \code{CapHist} class object.
#' 
#' If \code{mb.top} is a matrix then it must be square, must have non-negative and no NA values in the upper triangle, and all NA values on the lower triangle and diagonal.  If \code{mb.bot} is a matrix then it must have four rows named \code{m}, \code{u}, \code{n}, and \code{R} (see \code{\link{capHistSum}} for definitions), all values must be non-NA, and the first value of \code{m} must be 0.  The last value of \code{R} can either be 0 or some positive number (it is ultimately ignored in all calculations).
#' 
#' All parameter estimates are performed using equations 4.6-4.9 from Pollock et al (1990) and from page 204 in Seber 2002.  If \code{type="Jolly"} then all standard errors (square root of the variances) are from equations 4.11, 4.12, and 4.14 in Pollock et al. (1990) (these are different than those in Seber (2002) ... see Pollock et al.'s note on page 21).  If \code{type="Jolly"} and \code{phi.full=TRUE} then the full variance for the phi parameter is given as in eqn 4.18 in Pollock et al. (1990), otherwise eqn 4.13 from Pollock et al. (1990) is used.  When \code{type="Jolly"} the confidence interval are produced using normal theory (i.e., estimate +/- z*SE).  If \code{type="Manly"} then the confidence intervals for N and phi (none will be produced for B) are constructed using the methods of Manly (1984) and as described in 2.24-2.33 of Krebs (1989).  No standard errors are returned when \code{type="Manly"}.
#'
#' The \code{summary} function returns estimates of M, N, phi, B, and their associated standard errors and, if \code{verbose=TRUE} the intermediate calculations of \dQuote{observables} from the data -- n, m, R, r, and z.
#'
#' The level of confidence is not set in the \code{confint} function, in contrast to most \code{confint} functions.  Rather the confidence level is set in the main \code{mrOpen} function.
#'
#' @aliases mrOpen summary.mrOpen confint.mrOpen
#'
#' @param mb.top A matrix that contains the \dQuote{top} of the Method B table (i.e., a contingency table of capture sample (columns) and last seen sample (rows)) or an object of class \code{CapHist} from \code{\link{capHistSum}}.  See details.
#' @param mb.bot A data frame that contains the \dQuote{bottom} of the Method B table (i.e., the number of marked fish in the sample (\code{m}), the number of unmarked fish in the sample (\code{u}), the total number of fish in the sample (\code{n}), and the number of marked fish returned to the population following the sample (\code{R})).
#' @param type A string that indicates whether the large sample (normal theory) method of Jolly (\code{type="Jolly"}) or the \dQuote{arbitrary} method of Manly (\code{type="Manly"}) should be used to construct confidence intervals.
#' @param conf.level A single numeric that indicates the level of confidence to use for constructing confidence intervals (default is 0.95).  See details.
#' @param phi.full A logical that indicates whether the standard error for phi should include only sampling variability (\code{phi.full=FALSE}) or sampling and individual variability (\code{phi.full=TRUE},default).
#' @param object An object from \code{mrOpen} (i.e., of class \code{mrOpen}).
#' @param verbose A logical that indicates if the observables and other notes should be printed in \code{summary} and if the type of confidence interval used should be printed in \code{confint}.  See details.
#' @param parm A specification of which parameters are to be given confidence intervals.  If missing, all parameters are considered.
#' @param level Same as \code{conf.level} but used for compatability with generic \code{confint} function.
#' @param \dots Additional arguments for methods.
#'
#' @return A list with the following items:
#'  \itemize{
#'    \item df A data frame that contains observable summaries from the data and estimates of the number of extant marked fish  (M), population size for each possible sample period (N), apparent survival rate between each possible pair of sample periods (phi), and the number of additional individuals added to the population between each possible pair of sample periods (B).  In addition to the estimates, values of the standard errors and the lower and upper confidence interval bounds for each parameter are provided (however, see the details above).
#'    \item type The provided type of confidence intervals that was used.
#'    \item phi.full The provided logical that indicates the type of standard error for phi that was used.
#'    \item conf.level The provided level of confidence that was used.
#'  }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{capHistSum}}, \code{\link{mrClosed}}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/MROpen.pdf}
#' 
#' @section Testing:  The formulas have been triple-checked against formulas in Pollock et al. (1990), Manly (1984), and Seber (2002).
#' 
#' The results for the \code{\link{CutthroatAL}} data file (as analyzed in the example) was compared to results from the JOLLY program available at \url{http://www.mbr-pwrc.usgs.gov/software/jolly.html}.  The r and z values matched, all M and N estimates match at one decimal place, all phi are within 0.001, and all B are within 0.7.  The SE match for M except for two estimates that are within 0.1, match for N except for one estimate that is within 0.1, are within 0.001 for phi, and are within 1.3 for B (except for for the first estimate which is dramatically off).
#' 
#' The results of \code{mrOpen} related to Table 4.4 of Pollock et al. (1990) match (to one decimal place) except for three estimates that are within 0.1\% for N, match (to two decimal places) for phi except for where Pollock set phi>1 to phi=1, match for B except for Pollock set B<0 to B=0. The SE match (to two decimal places) for N except for N15 (which is within 0.5, <5\%), match (to three decimal places) for phi except for phi15 (which is within 0.001, <0.5\%), match (to two decimal places) for B except for B17 and B20 which are within 0.2 (<0.2\%)
#' 
#' All point estimates of M, N, phi, and B and the SE of phi match the results in Table 2.3 of Krebs (1989) (within minimal rounding error for a very small number of results).  The SE of N results are not close to those of Krebs (1989) (who does not provide a formula for SE so the discrepancy cannot be explored).  The SE of B results match those of Krebs (1989) for 5 of the 8 values and are within 5\% for 2 of the other 3 values (the last estimate is off by 27\%).
#' 
#' For comparing to Jolly's data as presented in Tables 5.1 and 5.2 of Seber (2002), M was within 4 (less than 1.5\%), N was within 3\% (except N2 which was within 9\%), phi was within 0.01 (less than 1.5%), and B was within 7 (less than 5\%) except for B2 and B8.
#'
#' @references
#' Jolly, G.M. 1965. Explicit estimates from capture-recapture data with both death and immigration -- stochastic model. Biometrika, 52:225-247.
#' 
#' Krebs, C.J.  1989.  Ecological Methodology.  Harper & Row Publishers, New York.
#'
#' Leslie, P.H. and D. Chitty. 1951. The estimation of population parameters from data obtained by means of the capture-recapture method. I. The maximum likelihood equations for estimating the death-rate. Biometrika, 38:269-292.
#'
#' Manly, B.F.J. 1984.  Obtaining confidence limits on parameters of the Jolly-Seber model for capture-recapture data. Biometrics, 40:749-758.
#'
#' Pollock, K.H., J.D. Nichols, C. Brownie, and J.E. Hines. 1991. Statistical inference for capture-recapture experiments. Wildlife Monographs, 107:1-97.
#'
#' Seber, G.A.F. 1965. A note on the multiple recapture census. Biometrika 52:249-259.
#'
#' Seber, G.A.F. 2002. The Estimation of Animal Abundance. Edward Arnold, second edition (reprinted).
#'
#' @keywords manip
#'
#' @examples
#' ## First example -- capture histories summarized with capHistSum()
#' data(CutthroatAL)
#' ch1 <- capHistSum(CutthroatAL,cols2use=-1)  # ignore first column of fish ID
#' ex1 <- mrOpen(ch1)
#' summary(ex1)
#' summary(ex1,verbose=TRUE)
#' confint(ex1)
#' confint(ex1,verbose=TRUE)
#'
#' ## Second example - Jolly's data -- summarized data entered "by hand"
#' s1 <- rep(NA,13)
#' s2 <- c(10,rep(NA,12))
#' s3 <- c(3,34,rep(NA,11))
#' s4 <- c(5,18,33,rep(NA,10))
#' s5 <- c(2,8,13,30,rep(NA,9))
#' s6 <- c(2,4,8,20,43,rep(NA,8))
#' s7 <- c(1,6,5,10,34,56,rep(NA,7))
#' s8 <- c(0,4,0,3,14,19,46,rep(NA,6))
#' s9 <- c(0,2,4,2,11,12,28,51,rep(NA,5))
#' s10 <- c(0,0,1,2,3,5,17,22,34,rep(NA,4))
#' s11 <- c(1,2,3,1,0,4,8,12,16,30,rep(NA,3))
#' s12 <- c(0,1,3,1,1,2,7,4,11,16,26,NA,NA)
#' s13 <- c(0,1,0,2,3,3,2,10,9,12,18,35,NA)
#' jolly.top <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13)
#'
#' n <- c(54,146,169,209,220,209,250,176,172,127,123,120,142)
#' R <- c(54,143,164,202,214,207,243,175,169,126,120,120,0)
#' m <- c(0,10,37,56,53,77,112,86,110,84,77,72,95)
#' u <- n-m
#' jolly.bot <- rbind(m,u,n,R)
#' 
#' ex2 <- mrOpen(jolly.top,jolly.bot)
#' summary(ex2,verbose=TRUE)
#' confint(ex2,verbose=TRUE)
#' 
#' ex3 <- mrOpen(jolly.top,jolly.bot,type="Manly")
#' summary(ex3,verbose=TRUE)
#' confint(ex3,verbose=TRUE)
#' 
#' ## demonstrate use of jolly()
#' ex3a <- jolly(jolly.top,jolly.bot)
#' 
#' @rdname mrOpen
#' @export
jolly <- function(...) { mrOpen(...) }
 
#' @rdname mrOpen
#' @export
mrOpen <- function(mb.top,mb.bot=NULL,type=c("Jolly","Manly"),conf.level=0.95,phi.full=TRUE) {
  type <- match.arg(type)
  if (class(mb.top)=="CapHist") {
    mb.bot <- mb.top$methodB.bot
    mb.top <- mb.top$methodB.top
  } else if (is.null(mb.bot)) stop("Must have a 'mb.top' and a 'mb.bot'.",call.=FALSE)
  iCheckMBtop(mb.top)
  iCheckMBbot(mb.bot)
  # Number of samples
  k <- ncol(mb.bot)
  if (k<=2) stop("The Jolly-Seber method requires more than 2 samples.\n",call.=FALSE)
  # Transpose method B bottom portion, delete u column, make a data.frame
  df <- data.frame(t(mb.bot))
  df <- df[,-which(names(df)=="u")]
  # Append r info
  df$r <- iCalcr(mb.top,k)
  # Append z info
  df$z <- iCalcz(mb.top,k)
  # Estimate M and append
  df <- iEstM(df)
  # Estimate population sizes with CIs and append
  df <- iEstN(df,type,conf.level)
  # Estimate survival rates wtih CIs and append
  df <- iEstPhi(df,k,type,conf.level,phi.full)
  # Estimate additions to the population and append
  df <- iEstB(df,k,type,conf.level)
  # round M, N, and B to one decimal
  tmp <- c(grep("M",names(df)),grep("N",names(df)),grep("B",names(df)))
  df[,tmp] <- round(df[,tmp],1)
  # round phi to three decimals
  tmp <- grep("phi",names(df))
  df[,tmp] <- round(df[,tmp],3)      
  # Put results in a list, assign a class, and return the list
  d <- list(df=df,type=type,phi.full=phi.full,conf.level=conf.level)
  class(d) <- "mrOpen"
  d
}

##############################################################
## INTERNAL -- functions to check aspects of the Metod B table parts
##############################################################
iCheckMBbot <- function(mb.bot) {
  nr <- nrow(mb.bot)
  if (nr!=4) stop("'mb.bot' must contain four rows with 'm', 'u', 'n', and 'R'.",call.=FALSE)
  if (any(!names(mb.bot) %in% c("m","u","n","R"))) stop("rownames of 'mb.bot' must be 'm', 'u', 'n', and 'R'.",call.=FALSE)
  if (any(is.na(mb.bot))) stop("All values in 'mb.bot' must be non-NA.",call.=FALSE)
  if (any(mb.bot<0)) stop("All values in 'mb.bot' must be non-negative.",call.=FALSE)
  if (is.na(mb.bot["m",1]) | mb.bot["m",1]!=0) stop("First value of 'm' row in 'mb.bot' must be 0.",call.=FALSE)
}

iCheckMBtop <- function(mb.top) {
  nr <- nrow(mb.top)
  nc <- ncol(mb.top)
  if (nr!=nc) stop("'mb.top' must be square.",call.=FALSE)
  if (!all(is.na(mb.top[lower.tri(mb.top,diag=TRUE)]))) stop ("Lower triangle and diagonal of 'mb.top' must all be 'NA'.",call.=FALSE)
  if (any(is.na(mb.top[upper.tri(mb.top)]))) stop("Upper triangle of 'mb.top' cannot contain any 'NA'.",call.=FALSE)
  if (any(mb.top<0,na.rm=TRUE)) stop("All non-NA values in 'mb.top' must be non-negative.",call.=FALSE)
}

##############################################################
## INTERNAL -- function to calculate r (future catches of fish
##   released in i)
##############################################################
iCalcr <- function(mb.top,k) {
  tmp <- apply(mb.top,1,sum,na.rm=TRUE)
  # make last value NA
  tmp[k] <- NA
  # return vector
  tmp
}

##############################################################
## INTERNAL -- function to calculate z (number caught before i,
##   but not in i, but later as well)
##############################################################
iCalcz <- function(mb.top,k) {
  # Loop through 2nd to penultimate sample, but initialize vector for results first
  #   Initialization will put NA in first and last samples
  tmp <- rep(NA,k)
  for (i in 2:(k-1)) {
    # Z is sum of matrix with smaller rows and larger columns than i
    tmp[i] <- sum(mb.top[1:(i-1),(i+1):k],na.rm=TRUE)
  }
  # return the vector
  tmp
}

##############################################################
## INTERNAL -- function to estimate N
##############################################################
iEstM <- function(df) {
  # Estimate marked fish (eqn 4.6 in Pollock et al. (1990), eqn 5.22 in Seber (2002))
  df$M <- df$m+(df$R+1)*df$z/(df$r+1)
  # Standard error of marked fish (eqn 4.11 in Pollock et al. (1990))
  df$M.se <- sqrt((df$M-df$m)*(df$M-df$m+df$R)*((1/df$r)-1/df$R))
  df
}

##############################################################
## INTERNAL -- function to estimate N
##############################################################
iEstN <- function(df,type,conf.level) {
  # Estimate population (eqn 4.7 in Pollock et al. (1990), p. 204 in Seber (2002))
  df$N <- (df$n+1)*df$M/(df$m+1)
  # make a correction if more fish were sampled then the PE
  if (any(df$N<df$n,na.rm=TRUE)) {
    warning("At least one population estimate is less than the sample size at time t.\n  Population estimates were set equal to sample size for those times.\n",call.=FALSE)
    df$N[which(df$N<df$n)] <- df$n[which(df$N<df$n)]
  }
  switch(type,
         Jolly={
           # SE of N (eqn 4.11 in Pollock et al. (1990))
           df$N.se <- sqrt(df$N*(df$N-df$n)*((df$M-df$m+df$R)/df$M*((1/df$r)-(1/df$R))+((df$N-df$M)/(df$N*df$m))))
           # Normal theory CI
           zstar <- qnorm(1-(1-conf.level)/2)
           df$N.lci <- df$N-zstar*df$N.se
           df$N.uci <- df$N+zstar*df$N.se
         },
         Manly={ # Manly arbitrary method
           p <- df$n/df$N
           T1.N <- log(df$N) + log((1-(p/2)+sqrt(1-p))/2)  # Manly's (1984) eqn 7
           # variance for above ... Manly's (1984) eqn 8
           var.T1.N <- ((df$M-df$m+df$R+1)/(df$M+1))*((1/(df$r+1))-(1/(df$R+1)))+(1/(df$m+1))+(1/(df$n+1))
           # From discussion at the bottom of p. 753 in Manly (1984)
           L <- exp(T1.N-1.6*sqrt(var.T1.N))
           U <- exp(T1.N+2.4*sqrt(var.T1.N))
           # From Manly's (1984) eqn 15
           df$N.lci <- ((4*L+df$n)^2)/(16*L)
           df$N.uci <- ((4*U+df$n)^2)/(16*U)
         }
  ) # end switch
  # return the data.frame
  df
}

##############################################################
## INTERNAL -- estimate phi
##############################################################
iEstPhi <- function(df,k,type,conf.level,phi.full) {
  # By definition, allows estimate of first sample survival
  df$M[1] <- 0
  # Current time vector w/o last row to match size of future time vector
  df1 <- df[-k,]
  # Future time vector w/o first row to match size of current time vector
  df2 <- df[-1,]
  # Estimate of phi (eqn 4.8 in Pollock et al. (1990), p. 204 in Seber (2002))
  phi <- df2$M/(df1$M-df1$m+df1$R)
  switch(type,
         Jolly={ # Jolly large-sample method 
           # variance of phi calculation (eqn 4.13 in Pollock et al. (1990))
           phi.var <- (phi^2)*(((df2$M-df2$m)*(df2$M-df2$m+df2$R))/(df2$M^2)*((1/df2$r)-(1/df2$R))+
                                 ((df1$M-df1$m)/(df1$M-df1$m+df1$R)*((1/df1$r)-(1/df1$R))))
           # full variance of phi (eqn 4.18 in Pollock et al. (1990))
           if (phi.full) phi.var <- phi.var + ((phi*(1-phi))/(df1$M-df1$m+df1$R))
           phi.se <- sqrt(phi.var)
           # normal theory confidence interval
           zstar <- qnorm(1-(1-conf.level)/2)
           phi.lci <- phi-zstar*phi.se
           phi.uci <- phi+zstar*phi.se
           phi.se[k] <- phi[k] <- phi.lci[k] <- phi.uci[k] <- NA
           df$M[1] <- NA
           # return results
           data.frame(df,phi,phi.se,phi.lci,phi.uci)
         },
         Manly={ # Manly arbitrary method
           # B & C are from Manly (1984) page 751 (following eqn 10)
           B <- ((df2$M-df2$m+1)*(df2$M-df2$m+df2$R+1)/((df2$M+1)^2))*((1/(df2$r+1))-(1/(df2$R+1)))+
             ((df1$M-df1$m+1)/(df1$M-df1$m+df1$R+1))*((1/(df1$r+1))-(1/(df1$R+1)))
           C <- 1/(df2$M+1)
           # this is for convenience in Manly's (1984) eqn 9
           A <- C/(B+C)
           T2.phi <- log((1-sqrt(1-A*phi))/(1+sqrt(1+A*phi)))  # Manly's (1984) eqn 9
           var.T2.phi <- B+C   # Manly's (1974) eqn 10
           # From discussion at the bottom of p. 753 in Manly (1984)
           L <- exp(T2.phi-1.9*sqrt(var.T2.phi))
           U <- exp(T2.phi+2.1*sqrt(var.T2.phi))
           # From Manly's (1984) eqn 16
           phi.lci <- (1/A)*(1-(((1-L)^2)/((1+L)^2)))
           phi.uci <- (1/A)*(1-(((1-U)^2)/((1+U)^2)))
           phi[k] <- phi.lci[k] <- phi.uci[k] <- NA
           df$M[1] <- NA
           # return results
           data.frame(df,phi,phi.lci,phi.uci)
         }
  ) # end switch
}

##############################################################
## INTERNAL -- estimate B
##############################################################
iEstB <- function(df,k,type,conf.level) {
  # Current time vector w/o last row to match size of future time vector
  df1 <- df[-k,]
  # Future time vector w/o first row to match size of current time vector
  df2 <- df[-1,]
  # Estimate of additions (eqn 4.9 in Pollock et al. (1990), p. 204 in Seber (2002))
  B <- df2$N-df1$phi*(df1$N-df1$n+df1$R)
  switch(type,
         Jolly={
           # SE of B (eqn 4.14 in Pollock et al. (1990))
           B.se <- sqrt((((B^2)*(df2$M-df2$m)*(df2$M-df2$m+df2$R)/(df2$M^2))*((1/df2$r)-(1/df2$R)))
                        +((df1$M-df1$m)/(df1$M-df1$m+df1$R)*((df1$phi*df1$R*(df1$N-df1$M))^2)/(df1$M^2)*((1/df1$r)-(1/df1$R)))
                        +((df1$N-df1$n)*(df2$N-B)*(df1$N-df1$M)*(1-df1$phi)/(df1$N*(df1$M-df1$m+df1$R)))
                        +(df2$N*(df2$N-df2$n)*(df2$N-df2$M)/(df2$N*df2$m))
                        +((df1$phi^2)*df1$N*(df1$N-df1$n)*(df1$N-df1$M)/(df1$N*df1$m)))
           # normal theory confidence interval
           zstar <- qnorm(1-(1-conf.level)/2)
           B.lci <- B-zstar*B.se
           B.uci <- B+zstar*B.se
           B[k] <- B.se[k] <- B.lci[k] <- B.uci[k] <- NA
           # return results
           data.frame(df,B,B.se,B.lci,B.uci)
         },
         Manly={
           cat("Manly did not provide a method for computing confidence intervals for B.\n")
           B[k] <- NA
           data.frame(df,B)
         }
  ) #end switch
}      

#' @rdname mrOpen
#' @export
summary.mrOpen <- function(object,verbose=FALSE,...) {
  if (verbose) {
    cat("Observables\n")
    print(object$df[,c("m","n","R","r","z")])
    cat("\nEstimates\n")
  }
  if (object$type=="Jolly") {
    print(object$df[,c("M","M.se","N","N.se","phi","phi.se","B","B.se")]) 
  } else {
    print(object$df[,c("M","N","phi","B")]) 
  }
  if (verbose) {
    if (object$phi.full) cat("\nStandard error of phi includes sampling and individual variability.\n")
      else cat("\nStandard error of phi includes only sampling variability.\n")
  }
}

#' @rdname mrOpen
#' @export
confint.mrOpen <- function(object,parm=c("all","N","phi","B"),level=NULL,conf.level=NULL,verbose=FALSE,...) {
  if(!is.null(conf.level)) cat("Confidence level was set at",conf.level,"in mrOpen function.  It cannot be changed here.\n")
  parm <- match.arg(parm)
  if (verbose) cat("The",object$type,"method was used to construct confidence intervals.\n\n")
  if (object$type=="Manly") {
    if ((parm=="all" | parm=="B")) cat("Manly did not provide a method for constructing confidence intervals for B.\n\n")
    if (parm=="all") print(object$df[,c("N.lci","N.uci","phi.lci","phi.uci")])
      else if (parm=="N") print(object$df[,c("N.lci","N.uci")])
        else if (parm=="phi") print(object$df[,c("phi.lci","phi.uci")])
  } else {
    if (parm=="all") print(object$df[,c("N.lci","N.uci","phi.lci","phi.uci","B.lci","B.uci")])
      else if (parm=="N") print(object$df[,c("N.lci","N.uci")])
        else if (parm=="phi") print(object$df[,c("phi.lci","phi.uci")])
          else print(object$df[,c("B.lci","B.uci")])
  }
}
