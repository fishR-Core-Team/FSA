#'Estimate initial population size for multiple census mark-recapture data from an open population.
#'
#'This function takes the two parts of a Method B table and uses the Jolly-Seber method to estimate the population size at each possible sample period and the survival rate and number of additional individuals added to the population between possible sample periods.  This method assumes that the population is open.
#'
#'If \code{mb.top} contains an object from the \code{capHistSum} function then \code{mb.bot} can be left missing or will be ignored.  In this case, the function will extract the needed data from the \code{methodB.top} and \code{methodB.bot} portions of the \code{CapHist} class object.
#'
#'Seber's (1965) modification to the traditional Jolly-Seber abundance and mortality estimates are used.
#'
#'Manly (1984) did not provide details for computing a confidence interval for the B parameter.  Thus, confidence intervals for this parameter are not included in the output when \code{ci.type="Manly"} is used.  Also, Manly's method does not compute standard errors; therefore, no standard errors are returned when \code{ci.type="Manly"} is used.
#'
#'If \code{type="observables"} in the \code{summary} function then only items that can be observed from the data will be returned -- n, m, R, r, and z.  If \code{type="estimates"} in the \code{summary} function then only items that are estimated from observables will be returned -- M, N, phi, B, and their associated standard errors.
#'
#'The level of confidence is not set in the \code{confint} function, in contrast to most \code{confint} functions.  Rather the confidence level is set in the main \code{mrOpen} function.
#'
#' @aliases mrOpen summary.mrOpen confint.mrOpen
#'
#' @param mb.top A matrix that contains the \dQuote{top} of the Method B table (i.e., a contingency table of capture sample (columns) and last seen sample (rows)) or an object of class \code{CapHist} from the \code{capHistSum} function.  See details.
#' @param mb.bot A data frame that contains the \dQuote{bottom} of the Method B table (i.e., the number of marked fish in the sample (\code{m}), the number of unmarked fish in the sample (\code{u}), the total number of fish in the sample (\code{n}), and the number of marked fish returned to the population following the sample (\code{R})).
#' @param ci.type A string that indicates whether the large sample method of Jolly (\code{ci.type="Jolly"}) or the \dQuote{arbitrary} method of Manly (\code{ci.type="Manly"}) should be used to construct confidence intervals.
#' @param conf.level A number that indicates the level of confidence to use for constructing confidence intervals (default is 0.95).  See details.
#' @param phi.type A string that indicates whether the standard error for phi should include only sampling variability (\code{phi.type="SE"}) or sampling and individual variability (\code{phi.type="SD"},default).
#' @param object An object saved from the \code{mrOpen} call (i.e., of class \code{mrOpen}).
#' @param type The type of summary to return.  See details.
#' @param parm A specification of which parameters are to be given confidence intervals.  If missing, all parameters are considered.
#' @param level Same as \code{conf.level} but used for compatability with generic \code{confint} function.
#' @param \dots Additional arguments for methods.
#'
#' @return A list with the following items:
#'\itemize{
#'\item df A data frame that contains observable summaries from the data
#'and estimates of the number of extant marked fish, population size for each
#'possible sample period, survival rate between each possible pair of sample
#'periods, and the number of additional individuals added to the population
#'between each possible pair of sample periods.  In addition to the estimates,
#'values of the standard errors and the lower and upper confidence interval
#'bounds for each parameter are provided (however, see the details above).
#'\item ci.type The provided type of confidence intervals that was used.
#'\item phi.type The provided type of standard error for phi that was
#'used.
#'\item conf.level The provided level of confidence that was used.
#'}
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{capHistSum}}, \code{\link{mrClosed}}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/MROpen.pdf}
#'
#' @references Jolly, G.M. 1965. Explicit estimates from capture-recapture data with both death and immigration -- stochastic model. Biometrika, 52:225-247.
#'
#'Seber, G.A.F. 1965. A note on the multiple recapture census. Biometrika 52:249-259.
#'
#'Seber, G.A.F. 1982. The Estimation of Animal Abundance. Edward Arnold, second edition.
#'
#'Leslie, P.H. and D. Chitty. 1951. The estimation of population parameters from data obtained by means of the capture-recapture method. I. The maximum likelihood equations for estimating the death-rate. Biometrika, 38:269-292.
#'
#'Manly, B.F.J. Obtaining confidence limits on parameters of the Jolly-Seber model for capture-recapture data. Biometrics, 40:749-758.
#'
#'Pollock, K.H., J.D. Nichols, C. Brownie, and J.E. Hines. 1991. Statistical inference for capture-recapture experiments. Wildlife Monographs, 107:1-97.
#'
#' @keywords manip
#'
#' @examples
#'## First example -- capture histories summarized with capHistSum()
#'data(CutthroatAL)
#'ch1 <- capHistSum(CutthroatAL,-1)  # ignore first column of fish ID
#'ex1 <- mrOpen(ch1)
#'summary(ex1)
#'summary(ex1)
#'
#'## Second example - Jolly's data -- summarized data entered "by hand"
#'s1 <- rep(NA,13)
#'s2 <- c(10,rep(NA,12))
#'s3 <- c(3,34,rep(NA,11))
#'s4 <- c(5,18,33,rep(NA,10))
#'s5 <- c(2,8,13,30,rep(NA,9))
#'s6 <- c(2,4,8,20,43,rep(NA,8))
#'s7 <- c(1,6,5,10,34,56,rep(NA,7))
#'s8 <- c(0,4,0,3,14,19,46,rep(NA,6))
#'s9 <- c(0,2,4,2,11,12,28,51,rep(NA,5))
#'s10 <- c(0,0,1,2,3,5,17,22,34,rep(NA,4))
#'s11 <- c(1,2,3,1,0,4,8,12,16,30,rep(NA,3))
#'s12 <- c(0,1,3,1,1,2,7,4,11,16,26,NA,NA)
#'s13 <- c(0,1,0,2,3,3,2,10,9,12,18,35,NA)
#'jolly.top <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13)
#'
#'n <- c(54,146,169,209,220,209,250,176,172,127,123,120,142)
#'R <- c(54,143,164,202,214,207,243,175,169,126,120,120,0)
#'m <- c(0,10,37,56,53,77,112,86,110,84,77,72,95)
#'u <- n-m
#'
#'jolly.bot <- rbind(m,u,n,R)
#'ex2 <- mrOpen(jolly.top,jolly.bot)
#'summary(ex2)
#'confint(ex2)
#'ex3 <- mrOpen(jolly.top,jolly.bot,ci.type="Manly")
#'summary(ex3)
#'confint(ex3)
#'
#' @rdname mrOpen
#' @export
mrOpen <- function(mb.top,mb.bot=NULL,ci.type=c("Jolly","Manly"),conf.level=0.95,phi.type=c("SD","SE")) {
  # All large sample formulae are as they appear in Pollock et al. (1990)
  # r,z,M,N,phi,B all appear to be accurate with data in Krebs
  # r,z accurate with data in Begon
  # M,N,phi all approximately (rounding?) accurate with data in Begon
  # B not accurate with data in Begon

  # Jolly N.se, phi.se, B.se accurate with data in Krebs
  # Jolly N.SE, phi.SE appears approximately accurate (rounding?) with data in Begon
  # Jolly B.SE not accurate with data in Begon
  # Manly N.CI, phi.CI NOT TESTED

  ## INTERNAL -- function to estimate N
  est.N <- function(df,type,conf.level) {
    # Estimate population
    df$N <- (df$n+1)*df$M/(df$m+1)
    if (any(df$N<df$n,na.rm=TRUE)) {
      warning("At least one population estimate is less than the sample size at time t.\n  Population estimates were set equal to sample size for those times.\n",call.=FALSE)
      df$N[which(df$N<df$n)] <- df$n[which(df$N<df$n)]
    }
    switch(type,
      Jolly={ # Jolly large-sample method (from Begon, same as in Pollock et al. 1990)
        df$N.se <- sqrt(df$N*(df$N-df$n)*((df$M-df$m+df$R)/df$M*((1/df$r)-(1/df$R))+((df$N-df$M)/(df$N*df$m))))
        zstar <- qnorm(1-(1-conf.level)/2)
        df$N.lci <- df$N-zstar*df$N.se
        df$N.uci <- df$N+zstar*df$N.se
        df
      },
      Manly={ # Manly arbitrary method
        p <- df$n/df$N
        T1.N <- log(df$N) + log((1-(p/2)+sqrt(1-p))/2)
        var.T1.N <- ((df$M-df$m+df$R+1)/(df$M+1))*((1/(df$r+1))-(1/(df$R+1)))+(1/(df$m+1))+(1/(df$n+1))
        L <- exp(T1.N-1.6*sqrt(var.T1.N))
        U <- exp(T1.N+2.4*sqrt(var.T1.N))
        df$N.lci <- ((4*L+df$n)^2)/(16*L)
        df$N.uci <- ((4*U+df$n)^2)/(16*U)
        df
      }
    ) # end switch
  } # end internal est.N

  ## INTERNAL -- estimate phi
  est.phi <- function(df,k,type,conf.level,phi.type) {
    # By definition, allows estimate of first sample survival
    df$M[1] <- 0
    # Current time vector w/o last row to match size of future time vector
    df1 <- df[-k,]
    # Future time vector w/o first row to match size of current time vector
    df2 <- df[-1,]
    # Estimate of phi
    phi <- df2$M/(df1$M-df1$m+df1$R)
    switch(type,
      Jolly={ # Jolly large-sample method (from Begon, same as in Pollock et al. 1990)
        phi.var <- (phi^2)*(((df2$M-df2$m)*(df2$M-df2$m+df2$R))/(df2$M^2)*((1/df2$r)-(1/df2$R))+
                            ((df1$M-df1$m)/(df1$M-df1$m+df1$R)*((1/df1$r)-(1/df1$R))))
        if (phi.type=="SD") phi.var <- phi.var + ((phi*(1-phi))/(df1$M-df1$m+df1$R))
        phi.se <- sqrt(phi.var)
        zstar <- qnorm(1-(1-conf.level)/2)
        phi.lci <- phi-zstar*phi.se
        phi.uci <- phi+zstar*phi.se
        phi.se[k] <- phi[k] <- phi.lci[k] <- phi.uci[k] <- NA
        df$M[1] <- NA
        data.frame(df,phi,phi.se,phi.lci,phi.uci)
      },
      Manly={ # Manly arbitrary method
        B <- ((df2$M-df2$m+1)*(df2$M-df2$m+df2$R+1)/((df2$M+1)^2))*((1/(df2$r+1))-(1/(df2$R+1)))+
                ((df1$M-df1$m+1)/(df1$M-df1$m+df1$R+1))*((1/(df1$r+1))-(1/(df1$R+1)))
        C <- 1/(df2$M+1)
        A <- C/(B+C)
        T2.phi <- log((1-sqrt(1-A*phi))/(1+sqrt(1+A*phi)))
        var.T2.phi <- B+C
        L <- exp(T2.phi-1.9*sqrt(var.T2.phi))
        U <- exp(T2.phi+2.1*sqrt(var.T2.phi))
        phi.lci <- (1/A)*(1-(((1-L)^2)/((1+L)^2)))
        phi.uci <- (1/A)*(1-(((1-U)^2)/((1+U)^2)))
        phi[k] <- phi.lci[k] <- phi.uci[k] <- NA
        df$M[1] <- NA
        data.frame(df,phi,phi.lci,phi.uci)
      }
    ) # end switch
  } # end internal est.phi

  ## INTERNAL -- estimate B
  est.B <- function(df,k,type,conf.level) {
    # Current time vector w/o last row to match size of future time vector
    df1 <- df[-k,]
    # Future time vector w/o first row to match size of current time vector
    df2 <- df[-1,]
    B <- df2$N-df1$phi*(df1$N-df1$n+df1$R)
    switch(type,
      Jolly={
        B.se <- sqrt((((B^2)*(df2$M-df2$m)*(df2$M-df2$m+df2$R)/(df2$M^2)))*((1/df2$r)-(1/df2$R))
               +((df1$M-df1$m)/(df1$M-df1$m+df1$R)*((df1$phi*df1$R*(df1$N-df1$M))^2)/(df1$M^2)*((1/df1$r)-(1/df1$R)))
               +((df1$N-df1$n)*(df2$N-B)*(df1$N-df1$M)*(1-df1$phi)/(df1$N*(df1$M-df1$m+df1$R)))
               +(df2$N*(df2$N-df2$n)*(df2$N-df2$M)/(df2$N*df2$m))
               +((df1$phi^2)*df1$N*(df1$N-df1$n)*(df1$N-df1$M)/(df1$N*df1$m)))
        zstar <- qnorm(1-(1-conf.level)/2)
        B.lci <- B-zstar*B.se
        B.uci <- B+zstar*B.se
        B[k] <- B.se[k] <- B.lci[k] <- B.uci[k] <- NA
        data.frame(df,B,B.se,B.lci,B.uci)
      },
      Manly={
        cat("Manly did not provide a method for computing confidence intervals for B.\n")
        B[k] <- NA
        data.frame(df,B)
      }
    ) #end switch
  } # end internal est.B       
       
  # MAIN FUNCTION
  ci.type <- match.arg(ci.type)
  phi.type <- match.arg(phi.type)
  if (class(mb.top)=="CapHist") {
    mb.bot <- mb.top$methodB.bot
    mb.top <- mb.top$methodB.top
  } else if (is.null(mb.bot)) stop("Must enter both a mb.top and a mb.bot argument.",call.=FALSE)
  # Number of samples
  k <- dim(mb.bot)[2]
  if (k<=2) stop("The Jolly-Seber method requires more than 2 samples.\n",call.=FALSE)
    else {
      # Transpose method B bottom portion, delete u column, make a data.frame
      df <- data.frame(t(mb.bot)[,-2])
      # Future catches of a sample
      df$r <- apply(mb.top,1,sum,na.rm=TRUE)
      df$r[k] <- NA
      # Initialize vector for loop
      df$z <- rep(NA,k)
      for (i in 2:(k-1)) {
        # Z is sum of matrix with smaller rows and larger columns
        df$z[i] <- sum(mb.top[1:(i-1),(i+1):k],na.rm=TRUE)
      }
      # Estimate marked fish
      df$M <- df$m+(df$R+1)*df$z/(df$r+1)
      # Standard error of marked fish
      df$M.se <- sqrt((df$M-df$m)*(df$M-df$m+df$R)*((1/df$r)-1/df$R))
      # Estimate population sizes with CIs
      df <- est.N(df,ci.type,conf.level)
      # Estimate survival rates wtih CIs
      df <- est.phi(df,k,ci.type,conf.level,phi.type)
      # Estimate additions to the population
      df <- est.B(df,k,ci.type,conf.level)
      switch(ci.type,
        Jolly={
          df[,c(6:11,16:19)] <- round(df[,c(6:11,16:19)],1)
          df[,12:15] <- round(df[,12:15],3)
        },
        Manly={
          df[,c(6:10,14)] <- round(df[,c(6:10,14)],1)
          df[,11:13] <- round(df[,11:13],3)
        }
      )  # end switch
    }
  d <- list(df=df,ci.type=ci.type,phi.type=phi.type,conf.level=conf.level)
  class(d) <- "mrOpen"
  d
}

#' @rdname mrOpen
#' @export
summary.mrOpen <- function(object,type=c("all","observables","estimates"),...) {
  type <- match.arg(type)
  if (type=="all" | type=="observables") {
    cat("Observables\n")
    print(object$df[,c("m","n","R","r","z")])
  }
  if (type=="all" | type=="estimates") {
    cat("\nEstimates\n")
    if (object$ci.type=="Jolly") { print(object$df[,c("M","M.se","N","N.se","phi","phi.se","B","B.se")]) }
      else { print(object$df[,c("M","N","phi","B")]) }
    if (object$phi.type=="SD") cat("\nStandard error of phi includes sampling and individual variability.\n")
      else cat("\nStandard error of phi includes only sampling variability.\n")
  }
}

#' @rdname mrOpen
#' @export
confint.mrOpen <- function(object,parm=c("all","N","phi","B"),level=NULL,conf.level=NULL,...) {
  if(!is.null(conf.level)) cat("Confidence level was set at",conf.level,"in mrOpen function.  It cannot be changed here.\n")
  parm <- match.arg(parm)
  cat("The",object$ci.type,"method was used to construct confidence intervals.\n\n")
  if (object$ci.type=="Manly") {
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
