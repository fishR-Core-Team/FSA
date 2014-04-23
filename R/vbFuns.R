#'Creates a function for a specific von Bertalanffy parameterization.
#'
#'Creates a function for a specific von Bertalanffy model parameterization.
#'
#'@param type A string that indicates the parameterization of the von Bertalanffy
#'model.
#'@param simple A logical that indicates whether the user should be allowed to send
#'all parameter values in the first parameter argument (\code{=FALSE}; default)
#'or whether all individual parameters must be specified (\code{=TRUE}).
#'@param msg A logical that indicates whether a message about the model and
#'parameter definitions should be output (\code{=TRUE}) or not (\code{=FALSE};
#'default).
#'@return A function that can be used to predict fish length given a vector of
#'ages and values for the model parameters and, in some parameterizations,
#'values for some constants.  The result should be saved to an object that can
#'then be used as a function name.  When the resulting function is used the
#'parameters are ordered as shown when the definitions of the parameters are
#'printed after the function is called (if \code{msg=TRUE}).
#'
#'If \code{simple=FALSE} was used then the values for all three parameters can
#'be included as a vector of length three in the first paramater argument.
#'Similarly, the values for all constants can be included as a vector in the
#'first constant argument (i.e., \code{t1}).  If \code{simple=TRUE} then all
#'parameters and constants must be declared individually.  In addition, if
#'\code{simple=TRUE} then the resulting function is somewhat easier to read.
#'@note The \sQuote{original} and \sQuote{vonBertalanffy} and the
#'\sQuote{typical} and \sQuote{BevertonHolt} versions are synonomous.
#'@note Thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'@seealso \code{\link{vbComp}} and \code{\link{growthModelSim}}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffy.pdf},
#'\url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffyExtra.pdf}
#'@references Fabens, A. 1965. Properties and fitting of the von Bertalanffy
#'growth curve. Growth 29:265-289.
#'
#'Francis, R.I.C.C.  1988.  Are growth parameters estimated from tagging and
#'age-length data comparable?  Canadian Journal of Fisheries and Aquatic
#'Sciences, 45:936-942.
#'
#'Gallucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing, fitting, and
#'testing a simple growth model.  Transactions of the American Fisheries
#'Society, 108:14-25.
#'
#'Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.  Analysis and
#'comparison of fish growth from small samples of length-at-age data: Detection
#'of sexual dimorphism in Eurasian perch as an example.  Transactions of the
#'American Fisheries Society 128:483-490.
#'
#'Polacheck, T., J.P. Eveson, and G.M. Laslett.  2004.  Increase in growth
#'rates of southern bluefin tuna (\emph{Thunnus maccoyii}) over four decades:
#'1960 to 2000.  Canadian Journal of Fisheries and Aquatic Sciences,
#'61:307-322.
#'
#'Schnute, J.  1981.  A versatile growth model with statistically stable
#'parameters. Canadian Journal of Fisheries and Aquatic Sciences, 38:1128-1140.
#'
#'Somers, I. F. 1988. On a seasonally oscillating growth function. Fishbyte
#'6(1):8-11.
#'
#'Wang, Y.-G.  1998.  An improved Fabens method for estimation of growth
#'parameters in the von Bertalanffy model with individual asymptotes.  Canadian
#'Journal of Fisheries and Aquatic Sciences 55:397-400.
#'@export
#'@keywords manip
#'@examples
#'## Simple Examples
#'( vb1 <- vbFuns() )              # typical parameterization
#'ages <- 0:20
#'plot(vb1(ages,Linf=20,K=0.3,t0=-0.2)~ages,type="b",pch=19)
#'
#'( vb2 <- vbFuns("Francis") )     # Francis parameterization
#'predL <- vb2(ages,L1=10,L2=19,L3=20,t1=2,t3=18)
#'plot(predL~ages,type="b",pch=19)
#'
#'( vb2s <- vbFuns("Francis",simple=TRUE) )   # compare to vb2
#'
#'## Examples of fitting Von B models
#'##   After the last example a plot is constructed with three lines on top of each
#'##   other illustrating that the parameterizations all produce the same fitted
#'##   values.  However, observe the correlations in the summary() results.
#'
#'# Fitting the typical paramaterization of the von B function
#'data(SpotVA1)
#'fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,start=vbStarts(tl~age,data=SpotVA1))
#'summary(fit1,correlation=TRUE)
#'plot(tl~age,data=SpotVA1,pch=19)
#'curve(vb1(x,Linf=coef(fit1)[1],K=coef(fit1)[2],t0=coef(fit1)[3]),from=0,to=5,
#'  col="red",lwd=10,add=TRUE)
#'
#'# Fitting the Francis paramaterization of the von B function
#'fit2 <- nls(tl~vb2(age,L1,L2,L3,t1=0,t3=5),data=SpotVA1,
#'  start=vbStarts(tl~age,data=SpotVA1,type="Francis",tFrancis=c(0,5)))
#'summary(fit2,correlation=TRUE)
#'## showing how the coefficients and t values can be sent to the first arguments
#'curve(vb2(x,L1=coef(fit2),t1=c(0,5)),from=0,to=5,col="blue",lwd=5,add=TRUE)
#'
#'# Fitting the Schnute parameterization of the von B function
#'vb3 <- vbFuns("Schnute")
#'fit3 <- nls(tl~vb3(age,L1,L2,K,t1=0,t2=4),data=SpotVA1,
#'  start=vbStarts(tl~age,data=SpotVA1,type="Schnute"))
#'summary(fit3,correlation=TRUE)
#'curve(vb3(x,L1=coef(fit3),t1=c(0,4)),from=0,to=5,col="green",lwd=2,add=TRUE)
#'
vbFuns <- function(type=c("typical","original","BevertonHolt","Fabens","Fabens2",
                          "Francis","GallucciQuinn","Laslett","Mooij","Schnute",
                          "Somers","Somers2","vonBertalanffy","Wang","Wang2"),
                   simple=FALSE,msg=FALSE) {
  typical <- BevertonHolt <- function(t,Linf,K=NULL,t0=NULL) {
        if (length(Linf)==3) {
          K <- Linf[2]
          t0 <- Linf[3]
          Linf <- Linf[1]
        } else if (length(Linf)!=1 | is.null(K) | is.null(t0)) {
          stop("One or more model parameters (Linf, K, t0) are missing or incorrect.",call.=FALSE)
        }
        Linf*(1-exp(-K*(t-t0)))
  }
  Stypical <- sBevertonHolt <- function(t,Linf,K,t0) {
        Linf*(1-exp(-K*(t-t0)))
  } 
  original <- vonBertalanffy <- function(t,Linf,L0=NULL,K=NULL) {
        if (length(Linf)==3) {
          L0 <- Linf[2]
          K <- Linf[3]
          Linf <- Linf[1]
        } else if (length(Linf)!=1 | is.null(L0) | is.null(K)) {
          stop("One or more model parameters (Linf, L0, K) are missing or incorrect.",call.=FALSE)
        }
        Linf-(Linf-L0)*exp(-K*t)
  } 
  Soriginal <- SvonBertalanffy <- function(t,Linf,L0,K) {
        Linf-(Linf-L0)*exp(-K*t)
  }
  GallucciQuinn <- function(t,omega,K=NULL,t0=NULL) {
        if (length(omega)==3) {
          K <- omega[2]
          t0 <- omega[3]
          omega <- omega[1]
        } else if (length(K)!=1 | is.null(t0) | is.null(omega)) {
          stop("One or more model parameters (K, t0, omega) are missing or incorrect.",call.=FALSE)
        }
        (omega/K)*(1-exp(-K*(t-t0)))
  }
  SGallucciQuinn <- function(t,omega,K,t0) {
        (omega/K)*(1-exp(-K*(t-t0)))
  }
  Mooij <- function(t,Linf,L0=NULL,omega=NULL) {
         if (length(Linf)==3) {
           L0 <- Linf[2]
           omega <- Linf[3]
           Linf <- Linf[1]
         } else if (length(Linf)!=1 | is.null(L0) | is.null(omega)) {
           stop("One or more model parameters (Linf, L0, omega) are missing or incorrect.",call.=FALSE)
         }
         Linf-(Linf-L0)*exp(-(omega/Linf)*t)
  }
  SMooij <- function(t,Linf,L0,omega) {
         Linf-(Linf-L0)*exp(-(omega/Linf)*t)
  }
  Schnute <- function(t,L1,L2=NULL,K=NULL,t1,t2=NULL) {
         if (length(L1)==3) {
           L2 <- L1[2]
           K <- L1[3]
           L1 <- L1[1]
         } else if (length(L1)!=1 | is.null(L2) | is.null(K)) {
           stop("One or more model parameters (L1, L2, K) are missing or incorrect.",call.=FALSE)
         }
         if (length(t1)==2) {
           t2 <- t1[2]
           t1 <- t1[1]
         } else if (length(t1)!=1 | is.null(t2)) {
           stop("One or more model definitions (t1, t2) are missing or incorrect.",call.=FALSE)
         }
         L1+(L2-L1)*((1-exp(-K*(t-t1)))/(1-exp(-K*(t2-t1))))
  }
  SSchnute <- function(t,L1,L2,K,t1,t2) {
         L1+(L2-L1)*((1-exp(-K*(t-t1)))/(1-exp(-K*(t2-t1))))
  }
  Francis <- function(t,L1,L2=NULL,L3=NULL,t1,t3=NULL) {
         if (length(L1)==3) {
           L2 <- L1[2]
           L3 <- L1[3]
           L1 <- L1[1]
         } else if (length(L1)!=1 | is.null(L2) | is.null(L3)) {
           stop("One or more model parameters (L1, L2, L3) are missing or incorrect.",call.=FALSE)
         }
         if (length(t1)==2) {
           t3 <- t1[2]
           t1 <- t1[1]
         } else if (length(t1)!=1 | is.null(t3)) {
           stop("One or more model definitions (t1, t3) are missing or incorrect.",call.=FALSE)
         }
         r <- (L3-L2)/(L2-L1)
         L1+(L3-L1)*((1-r^(2*((t-t1)/(t3-t1))))/(1-r^2))
  }
  SFrancis <- function(t,L1,L2,L3,t1,t3) {
         r <- (L3-L2)/(L2-L1)
         L1+(L3-L1)*((1-r^(2*((t-t1)/(t3-t1))))/(1-r^2))
  }
  Somers <- function(t,Linf,K,t0,C,ts) {
         if (length(Linf)==5) {
           K <- Linf[2]
           t0 <- Linf[3]
           C <- Linf[4]
           ts <- Linf[5]
           Linf <- Linf[1]
         } else if (length(Linf)!=1 | is.null(K) | is.null(t0) | is.null(C) | is.null(ts)) {
           stop("One or more model parameters (Linf, K, t0, C, ts) are missing or incorrect.",call.=FALSE)
         }
         St <- (C*K)/(2*pi)*sin(2*pi*(t-ts))
         Sto <- (C*K)/(2*pi)*sin(2*pi*(t0-ts))
         Linf*(1-exp(-K*(t-t0)-St+Sto))
  }
  SSomers <- function(t,Linf,K,t0,C,ts) {
         Linf*(1-exp(-K*(t-t0)-(C*K)/(2*pi)*sin(2*pi*(t-ts))+(C*K)/(2*pi)*sin(2*pi*(t0-ts))))
  }
  Somers2 <- function(t,Linf,K,t0,C,WP) {
         if (length(Linf)==5) {
           K <- Linf[2]
           t0 <- Linf[3]
           C <- Linf[4]
           WP <- Linf[5]
           Linf <- Linf[1]
         } else if (length(Linf)!=1 | is.null(K) | is.null(t0) | is.null(C) | is.null(WP)) {
           stop("One or more model parameters (Linf, K, t0, C,WP) are missing or incorrect.",call.=FALSE)
         }
         Rt <- (C*K)/(2*pi)*sin(2*pi*(t-WP+0.5))
         Rto <- (C*K)/(2*pi)*sin(2*pi*(t0-WP+0.5))
         Linf*(1-exp(-K*(t-t0)-Rt+Rto))
  }
  SSomers2 <- function(t,Linf,K,t0,C,WP) {
         Linf*(1-exp(-K*(t-t0)-(C*K)/(2*pi)*sin(2*pi*(t-WP+0.5))+(C*K)/(2*pi)*sin(2*pi*(t0-WP+0.5))))
  }
  Fabens <- function(Lm,dt,Linf,K) {
         if (length(Linf)==2) {
           K <- Linf[2]
           Linf <- Linf[1]
         } else if (length(Linf)!=1 | is.null(K)) {
           stop("One or more model parameters (Linf, K) are missing or incorrect.",call.=FALSE)
         }
         Lm+(Linf-Lm)*(1-exp(-K*dt))
  }
  SFabens <- function(Lm,dt,Linf,K) {
         Lm+(Linf-Lm)*(1-exp(-K*dt))
  }
  Fabens2 <- function(Lm,dt,Linf,K) {
         if (length(Linf)==2) {
           K <- Linf[2]
           Linf <- Linf[1]
         } else if (length(Linf)!=1 | is.null(K)) {
           stop("One or more model parameters (Linf, K) are missing or incorrect.",call.=FALSE)
         }
         (Linf-Lm)*(1-exp(-K*dt))
  }
  SFabens2 <- function(Lm,dt,Linf,K) {
         (Linf-Lm)*(1-exp(-K*dt))
  }
  Wang <- function(Lm,dt,Linf,K,b) {
         if (length(Linf)==3) {
           b <- Linf[3]
           K <- Linf[2]
           Linf <- Linf[1]
         } else if (length(Linf)!=1 | is.null(K) | is.null(b)) {
           stop("One or more model parameters (Linf, K, b) are missing or incorrect.",call.=FALSE)
         }
         (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
  }
  SWang <- function(Lm,dt,Linf,K,b) {
         (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
  }
  Wang2 <- function(Lm,dt,K,a,d) {
         if (length(K)==3) {
           d <- K[3]
           a <- K[2]
           K <- K[1]
         } else if (length(K)!=1 | is.null(a) | is.null(d)) {
           stop("One or more model parameters (K, a, d) are missing or incorrect.",call.=FALSE)
         }
         (a+d*Lm)*(1-exp(-K*dt))
  }
  SWang2 <- function(Lm,dt,K,a,d) {
         (a+d*Lm)*(1-exp(-K*dt))
  }  
  Laslett <- function(t,Linf,K1,K2,t0,a,b) {
         if (length(Linf)==6) { 
           K1 <- Linf[2]
           K2 <- Linf[3]
           t0 <- Linf[4]
           a <- Linf[5]
           b <- Linf[6]
           Linf <- Linf[1]
         } else if (length(Linf)!=1 | is.null(K1) | is.null(K2) | is.null(t0) | is.null(a) | is.null(b)) {
           stop("One or more model parameters (Linf, K1, K2, t0, a, b) are missing or incorrect.",call.=FALSE)
         } 
         Linf*(1-exp(-K2*(t-t0))*((1+exp(-b*(t-t0-a)))/(1+exp(a*b)))^(-(K2-K1)/b))
  }
  SLaslett <- function(t,Linf,K1,K2,t0,a,b) {
         Linf*(1-exp(-K2*(t-t0))*((1+exp(-b*(t-t0-a)))/(1+exp(a*b)))^(-(K2-K1)/b))
  }
  type <- match.arg(type)
  if (msg) {
    switch(type,
      typical=,BevertonHolt= {
        cat("You have chosen the 'traditional' or 'Beverton-Holt' von Bertalanffy parameterization.\n\n")
        cat("  E[L|t] = Linf*(1-exp(-K*(t-to)))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      K = exponential rate of approach to Linf\n")
        cat("      t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
      },
      original=,vonBertalanffy={
        cat("You have chosen the 'original' von Bertalanffy parameterization.\n\n")
        cat("  E[L|t] = Linf-(Linf-L0)*exp(-K*t)\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      L0 = the mean length at age-0 (i.e., hatching or birth)\n")
        cat("      K = exponential rate of approach to Linf\n\n")
      },
      Francis={
        cat("You have chosen the 'Francis' von Bertalanffy parameterization.\n\n")
        cat("  E[L|t] = L1+(L3-L1)*[(1-r^(2*[(t-t1)/(t3-t1)]))/(1-r^2)]\n\n")
        cat("where r = [(L3-L2)/(L2-L1)] and\n\n")
        cat("where L1 = the mean length at the first (small) reference age\n")
        cat("      L2 = the mean length at the intermediate reference age\n")
        cat("      L3 = the mean length at the third (large) reference age\n\n")
        cat("You must also supply the constant values (i.e., they are NOT model parameters) for\n")
        cat("      t1 = the first (usually a younger) reference age\n")
        cat("      t3 = the third (usually an older) reference age\n\n")
      },
      GallucciQuinn={
        cat("You have chosen the 'Gallucci and Quinn (1979)' von Bertalanffy parameterization.\n\n")
        cat("  E[L|t] = [omega/K]*(1-exp(-K*(t-to)))\n\n")
        cat("where omega = growth rate near t0\n")
        cat("      K = exponential rate of approach to Linf\n")
        cat("      t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
      },
      Mooij={
        cat("You have chosen the 'Mooij et al. (1999)' von Bertalanffy parameterization.\n\n")
        cat("  E[L|t] = Linf-(Linf-L0)*exp(-(omega/Linf)*t)\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      L0 = the mean length at age-0 (i.e., hatching or birth)\n")
        cat("      omega = growth rate near L0\n\n")
      },
      Schnute={
        cat("You have chosen the 'Schnute (1981)' von Bertalanffy parameterization.\n\n")
        cat("  E[L|t] = L1+(L2-L1)*[(1-exp(-K*(t-t1)))/(1-exp(-K*(t2-t1)))]\n\n")
        cat("where L1 = the mean length at the youngest age in the sample\n")
        cat("      L2 = the mean length at the oldest age in the sample\n")
        cat("      K = exponential rate of approach to Linf\n\n")
        cat("You must also supply the constant values (i.e., they are NOT model parameters) for\n")
        cat("      t1 = the youngest age in the sample\n")
        cat("      t2 = the oldest age in the sample\n\n")
      },
      Somers={
        cat("You have chosen the 'Somers Seasonal' von Bertalanffy parameterization.\n\n")
        cat("  E[L|t] = Linf*(1-exp(-K*(t-to)-St+St0))\n\n")
        cat("where St = (CK/2*pi)*sin(2*pi*(t-ts)) and\n")
        cat("where St0 = (CK/2*pi)*sin(2*pi*(t0-ts)) and\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      K = exponential rate of approach to Linf\n")
        cat("      t0 = the theoretical age when length = 0 (a modeling artifact)\n")
        cat("      C = proportional growth depression at 'winter peak'\n")
        cat("      ts = time from t=0 until the first growth oscillation begins.\n\n")
      },
      Somers2={
        cat("You have chosen the 'Modified Somers Seasonal' von Bertalanffy parameterization.\n\n")
        cat("  E[L|t] = Linf*(1-exp(-K*(t-to)-Rt+Rt0))\n\n")
        cat("where Rt = (CK/2*pi)*sin(2*pi*(t-WP+0.5)) and\n")
        cat("where Rt0 = (CK/2*pi)*sin(2*pi*(t0-WP+0.5)) and\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      K = exponential rate of approach to Linf\n")
        cat("      t0 = the theoretical age when length = 0 (a modeling artifact)\n")
        cat("      C = proportional growth depression at 'winter peak'\n")
        cat("      WP = the 'winter peak' (point of slowest growth).\n\n")
      },
      Fabens={
        cat("You have chosen the 'Fabens' von Bertalanffy parameterization for tag-return data.\n\n")
        cat("  E[Lr|Lm,dt] = Lm + (Linf-Lm)*(1-exp(-K*dt))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      K = exponential rate of approach to Linf\n\n")
        cat("and the data are Lr = length at time of recapture\n")
        cat("                 Lm = length at time of marking\n")
        cat("                 dt = time between marking and recapture.\n\n")
      },
      Wang={
        cat("You have chosen the 'Wang' von Bertalanffy parameterization for tag-return data.\n\n")
        cat("  E[Lr-Lm|Lm,dt] = (Linf+b(Lm-E(Lm))-Lm)*(1-exp(-K*dt))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      K = exponential rate of approach to Linf\n")
        cat("      b = parameters\n\n")
        cat("and the data are Lr = length at time of recapture\n")
        cat("                 Lm = length at time of marking\n")
        cat("                 dt = time between marking and recapture.\n\n")
        cat("and with E(Lm) = expectation (i.e., mean) of Lm.\n\n")
      },
      Wang2={
        cat("You have chosen the 'Wang2' von Bertalanffy parameterization for tag-return data.\n\n")
        cat("  E[Lr-Lm|Lm,dt] = (a+dLm)*(1-exp(-K*dt))\n\n")
        cat("where K = exponential rate of approach to Linf\n")
        cat("      a, b = parameters\n\n")
        cat("and the data are Lr = length at time of recapture\n")
        cat("                 Lm = length at time of marking\n")
        cat("                 dt = time between marking and recapture.\n\n")
        cat("and with E(Lm) = expectation (i.e., mean) of Lm.\n\n")
      },
      Laslett={
        cat("You have chosen the 'Laslett' 'double' von Bertalanffy parameterization.\n\n")
        cat(" E[L|t] = Linf*[1-exp(-K2*(t-to))((1+exp(-b(t-t0-a)))/(1+exp(ab)))^(-(K2-K1)/b)]\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      t0 = the theoretical age when length = 0 (a modeling artifact)\n")
        cat("      K1 = the first (younger ages) exponential rate of approach to Linf\n")
        cat("      K2 = the second (older ages) exponential rate of approach to Linf\n")
        cat("      b = governs the rate of transition from K1 to K2\n")
        cat("      a = the central age of the transition from K1 to K2\n\n")
      }
    )
  }
  if (simple) type <- paste("S",type,sep="")
  get(type)
}
