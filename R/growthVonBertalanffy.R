#' @name growthVonBertalanffy
#' 
#' @title Creates a function for a specific parameterizations of the von Bertalanffy growth function.
#'
#' @description Creates a function for a specific parameterizations of the von Bertalanffy growth function.  Use \code{vbModels()} to see the equations for each model.
#'
#' @param type A string that indicates the parameterization of the von Bertalanffy growth function.
#' @param simple A logical that indicates whether the user should be allowed to send all parameter values in the first parameter argument (\code{=FALSE}; default) or whether all individual parameters must be specified (\code{=TRUE}).
#' @param msg A logical that indicates whether a message about the function and parameter definitions should be output (\code{=TRUE}) or not (\code{=FALSE}; default).
#' @param \dots Not implemented.
#' 
#' @return \code{vbFuns} returns a function that can be used to predict fish length given a vector of ages and values for the function parameters and, in some parameterizations, values for some constants.  The result should be saved to an object that can then be used as a function name.  When the resulting function is used the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (if \code{msg=TRUE}).  If \code{simple=FALSE} then the values for all parameters may be included as a vector in the first parameter argument.  Similarly, the values for all constants may be included as a vector in the first constant argument (i.e., \code{t1}).  If \code{simple=TRUE} then all parameters and constants must be declared individually.  The resulting function is somewhat easier to read when \code{simple=TRUE}.
#' 
#' \code{vbModels} returns a graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @note The \sQuote{original} and \sQuote{vonBertalanffy} and the \sQuote{typical} and \sQuote{BevertonHolt} versions are synonymous.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth}.
#'
#' @seealso See \code{\link{gompFuns}}, \code{\link{logisticFuns}}, and \code{\link{schnute}} for similar functionality for other models.  See \code{\link{vbStarts}} for methods to find starting values.
#'
#' @references Ogle, D.H.  2016.  Introductory Fisheries Analyses with R.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Fabens, A. 1965. Properties and fitting of the von Bertalanffy growth curve. Growth 29:265-289.
#'
#' Francis, R.I.C.C.  1988.  Are growth parameters estimated from tagging and age-length data comparable?  Canadian Journal of Fisheries and Aquatic Sciences, 45:936-942.
#'
#' Gallucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing, fitting, and testing a simple growth model.  Transactions of the American Fisheries Society, 108:14-25.
#'
#' Garcia-Berthou, E., G. Carmona-Catot, R. Merciai, and D.H. Ogle.  \href{https://www.researchgate.net/publication/257658359_A_technical_note_on_seasonal_growth_models}{A technical note on seasonal growth models.}  Reviews in Fish Biology and Fisheries 22:635-640.
#' 
#' Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.  Analysis and comparison of fish growth from small samples of length-at-age data: Detection of sexual dimorphism in Eurasian perch as an example.  Transactions of the American Fisheries Society 128:483-490.
#'
#' Polacheck, T., J.P. Eveson, and G.M. Laslett.  2004.  Increase in growth rates of southern bluefin tuna (\emph{Thunnus maccoyii}) over four decades: 1960 to 2000.  Canadian Journal of Fisheries and Aquatic Sciences, 61:307-322.
#'
#' Schnute, J.  1981.  A versatile growth model with statistically stable parameters. Canadian Journal of Fisheries and Aquatic Sciences, 38:1128-1140.
#'
#' Somers, I. F. 1988. \href{http://www.worldfishcenter.org/Naga/na_2914.pdf}{On a seasonally oscillating growth function.} Fishbyte 6(1):8-11.
#' 
#' Vaughan, D. S. and T. E. Helser.  1990.  \href{http://docs.lib.noaa.gov/noaa_documents/NMFS/SEFSC/TM_NMFS_SEFSC/NMFS_SEFSC_TM_263.pdf}{Status of the red drum stock of the Atlantic coast: Stock assessment report for 1989}.  NOAA Technical Memorandum NMFS-SEFC-263, 117 p.
#'
#' Wang, Y.-G.  1998.  An improved Fabens method for estimation of growth parameters in the von Bertalanffy model with individual asymptotes.  Canadian Journal of Fisheries and Aquatic Sciences 55:397-400.
#'
#' Weisberg, S., G.R. Spangler, and L. S. Richmond. 2010. Mixed effects models for fish growth. Canadian Journal of Fisheries And Aquatic Sciences 67:269-277.
#'
#' @keywords manip hplot
#'
#' @examples
#' ## See the formulae
#' \dontrun{windows(8,5)}
#' vbModels()
#' \dontrun{windows(6,5)}
#' vbModels("seasonal")
#' vbModels("tagging")
#' 
#' ## Simple Examples
#' ( vb1 <- vbFuns() )              # typical parameterization
#' ages <- 0:20
#' plot(vb1(ages,Linf=20,K=0.3,t0=-0.2)~ages,type="b",pch=19)
#'
#' ( vb2 <- vbFuns("Francis") )     # Francis parameterization
#' plot(vb2(ages,L1=10,L2=19,L3=20,t1=2,t3=18)~ages,type="b",pch=19)
#'
#' ( vb2c <- vbFuns("Francis",simple=TRUE) )   # compare to vb2
#'
#' ## Examples of fitting Von B models
#' ##   After the last example a plot is constructed with three lines on top of each
#' ##   other illustrating that the parameterizations all produce the same fitted
#' ##   values.  However, observe the correlations in the summary() results.
#'
#' # Fitting the typical paramaterization of the von B function
#' data(SpotVA1)
#' fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,start=vbStarts(tl~age,data=SpotVA1))
#' summary(fit1,correlation=TRUE)
#' plot(tl~age,data=SpotVA1,pch=19)
#' curve(vb1(x,Linf=coef(fit1)),from=0,to=5,col="red",lwd=10,add=TRUE)
#'
#' # Fitting the Francis paramaterization of the von B function
#' fit2 <- nls(tl~vb2c(age,L1,L2,L3,t1=0,t3=5),data=SpotVA1,
#'             start=vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5)))
#' summary(fit2,correlation=TRUE)
#' curve(vb2c(x,L1=coef(fit2)[1],L2=coef(fit2)[2],L3=coef(fit2)[3],t1=0,t3=5),
#'       from=0,to=5,col="blue",lwd=5,add=TRUE)
#'
#' # Fitting the Schnute parameterization of the von B function
#' vb3 <- vbFuns("Schnute")
#' fit3 <- nls(tl~vb3(age,L1,L3,K,t1=0,t3=4),data=SpotVA1,
#'             start=vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,4)))
#' summary(fit3,correlation=TRUE)
#' curve(vb3(x,L1=coef(fit3),t1=c(0,4)),from=0,to=5,col="green",lwd=2,add=TRUE)
#'
NULL

#' @rdname growthVonBertalanffy
#' @export
vbFuns <- function(type=c("typical","BevertonHolt","original","vonBertalanffy",
                          "GQ","GallucciQuinn","Mooij","Weisberg",
                          "Schnute","Francis","Laslett","Polacheck",
                          "Somers","Somers2",
                          "Fabens","Fabens2","Wang","Wang2","Wang3"),
                   simple=FALSE,msg=FALSE) {
  typical <- BevertonHolt <- function(t,Linf,K=NULL,t0=NULL) {
  if (length(Linf)==3) { K <- Linf[[2]]
                         t0 <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf*(1-exp(-K*(t-t0)))
}
  Stypical <- SBevertonHolt <- function(t,Linf,K,t0) {
    Linf*(1-exp(-K*(t-t0)))
  }
  original <- vonBertalanffy <- function(t,Linf,L0=NULL,K=NULL) {
  if (length(Linf)==3) { L0 <- Linf[[2]]
                         K <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf-(Linf-L0)*exp(-K*t)
}
  Soriginal <- SvonBertalanffy <- function(t,Linf,L0,K) {
    Linf-(Linf-L0)*exp(-K*t)
  }
  GQ <- GallucciQuinn <- function(t,omega,K=NULL,t0=NULL) {
  if (length(omega)==3) { K <- omega[[2]]
                          t0 <- omega[[3]]
                          omega <- omega[[1]] }
  (omega/K)*(1-exp(-K*(t-t0)))
}
  SGQ <- SGallucciQuinn <- function(t,omega,K,t0) {
    (omega/K)*(1-exp(-K*(t-t0)))
  }
  Mooij <- function(t,Linf,L0=NULL,omega=NULL) {
  if (length(Linf)==3) { L0 <- Linf[[2]]
                         omega <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf-(Linf-L0)*exp(-(omega/Linf)*t)
}
  SMooij <- function(t,Linf,L0,omega) {
    Linf-(Linf-L0)*exp(-(omega/Linf)*t)
  }
  Weisberg <- function(t,Linf,t50=NULL,t0=NULL) {
  if (length(Linf)==3) { t50 <- Linf[[2]]
                         t0 <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0)))
}
  SWeisberg <- function(t,Linf,t50,t0) {
    Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0)))
  } 
  Schnute <- function(t,L1,L3=NULL,K=NULL,t1,t3=NULL) {
  if (length(L1)==3) { L3 <- L1[[2]]; K <- L1[[3]]; L1 <- L1[[1]] }
  if (length(t1)==2) { t3 <- t1[[2]]; t1 <- t1[[1]] }
  L1+(L3-L1)*((1-exp(-K*(t-t1)))/(1-exp(-K*(t3-t1))))
}
  SSchnute <- function(t,L1,L3,K,t1,t3) {
    L1+(L3-L1)*((1-exp(-K*(t-t1)))/(1-exp(-K*(t3-t1))))
  }
  Francis <- function(t,L1,L2=NULL,L3=NULL,t1,t3=NULL) {
  if (length(L1)==3) { L2 <- L1[[2]]; L3 <- L1[[3]]; L1 <- L1[[1]] }
  if (length(t1)==2) { t3 <- t1[[2]]; t1 <- t1[[1]] }
  r <- (L3-L2)/(L2-L1)
  L1+(L3-L1)*((1-r^(2*((t-t1)/(t3-t1))))/(1-r^2))
}
  SFrancis <- function(t,L1,L2,L3,t1,t3) {
    r <- (L3-L2)/(L2-L1)
    L1+(L3-L1)*((1-r^(2*((t-t1)/(t3-t1))))/(1-r^2))
  }
  Laslett <- Polacheck <- function(t,Linf,K1,K2,t0,a,b) {
    if (length(Linf)==6) { K1 <- Linf[[2]]; K2 <- Linf[[3]]
    t0 <- Linf[[4]]; a <- Linf[[5]]
    b <- Linf[[6]]; Linf <- Linf[[1]] }
    Linf*(1-exp(-K2*(t-t0))*((1+exp(-b*(t-t0-a)))/(1+exp(a*b)))^(-(K2-K1)/b))
  }
  SLaslett <- SPolacheck <- function(t,Linf,K1,K2,t0,a,b) {
    Linf*(1-exp(-K2*(t-t0))*((1+exp(-b*(t-t0-a)))/(1+exp(a*b)))^(-(K2-K1)/b))
  }
  Somers <- function(t,Linf,K,t0,C,ts) {
  if (length(Linf)==5) { K <- Linf[[2]]; t0 <- Linf[[3]]
                         C <- Linf[[4]]; ts <- Linf[[5]]
                         Linf <- Linf[[1]] }
  St <- (C*K)/(2*pi)*sin(2*pi*(t-ts))
  Sto <- (C*K)/(2*pi)*sin(2*pi*(t0-ts))
  Linf*(1-exp(-K*(t-t0)-St+Sto))
}
  SSomers <- function(t,Linf,K,t0,C,ts) {
    Linf*(1-exp(-K*(t-t0)-(C*K)/(2*pi)*sin(2*pi*(t-ts))+(C*K)/(2*pi)*sin(2*pi*(t0-ts))))
  }
  Somers2 <- function(t,Linf,K,t0,C,WP) {
  if (length(Linf)==5) { K <- Linf[[2]]; t0 <- Linf[[3]]
                         C <- Linf[[4]]; WP <- Linf[[5]]
                         Linf <- Linf[[1]] }
  Rt <- (C*K)/(2*pi)*sin(2*pi*(t-WP+0.5))
  Rto <- (C*K)/(2*pi)*sin(2*pi*(t0-WP+0.5))
  Linf*(1-exp(-K*(t-t0)-Rt+Rto))
}
  SSomers2 <- function(t,Linf,K,t0,C,WP) {
    Linf*(1-exp(-K*(t-t0)-(C*K)/(2*pi)*sin(2*pi*(t-WP+0.5))+(C*K)/(2*pi)*sin(2*pi*(t0-WP+0.5))))
  }
  Fabens <- function(Lm,dt,Linf,K) {
  if (length(Linf)==2) { K <- Linf[[2]]; Linf <- Linf[[1]] }
  Lm+(Linf-Lm)*(1-exp(-K*dt))
}
  SFabens <- function(Lm,dt,Linf,K) {
    Lm+(Linf-Lm)*(1-exp(-K*dt))
  }
  Fabens2 <- function(Lm,dt,Linf,K) {
  if (length(Linf)==2) { K <- Linf[[2]]; Linf <- Linf[[1]] }
  (Linf-Lm)*(1-exp(-K*dt))
}
  SFabens2 <- function(Lm,dt,Linf,K) {
    (Linf-Lm)*(1-exp(-K*dt))
  }
  Wang <- function(Lm,dt,Linf,K,b) {
  if (length(Linf)==3) { b <- Linf[[3]]; K <- Linf[[2]]
                         Linf <- Linf[[1]] }
  (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
}
  SWang <- function(Lm,dt,Linf,K,b) {
    (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
  }
  Wang2 <- function(Lm,dt,K,a,b) {
    if (length(K)==3) { b <- K[[3]]; a <- K[[2]]; K <- K[[1]] }
    (a+b*Lm)*(1-exp(-K*dt))
  }
  SWang2 <- function(Lm,dt,K,a,b) {
    (a+b*Lm)*(1-exp(-K*dt))
  }
  Wang3 <- function(Lm,dt,K,a,b) {
    if (length(K)==3) { b <- K[[3]]; a <- K[[2]]; K <- K[[1]] }
    Lm+(a+b*Lm)*(1-exp(-K*dt))
  }
  SWang3 <- function(Lm,dt,K,a,b) {
    Lm+(a+b*Lm)*(1-exp(-K*dt))
  }
  type <- match.arg(type)
  if (msg) {
    switch(type,
      typical=,BevertonHolt= {
        message("You have chosen the 'typical' or 'BevertonHolt' parameterization.\n\n",
                "  E[L|t] = Linf*(1-exp(-K*(t-t0)))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           K = exponential rate of approach to Linf\n",
                "          t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
      },
      original=,vonBertalanffy={
        message("You have chosen the 'original' or 'vonBertalanffy` parameterization.\n\n",
                "  E[L|t] = Linf-(Linf-L0)*exp(-K*t)\n\n",
                "  where Linf = asymptotic mean length\n",
                "          L0 = the mean length at age-0 (i.e., hatching or birth)\n",
                "           K = exponential rate of approach to Linf\n\n")
      },
      Francis={
        message("You have chosen the 'Francis' parameterization.\n\n",
                "  E[L|t] = L1+(L3-L1)*[(1-r^(2*[(t-t1)/(t3-t1)]))/(1-r^2)]\n\n",
                "  where r = [(L3-L2)/(L2-L1)] and\n\n",
                "       L1 = the mean length at the first (small) reference age\n",
                "       L2 = the mean length at the intermediate reference age\n",
                "       L3 = the mean length at the third (large) reference age\n\n",
                "You must also supply the constant values (i.e., they are NOT model parameters) for\n",
                "       t1 = the first (usually a younger) reference age\n",
                "       t3 = the third (usually an older) reference age\n\n")
      },
      GQ=,GallucciQuinn={
        message("You have chosen the 'GQ' or 'GallucciQuinn' parameterization.\n\n",
                "  E[L|t] = [omega/K]*(1-exp(-K*(t-t0)))\n\n",
                "  where omega = growth rate near t0\n",
                "            K = exponential rate of approach to Linf\n",
                "           t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
      },
      Mooij={
        message("You have chosen the 'Mooij' parameterization.\n\n",
                "  E[L|t] = Linf-(Linf-L0)*exp(-(omega/Linf)*t)\n\n",
                "  where Linf = asymptotic mean length\n",
                "          L0 = the mean length at age-0 (i.e., hatching or birth)\n",
                "       omega = growth rate near L0\n\n")
      },
      Weisberg= {
        message("You have chosen the 'Weisberg' parameterization.\n\n",
                "  E[L|t] = Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0)))\n\n",
                "  where Linf = asymptotic mean length\n",
                "         t50 = age when half of Linf is reached\n",
                "          t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
      },
      Schnute={
        message("You have chosen the 'Schnute' parameterization.\n\n",
                "  E[L|t] = L1+(L2-L1)*[(1-exp(-K*(t-t1)))/(1-exp(-K*(t2-t1)))]\n\n",
                "  where L1 = the mean length at the youngest age in the sample\n",
                "        L2 = the mean length at the oldest age in the sample\n",
                "         K = exponential rate of approach to Linf\n\n",
                "  You must also supply the constant values (i.e., they are NOT model parameters) for\n",
                "        t1 = the youngest age in the sample\n",
                "        t2 = the oldest age in the sample\n\n")
      },
      Laslett=,Polacheck={
        message("You have chosen the 'Laslett/Polacheck' 'double' parameterization.\n\n",
                "  E[L|t] = Linf*[1-exp(-K2*(t-to))((1+exp(-b(t-t0-a)))/(1+exp(ab)))^(-(K2-K1)/b)]\n\n",
                "  where Linf = asymptotic mean length\n",
                "          t0 = the theoretical age when length = 0 (a modeling artifact)\n",
                "          K1 = the first (younger ages) exponential rate of approach to Linf\n",
                "          K2 = the second (older ages) exponential rate of approach to Linf\n",
                "           b = governs the rate of transition from K1 to K2\n",
                "           a = the central age of the transition from K1 to K2\n\n")
      },
      Somers={
        message("You have chosen the 'Somers Seasonal' parameterization.\n\n",
                "  E[L|t] = Linf*(1-exp(-K*(t-to)-St+St0))\n\n",
                "  where St = (CK/2*pi)*sin(2*pi*(t-ts)) and\n",
                "       St0 = (CK/2*pi)*sin(2*pi*(t0-ts)) and\n\n",
                "  and Linf = asymptotic mean length\n",
                "         K = exponential rate of approach to Linf\n",
                "        t0 = the theoretical age when length = 0 (a modeling artifact)\n",
                "         C = proportional growth depression at 'winter peak'\n",
                "        ts = time from t=0 until the first growth oscillation begins.\n\n")
      },
      Somers2={
        message("You have chosen the modified 'Somers2 Seasonal' parameterization.\n\n",
                "  E[L|t] = Linf*(1-exp(-K*(t-to)-Rt+Rt0))\n\n",
                "  where Rt = (CK/2*pi)*sin(2*pi*(t-WP+0.5)) and\n",
                "       Rt0 = (CK/2*pi)*sin(2*pi*(t0-WP+0.5)) and\n\n",
                "  and Linf = asymptotic mean length\n",
                "         K = exponential rate of approach to Linf\n",
                "        t0 = the theoretical age when length = 0 (a modeling artifact)\n",
                "         C = proportional growth depression at 'winter peak'\n",
                "        WP = the 'winter peak' (point of slowest growth).\n\n")
      },
      Fabens={
        message("You have chosen the 'Fabens' parameterization for tag-return data.\n\n",
                "  E[Lr|Lm,dt] = Lm + (Linf-Lm)*(1-exp(-K*dt))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           K = exponential rate of approach to Linf\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n")
      },
      Fabens2={
        message("You have chosen the 'Fabens2' parameterization for tag-return data.\n\n",
                "  E[Lr|Lm,dt] = (Linf-Lm)*(1-exp(-K*dt))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           K = exponential rate of approach to Linf\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n")
      },
      Wang={
        message("You have chosen the 'Wang' parameterization for tag-return data.\n\n",
                "  E[Lr-Lm|Lm,dt] = (Linf+b(Lm-E(Lm))-Lm)*(1-exp(-K*dt))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           K = exponential rate of approach to Linf\n",
                "           b = parameter\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n",
                "  and with E(Lm) = expectation (i.e., mean) of Lm.\n\n")
      },
      Wang2={
        message("You have chosen the 'Wang2' parameterization for tag-return data.\n\n",
                "  E[Lr-Lm|Lm,dt] = (a+bLm)*(1-exp(-K*dt))\n\n",
                "  where K = exponential rate of approach to Linf\n",
                "     a, b = parameters\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n")
      },
      Wang3={
        message("You have chosen the 'Wang3' parameterization for tag-return data.\n\n",
                "  E[Lr|Lm,dt] = Lm+(a+bLm)*(1-exp(-K*dt))\n\n",
                "  where K = exponential rate of approach to Linf\n",
                "     a, b = parameters\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n")
      }
    )
  }
  if (simple) type <- paste("S",type,sep="")
  get(type)
}


#' @rdname growthVonBertalanffy
#' @export
vbModels <- function(type=c("size","seasonal","tagging"),...) {
  ## Set some plotting parameters
  op <- par(mar=c(0,0,3,0),...)
  ## Check the type argument
  type <- match.arg(type)
  ## Show the models
  if (type=="size") {
    plot(1,type="n",ylim=c(0,7),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA von Bertalanffy Parameterizations")
    iGrowthModels("vbOriginal",0,6.0)
    iGrowthModels("vbTypical", 0,4.0)
    iGrowthModels("vbGQ",      0,2.0)
    iGrowthModels("vbMooij",   0,0.5)
    abline(v=0.5)
    iGrowthModels("vbWeisberg",0.50,6.0)
    iGrowthModels("vbSchnute", 0.50,4.0)
    iGrowthModels("vbFrancis", 0.50,2.0)
    iGrowthModels("vbFrancis2",0.65,0.5)
  } else if (type=="seasonal") {
    plot(1,type="n",ylim=c(0,6),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA von Bertalanffy Seasonal Parameterizations")
    iGrowthModels("vbSomers1", 0,5.5)
    iGrowthModels("vbSomers1a", 0.15,4.5)
    iGrowthModels("vbSomers2",  0,2.5)
    iGrowthModels("vbSomers2a", 0.15,1.5)
  } else {
    plot(1,type="n",ylim=c(0,7),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA von Bertalanffy Tag-Recapture Parameterizations")
    iGrowthModels("vbFabens1", 0,6.5)
    iGrowthModels("vbFabens2", 0,5)
    iGrowthModels("vbWang1",   0,3.5)
    iGrowthModels("vbWang2",   0,2.0)
    iGrowthModels("vbWang3",   0,0.5)
  }
  ## Return to the default plotting parameters
  par(op)
}

## Internal function for plotting the different models.  Send positions in xpos and ypos.
## Used in gompModels, logisticModels, RichardsModels, and SchnuteModels as well
iGrowthModels <- function(which,xpos,ypos) {
  switch(which,
         vbOriginal= {text(xpos,ypos,expression(plain("Original: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt}),pos=4)},
         vbTypical=  {text(xpos,ypos,expression(plain("Typical: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)},
         vbGQ=       {text(xpos,ypos,expression(plain("GQ: ")~~~E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)},
         vbMooij=    {text(xpos,ypos,expression(plain("Mooij: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t}),pos=4)},
         vbWeisberg= {text(xpos,ypos,expression(plain("Weisberg: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-frac(log(2),(K[0]~-~t[0]))*(t~-~t[0])},")")),pos=4)},
         vbSchnute=  {text(xpos,ypos,expression(plain("Schnute: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[3]~-~t[1])})),pos=4)},
         vbFrancis=  {text(xpos,ypos,expression(plain("Francis: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2})),pos=4)},
         vbFrancis2= {text(xpos,ypos,expression(plain("where" )~r==frac(L[3]-L[2],L[2]-L[1])),pos=4)},
         
         vbSomers1=  {text(xpos,ypos,expression(plain("Somers1: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-S(t)+S(t[0])},")")),pos=4)},
         vbSomers1a= {text(xpos,ypos,expression(plain("where" )~S(t)==bgroup("(",frac(C*K,2)*~pi,")")*~sin(2*pi*(t-t[s]))),pos=4)},
         vbSomers2=  {text(xpos,ypos,expression(plain("Somers2: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-R(t)+R(t[0])},")")),pos=4)},
         vbSomers2a= {text(xpos,ypos,expression(plain("where" )~R(t)==bgroup("(",frac(C*K,2)*~pi,")")*~sin(2*pi*(t-WP+0.5))),pos=4)},
         
         vbFabens1=  {text(xpos,ypos,expression(plain("Fabens1: ")~~~E(L[r]-L[m])==(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         vbFabens2=  {text(xpos,ypos,expression(plain("Fabens2: ")~~~E(L[r])==L[m]+(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         vbWang1=  {text(xpos,ypos,expression(plain("Wang1: ")~~~E(L[r]-L[m])==(L[infinity]+beta*(L[t]-L[t])-L[m])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         vbWang2=  {text(xpos,ypos,expression(plain("Wang2: ")~~~E(L[r]-L[m])==(alpha+beta*L[t])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         vbWang3=  {text(xpos,ypos,expression(plain("Wang3: ")~~~E(L[r])==L[m]+(alpha+beta*L[t])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         
         gOriginal=  {text(xpos,ypos,expression(plain("Original:         ")~~~E(L[t])==L[infinity]*~e^{-e^{a-g[i]*t}}),pos=4)},
         gRicker1=   {text(xpos,ypos,expression(plain("Ricker1:          ")~~~E(L[t])==L[infinity]*~e^{-e^{-g[i]*(t-t[i])}}),pos=4)},
         gRicker2=   {text(xpos,ypos,expression(plain("Ricker2, QD1: ")~~~E(L[t])==L[0]*~e^{a*bgroup("(",1-e^{-g[i]*t},")")}),pos=4)},
         gRicker3=   {text(xpos,ypos,expression(plain("Ricker3, QD2: ")~~~E(L[t])==L[infinity]*~e^{-a*~e^{-g[i]*t}}),pos=4)},
         gQD3=       {text(xpos,ypos,expression(plain("QD3:              ")~~~E(L[t])==L[infinity]*~e^{-~frac(1,g[i])*~e^{-g[i]*~(~t~-~t^{plain("*")})}}),pos=4)},
         
         CJ1=  {text(xpos,ypos,expression(plain("CJ1: ")~~~E(L[t])==frac(L[infinity],1+g[-infinity]*(t-t[i]))),pos=4)},
         CJ2=  {text(xpos,ypos,expression(plain("CJ2: ")~~~E(L[t])==frac(L[infinity],1+~ae^{-g[-infinity]*t})),pos=4)},
         Karkach= {text(xpos,ypos,expression(plain("Karkach: ")~~~E(L[t])==frac(L[0]*L[infinity],L[0]+(L[infinity]-L[0])*e^{-g[-infinity]*t})),pos=4)},
         
         Richards1=  {text(xpos,ypos,expression(plain("Richards1: ")~~~E(L[t])==L[infinity]*~bgroup("(",1-a*e^{-kt},")")^{b}),pos=4)},
         Richards2=  {text(xpos,ypos,expression(plain("Richards2: ")~~~E(L[t])==L[infinity]*~bgroup("(",1-frac(1,b)*~e^{-k*(t-t[i])},")")^{~b}),pos=4)},
         Richards3=  {text(xpos,ypos,expression(plain("Richards3: ")~~~E(L[t])==frac(L[infinity],bgroup("(",1+b*e^{-k*(t-t[i])},")")^{~frac(1,b)})),pos=4)},
         Richards4=  {text(xpos,ypos,expression(plain("Richards4: ")~~~E(L[t])==L[infinity]*~bgroup("(",1+(b-1)*~e^{-k*(t-t[i])},")")^{~frac(1,1-b)}),pos=4)},
         
         Schnute1=  {text(xpos,ypos,expression(plain("Case 1: ")~~~E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])}),"]")^{~frac(1,b)}),pos=4)},
         Schnute2=  {text(xpos,ypos,expression(plain("Case 2: ")~~~E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])})}),pos=4)},
         Schnute3=  {text(xpos,ypos,expression(plain("Case 3: ")~~~E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(~t~-~t[1],~t[3]~-~t[1]),"]")^{~frac(1,b)}),pos=4)},
         Schnute4=  {text(xpos,ypos,expression(plain("Case 4: ")~~~E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(~t~-~t[1],~t[3]~-~t[1])}),pos=4)}
  ) # end swich
} ## end iGrowthModels internal function
