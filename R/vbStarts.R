#' @title Find reasonable starting values for a von Bertalanffy model.
#' 
#' @description Finds reasonable starting values for the parameters in a specific von Bertalanffy model parameterization.
#' 
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of the von Bertalanffy growth model.  There is no guarantee that these starting values are the \sQuote{best} starting values.  One should use them with caution and should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#' 
#' The Linf and K paramaters are estimated via the Ford-Walford plot (see \code{\link{walfordPlot}}.  The product of the starting values for Linf and K is used as a starting value for omega in the GallucciQuinn and Mooij parameterizations.  The result of log(2) divided by the starting value for K is used as the starting value for t50 in the Weisberg parameterization.
#' 
#' If \code{meth0="yngAge"} then a starting value for t0 or L0 is found by algebraically solving the typical or original paramaterizations, respectively, for t0 or L0 using the first age with more than one data point.  If \code{meth0="poly"} then a second-degree polynomial model is fit to the mean length-at-age data.  The t0 starting value is set equal to the root of the polynomial that is closest to zero.  The L0 starting value is set equal to the mean length at age-0 predicted from the polynomial.
#' 
#' Starting values for the L1 and L3 parameters in the Schnute paramaterization and the L1, L2, and L3 parameters in the Francis parameterization can be found in two ways.  If \code{methEV="poly"} then the starting values are the predicted length-at-age from a second-degree polynomial fit to the mean length-at-age data.  If \code{methEV="means"} then the observed sample means at the corresponding ages are used.  In the case where one of the supplied ages is fractional then the value returned will be linearly interpolated between the mean lengths of the two closest ages.  The ages to be used for L1 and L3 in the Schnute and Francis parameterizations are supplied as a numeric vector of length 2 in \code{ages2use=}.  If \code{ages2use=NULL} then the minimum and maximum observed ages will be used.  In the Francis method, L2 will correspond to the age half-way between the two ages in \code{ages2use=}.  A warning will be given if L2<L1 for the Schnute method or if L2<L1 or L3<L2 for the Francis method.
#' 
#' @aliases vbStarts
#' 
#' @param formula A formula of the form \code{len~age}.
#' @param data A data frame that contains the variables in \code{formula}.
#' @param type A string that indicates the parameterization of the von Bertalanffy model.
#' @param ages2use A numerical vector of the two ages to be used in the Schnute or Francis paramaterizations.  See details.
#' @param methEV A string that indicates how the lengths of the two ages in the Schnute paramaterization or the three ages in the Francis paramaterization should be derived.  See details.
#' @param meth0 A string that indicates how the t0 and L0 paramaters should be derived.  See details.
#' @param plot A logical that indicates whether a plot of the data with the model fit at the starting values superimposed should be created.
#' @param \dots Further arguments passed to the methods.
#' 
#' @return A list that contains reasonable starting values.  Note that the parameters will be listed in the same order and with the same names as listed in \code{\link{vbFuns}}.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @note The \sQuote{original} and \sQuote{vonBertalanffy} and the \sQuote{typical} and \sQuote{BevertonHolt} parameterizations are synonymous.
#' 
#' @seealso See \code{\link{growthModels}} and \code{\link{vbModels}} for a list of models and parameterizations used in \pkg{FSA}, \code{\link{vbFuns}} for functions that represent the von Bertalanffy parameterizations, \code{\link{walfordPlot}} for a common method to estimate Linf and K, and \code{\link{growthModelSim}} for a graphical method to determine starting values.
#' 
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffy.pdf}, \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffyExtra.pdf}
#' 
#' @references Francis, R.I.C.C.  1988.  Are growth parameters estimated from tagging and age-length data comparable?  Canadian Journal of Fisheries and Aquatic Sciences, 45:936-942.
#' 
#' Gallucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing, fitting, and testing a simple growth model.  Transactions of the American Fisheries Society, 108:14-25.
#' 
#' Garcia-Berthou, E., G. Carmona-Catot, R. Merciai, and D.H. Ogle.  \href{https://www.researchgate.net/publication/257658359_A_technical_note_on_seasonal_growth_models}{A technical note on seasonal growth models.}  Reviews in Fish Biology and Fisheries 22:635-640.
#' 
#' Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.  Analysis and comparison of fish growth from small samples of length-at-age data: Detection of sexual dimorphism in Eurasian perch as an example.  Transactions of the American Fisheries Society 128:483-490.
#' 
#' Schnute, J.  1981.  A versatile growth model with statistically stable parameters. Canadian Journal of Fisheries & Aquatic Sciences, 38:1128-1140.
#' 
#' Somers, I. F. 1988. \href{http://www.worldfishcenter.org/Naga/na_2914.pdf}{On a seasonally oscillating growth function.} Fishbyte 6(1):8-11.
#'
#' Weisberg, S., G.R. Spangler, and L. S. Richmond. 2010. Mixed effects models for fish growth. Canadian Journal of Fisheries And Aquatic Sciences 67:269-277.
#' 
#' @keywords manip
#' @examples
#' ## Simple Examples
#' data(SpotVA1)
#' vbStarts(tl~age,data=SpotVA1)
#' vbStarts(tl~age,data=SpotVA1,type="GallucciQuinn")
#' vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5))
#' vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,5))
#' 
#' ## Simple Example with a Plot
#' vbStarts(tl~age,data=SpotVA1,plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="GallucciQuinn",plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5),plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,5),plot=TRUE)
#' 
#' ## See examples in vbFuns() for use of vbStarts() when fitting Von B models
#' 
#' @export vbStarts
vbStarts <- function(formula,data=NULL,
                     type=c("typical","BevertonHolt","original","vonBertalanffy",
                            "GQ","GallucciQuinn","Mooij","Weisberg",
                            "Schnute","Francis","Somers","Somers2"),
                     ages2use=NULL,methEV=c("poly","means"),meth0=c("poly","yngAge"),
                     plot=FALSE,...) {
  # some checks and handle the formula
  type <- match.arg(type)
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) stop("'vbStarts' must have only one LHS variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'vbStarts' must have only one RHS variable.",call.=FALSE)
  if (!tmp$Eclass %in% c("numeric","integer")) stop("RHS variable must be numeric.",call.=FALSE)
  # get the length and age vectors
  len <- tmp$mf[,tmp$Rname[1]]
  age <- tmp$mf[,tmp$Enames[1]]
  # attempting to get by bindings warning in RCMD CHECK
  x <- NULL
  # mean lengths-at-age
  meanL <- tapply(len,age,mean)
  # ages represented
  ages <- as.numeric(names(meanL))
  ns <- tapply(len,age,length)
  # find starting values for K & Linf from Walford plot regression
  lmK <- lm(meanL[-1]~meanL[-length(meanL)])
  sK <- -log(coef(lmK)[2])
  sLinf <- coef(lmK)[1]/(1-coef(lmK)[2])
  # fit polynomial regression
  respoly <- lm(meanL~poly(ages,2))
  if(match.arg(meth0)=="poly") {
    # get real component of roots to polynomial equation
    resroots <- Re(polyroot(coef(respoly)))
    # find starting value for t0 as polynomial root closest to zero
    st0 <- resroots[which(abs(resroots)==min(abs(resroots)))]
    # find starting value for L0 as predicted value from polynomial at age=0
    sL0 <- predict(respoly,data.frame(ages=0))
  } else {
    # find the youngest age with a n>1
    yngAge <- min(ages[which(ns>1)])
    # find starting values for t0 from re-arrangement of typical VonB model and yngAge
    st0 <- yngAge+(1/sK)*log((sLinf-meanL[yngAge])/sLinf)
    # find starting values for L0 from re-arrangement of original VonB model and yngAge
    sL0 <- sLinf+(meanL[yngAge]-sLinf)/exp(-sK*yngAge)
  }
  # if a "double root" was found then reduce to just a single root
  if (length(st0)>1) st0 <- st0[1]
  if (length(sL0)>1) sL0 <- sL0[1]
  # strip attributes (names mostly)
  attributes(sLinf) <- attributes(sK) <- attributes(st0) <- attributes(sL0) <- attributes(meanL) <- NULL
  type <- match.arg(type)
  if (!(type %in% c("Schnute","Francis"))) iCheckKLinf(sK,sLinf,type,len)
  switch(type,
    typical=,BevertonHolt={ sv <- list(Linf=sLinf,K=sK,t0=st0) },
    original=,vonBertalanffy={ sv <- list(Linf=sLinf,L0=sL0,K=sK) },
    GQ=,GallucciQuinn={ sv <- list(omega=sLinf*sK,K=sK,t0=st0) },
    Mooij={ sv <- list(Linf=sLinf,L0=sL0,omega=sLinf*sK) },
    Weisberg={ sv <- list(Linf=sLinf,t50=log(2)/sK+st0,t0=st0) },
    Schnute=,Francis={
      if (is.null(ages2use)) ages2use <- range(ages)
      if (length(ages2use)!=2) stop("'age2use=' must be NULL or have only two ages.",call.=FALSE)
      if (ages2use[2]<=ages2use[1]) {
        warning("'ages2use' should be in ascending order; order reversed to continue.",call.=FALSE)
        ages2use <- rev(ages2use)
      }
      if (type=="Francis") ages2use <- c(ages2use[1],mean(ages2use),ages2use[2])
      switch(match.arg(methEV),
        poly={ vals <- predict(respoly,data.frame(ages=ages2use)) },
        means={
          meanFnx <- approxfun(ages,meanL)
          vals <- meanFnx(ages2use)
        }
      ) # end 'methEv' switch
      if (any(diff(vals)<=0)) warning("At least one of the starting values for an older age\n  is smaller than the starting value for a younger age.",call.=FALSE)
      attributes(vals) <- NULL
      ifelse(type=="Schnute",sv <- list(L1=vals[1],L3=vals[2],K=sK),sv <- list(L1=vals[1],L2=vals[2],L3=vals[3]))
    },
    Somers={ sv <- list(Linf=sLinf,K=sK,t0=st0,C=0.9,ts=0.1)   },
    Somers2={ sv <- list(Linf=sLinf,K=sK,t0=st0,C=0.9,WP=0.9)   }
  ) # end 'type' switch
  if (plot) {
    # create a transparency value that is the max of 1/2th of the maximum number at any length and age combination or 0.1
    clr <- rgb(0,0,0,max(2/max(table(age,len)),0.1))
    plot(len~age,pch=16,col=clr,xlab="Age",ylab="Length",
         main=paste("von B (",type," paramaterization) fit at initial values",sep=""))
    mdl <- vbFuns(type)
    if (!(type %in% c("Schnute","Francis"))) curve(mdl(x,sv[[1]],sv[[2]],sv[[3]]),from=min(age),to=max(age),col="red",lwd=3,add=TRUE)
      else if (type=="Schnute") curve(mdl(x,sv[[1]],sv[[2]],sv[[3]],t1=ages2use[1],t3=ages2use[2]),from=min(age),to=max(age),col="red",lwd=3,add=TRUE)
        else {
          curve(mdl(x,sv[[1]],sv[[2]],sv[[3]],t1=ages2use[1],t3=ages2use[2]),from=min(age),to=max(age),col="red",lwd=3,add=TRUE)
        }
    legend("bottomright",legend=paste(names(sv),formatC(unlist(sv),format="f",digits=2),sep=" = "),bty="n")
  }
  # return starting values list
  sv
}

iCheckKLinf <- function(sK,sLinf,type,len) {
  if (sK<0) {
    if (type %in% c("typical","original","BevertonHolt","vonBertalanffy","GQ","GallucciQuinn","Schnute")) {
      msg <- "The suggested starting value for K is negative, "
    } else {
      msg <- "One  suggested starting value is based on a negative K, "
    }
    msg <- paste(msg,"which suggests a model fitting problem.\n",sep="")
    msg <- paste(msg,"See a walfordPlot or chapmanPlot to examine the problem.\n",sep="")
    msg <- paste(msg,"Consider manually setting K=0.3 in the starting value list.\n",sep="")
    warning(msg,call.=FALSE)
  }
  if ((sLinf<0.5*max(len)) | sLinf>1.5*max(len)) {
    msg <- "Starting value for Linf is very different from the observed maximum length, "
    msg <- paste(msg,"which suggests a model fitting problem.\n",sep="")
    msg <- paste(msg,"See a walfordPlot or chapmanPlot to examine the problem.\n",sep="")
    msg <- paste(msg,"Consider manually setting Linf to the maximum observed length\n",sep="")
    msg <- paste(msg,"in the starting value list.\n",sep="")
    warning(msg,call.=FALSE)    
  } 
}