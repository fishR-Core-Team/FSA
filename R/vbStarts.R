#'Finds reasonable starting values for the parameters in a specific von
#'Bertalanffy model parameterization.
#'
#'Finds reasonable starting values for the parameters in a specific von
#'Bertalanffy model parameterization.
#'
#'This function attempts to find reasonable starting values for a variety of
#'parameterizations of the von Bertalanffy growth model.  There is no guarantee
#'that these starting values are the \sQuote{best} starting values.  One should
#'use them with caution and should perform sensitivity analyses to determine
#'the impact of different starting values on the final model results.
#'
#'The Linf and K paramaters are estimated via the common method of a
#'Ford-Walford plot.  The product of the starting values for Linf and K is used
#'as a starting value for omega in the Gallucci and Quinn and Mooij et al.
#'methods.
#'
#'If \code{meth0="yngAge"} then a starting value for t0 is found by
#'algebraically solving the typical paramaterization model for t0 using the
#'first age with more than one data point.  If \code{meth0="poly"} then a
#'starting value for t0 is set equal to the root of the second-degree
#'polynomial model fit to the mean length-at-age data that is closest to zero.
#'
#'If \code{meth0="yngAge"} then a starting value for L0 is found by
#'algebraically solving the original paramaterization model for t0 using the
#'first age with more than one data point.  If \code{meth0="poly"} then a
#'starting value for L0 is set equal to the mean length at age-0 predicted from
#'the second-degree polynomial model fit to the mean length-at-age data.
#'
#'Starting values for the L1 and L2 parameters in the Schnute paramaterization
#'and the L1, L2, and L3 parameters in the Francis parameterization can be
#'found in two ways.  If \code{methEV="poly"} then the mean lengths are
#'predicted from a second-degree polynomial fit to the mean length-at-age data.
#'If \code{methEV="means"} then the observed sample means at the corresponding
#'ages are used.  In the case where one of the supplied ages is fractional then
#'the value returned will be linear interpolated between the mean lengths of
#'the two closest ages.  Note that the Schnute method always uses the minimum
#'and maximum ages to define L1 and L2, respectively, whereas the ages used for
#'the Francis method must be supplied as a numerical vector of length three in
#'\code{tFrancis}.  An error will occur if \code{methEV="means"} and
#'\code{tFrancis=NULL}.  In addition, a warning will be given if L2<L1 for the
#'Schnute method or if L2<L1 or L3<L2 for the Francis method.
#'
#'@aliases vbStarts vbStarts.default vbStarts.formula
#'@param age Either a vector of observed ages or a formula of the form
#'\code{len~age}.
#'@param len A vector of observed lengths.
#'@param data A data frame from which the vectors of observed ages and lengths
#'can be found if a formula is used.
#'@param type A string that indicates the parameterization of the von Bertalanffy #'model.
#'@param tFrancis A numerical vector of the three ages to be used in the
#'Francis paramaterization.  See details.
#'@param methEV A string that indicates how the lengths of the two ages in the
#'Schnute paramaterization or the three ages in the Francis paramaterization
#'should be derived.  See details.
#'@param meth0 A string that indicates how the t0 and L0 paramaters should be
#'derived.  See details.
#'@param plot A logical that indicates whether a plot of the data with the model
#'fit at the starting values superimposed should be created.
#'@param \dots Further arguments passed to the methods.
#'@return A list that contains reasonable starting values.  Note that the
#'parameters will be listed in the same order and with the same names as listed
#'in \code{\link{vbFuns}}.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@note The \sQuote{original} and \sQuote{vonBertalanffy} and the
#'\sQuote{typical} and \sQuote{BevertonHolt} versions are synonomous.
#'@seealso \code{\link{growthModels}}, \code{\link{vbFuns}}, and \code{\link{growthModelSim}}
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffy.pdf},
#'\url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffyExtra.pdf}
#'@references Francis, R.I.C.C.  1988.  Are growth parameters estimated from
#'tagging and age-length data comparable?  Canadian Journal of Fisheries and
#'Aquatic Sciences, 45:936-942.
#'
#'Gallucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing, fitting, and
#'testing a simple growth model.  Transactions of the American Fisheries
#'Society, 108:14-25.
#'
#'Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.  Analysis and
#'comparison of fish growth from small samples of length-at-age data: Detection
#'of sequal dimorphism in Eurasian perch as an example.  Transactions of the
#'American Fisheries Society 128:483-490.
#'
#'Schnute, J.  1981.  A versatile growth model with statistically stable
#'parameters. Canadian Journal of Fisheries & Aquatic Sciences, 38:1128-1140.
#'
#'Somers, I. F. 1988. On a seasonally oscillating growth function. Fishbyte
#'6(1):8-11.
#'@keywords manip
#'@examples
#'## Simple Examples
#'data(SpotVA1)
#'vbStarts(tl~age,data=SpotVA1)
#'vbStarts(tl~age,data=SpotVA1,type="Francis",tFrancis=c(0,5))
#'
#'## Simple Example with a Plot
#'vbStarts(tl~age,data=SpotVA1,type="GallucciQuinn",plot=TRUE)
#'
#'## See examples in vbFuns() for use of vbStarts() when fitting Von B models
#'
#'@rdname vbStarts
#'@export vbStarts
vbStarts <- function(age, ...) {
  UseMethod("vbStarts") 
}

#'@rdname vbStarts
#'@method vbStarts formula
#'@S3method vbStarts formula
vbStarts.formula <- function(age,data=NULL,...) {
  mf <- model.frame(age,data)
  x <- mf[,2]
  y <- mf[,1]
  vbStarts.default(x,y,,...)
}

#'@rdname vbStarts
#'@method vbStarts default
#'@S3method vbStarts default
vbStarts.default <- function(age,len,type=c("typical","original","BevertonHolt",
                                            "Francis","GallucciQuinn","Mooij",
                                            "Schnute","Somers","Somers2",
                                            "vonBertalanffy"),
                             tFrancis=NULL,methEV=c("poly","means"),
                             meth0=c("poly","yngAge"),plot=FALSE,...) {
  x <- NULL  # attempting to get by bindings warning in RCMD CHECK
  meanL <- tapply(len,age,mean)                                                 # mean lengths-at-age
  ages <- as.numeric(names(meanL))                                              # ages represented
  ns <- tapply(len,age,length)
  lmK <- lm(meanL[-1]~meanL[-length(meanL)])                                    # find starting values for K & Linf from Walford plot regression
  sK <- -log(coef(lmK)[2])
  sLinf <- coef(lmK)[1]/(1-coef(lmK)[2])
  respoly <- lm(meanL~poly(ages,2))                                             # fit polynomial regression
  if(match.arg(meth0)=="poly") {
    resroots <- Re(polyroot(coef(respoly)))                                     # get real component of roots to polynomial equation
    st0 <- resroots[which(abs(resroots)==min(abs(resroots)))]                   # find starting value for t0 as polynomial root closest to zero
    sL0 <- predict(respoly,data.frame(ages=0))                                  # find starting value for L0 as predicted value from polynomial at age=0
  } else {
    yngAge <- min(ages[which(ns>1)])                                            # find the youngest age with a n>1
    st0 <- yngAge+(1/sK)*log((sLinf-meanL[yngAge])/sLinf)                       # find starting values for t0 from re-arrangement of typical VonB model and yngAge
    sL0 <- sLinf+(meanL[yngAge]-sLinf)/exp(-sK*yngAge)                          # find starting values for L0 from re-arrangement of original VonB model and yngAge
  }
  if (length(st0)>1) st0 <- st0[1]                                              # if a "double root" was found then reduce to just a single root
  if (length(sL0)>1) sL0 <- sL0[1]
  attributes(sLinf) <- attributes(sK) <- attributes(st0) <- attributes(sL0) <- attributes(meanL) <- NULL     # strip attributes (names mostly)
  type <- match.arg(type)
  if (!(type %in% c("Schnute","Francis"))) {
    if (sK<0) warning("Negative starting value for K suggests model fitting problems.\nTry setting K=0.3 instead.\n",call.=FALSE)
    if ((sLinf<0.5*max(len)) | sLinf>1.5*max(len)) 
      warning("Starting value for Linf is very different from the observed maximum length\nTry setting Linf to maximum length instead.\n",call.=FALSE)
  }
  switch(type,
    typical=,BevertonHolt={ sv <- list(Linf=sLinf,K=sK,t0=st0) },
    original=,vonBertalanffy={ sv <- list(Linf=sLinf,L0=sL0,K=sK) },
    GallucciQuinn={ sv <- list(omega=sLinf*sK,K=sK,t0=st0) },
    Mooij={ sv <- list(Linf=sLinf,L0=sL0,omega=sLinf*sK) },
    Schnute=,Francis={
      if (type=="Francis" & is.null(tFrancis)) stop("The 'tFrancis' argument can NOT be null when type='Francis'.",call.=FALSE)
      if (type=="Francis" & length(tFrancis)!=2) stop("The 'tFrancis' argument must have only two ages in it.",call.=FALSE)
      ifelse(type=="Schnute",ages2use <- range(ages),ages2use <- c(tFrancis[1],mean(tFrancis),tFrancis[2]))
      switch(match.arg(methEV),
        poly={ vals <- predict(respoly,data.frame(ages=ages2use)) },
        means={
          meanFnx <- approxfun(ages,meanL)
          vals <- meanFnx(ages2use)
        }
      ) # end 'methEv' switch
      if (any(diff(vals)<=0)) warning("At least one of the starting values for an older age\n  is smaller than the starting value for a younger age.",call.=FALSE)
      attributes(vals) <- NULL
      ifelse(type=="Schnute",sv <- list(L1=vals[1],L2=vals[2],K=sK),sv <- list(L1=vals[1],L2=vals[2],L3=vals[3]))
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
      else if (type=="Schnute") curve(mdl(x,sv[[1]],sv[[2]],sv[[3]],t1=min(ages),t2=max(ages)),from=min(age),to=max(age),col="red",lwd=3,add=TRUE)
        else {
          curve(mdl(x,sv[[1]],sv[[2]],sv[[3]],t1=tFrancis[1],t2=tFrancis[2],t3=tFrancis[3]),from=min(age),to=max(age),col="red",lwd=3,add=TRUE)
        }
    legend("bottomright",legend=paste(names(sv),formatC(unlist(sv),format="f",digits=2),sep=" = "),bty="n")
  }
  sv   # return starting values list
}
