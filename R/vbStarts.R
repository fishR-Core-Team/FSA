#' @title Find reasonable starting values for a von Bertalanffy growth function.
#' 
#' @description Finds reasonable starting values for the parameters in a specific parameterization of the von Bertalanffy growth function.
#' 
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of the von Bertalanffy growth function.  There is no guarantee that these starting values are the \sQuote{best} starting values.  One should use them with caution and should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#' 
#' The Linf and K paramaters are estimated via the concept of the Ford-Walford plot.  The product of the starting values for Linf and K is used as a starting value for omega in the GallucciQuinn and Mooij parameterizations.  The result of log(2) divided by the starting value for K is used as the starting value for t50 in the Weisberg parameterization.
#' 
#' If \code{meth0="yngAge"}, then a starting value for t0 or L0 is found by algebraically solving the typical or original paramaterization, respectively, for t0 or L0 using the mean length of the first age with more than one data point as a \dQuote{known} quantity.  If \code{meth0="poly"} then a second-degree polynomial model is fit to the mean length-at-age data.  The t0 starting value is set equal to the root of the polynomial that is closest to zero.  The L0 starting value is set equal to the mean length at age-0 predicted from the polynomial function.
#' 
#' Starting values for the L1 and L3 parameters in the Schnute paramaterization and the L1, L2, and L3 parameters in the Francis parameterization may be found in two ways.  If \code{methEV="poly"}, then the starting values are the predicted length-at-age from a second-degree polynomial fit to the mean lengths-at-age data.  If \code{methEV="means"} then the observed sample means at the corresponding ages are used.  In the case where one of the supplied ages is fractional, then the value returned will be linearly interpolated between the mean lengths of the two closest ages.  The ages to be used for L1 and L3 in the Schnute and Francis parameterizations are supplied as a numeric vector of length 2 in \code{ages2use=}.  If \code{ages2use=NULL} then the minimum and maximum observed ages will be used.  In the Francis method, L2 will correspond to the age half-way between the two ages in \code{ages2use=}.  A warning will be given if L2<L1 for the Schnute method or if L2<L1 or L3<L2 for the Francis method.
#' 
#' @param formula A formula of the form \code{len~age}.
#' @param data A data frame that contains the variables in \code{formula}.
#' @param type A string that indicates the parameterization of the von Bertalanffy model.
#' @param ages2use A numerical vector of the two ages to be used in the Schnute or Francis paramaterizations.  See details.
#' @param methEV A string that indicates how the lengths of the two ages in the Schnute paramaterization or the three ages in the Francis paramaterization should be derived.  See details.
#' @param meth0 A string that indicates how the t0 and L0 paramaters should be derived.  See details.
#' @param plot A logical that indicates whether a plot of the data with the superimposed model fit at the starting values should be created.
#' @param col.mdl A color for the model when \code{plot=TRUE}.
#' @param lwd.mdl A line width for the model when \code{plot=TRUE}.
#' @param lty.mdl A line type for the model when \code{plot=TRUE}.
#' @param cex.main A character expansion value for the main title when \code{plot=TRUE}.
#' @param col.main A color for the main title when \code{plot=TRUE}.
#' @param dynamicPlot DEPRECATED.
#' @param \dots Further arguments passed to the methods.
#' 
#' @return A list that contains reasonable starting values.  Note that the parameters will be listed in the same order and with the same names as listed in \code{\link{vbFuns}}.
#' 
#' @note The \sQuote{original} and \sQuote{vonBertalanffy} and the \sQuote{typical} and \sQuote{BevertonHolt} parameterizations are synonymous.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @section IFAR Chapter: 12-Individual Growth.
#' 
#' @seealso See \code{\link{growthFunShow}} to display the equations for the parameterizations used in \pkg{FSA} and \code{\link{vbFuns}} for functions that represent the von Bertalanffy parameterizations.
#' 
#' @references Ogle, D.H.  2016.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' See references in \code{\link{vbFuns}}.
#' 
#' @keywords manip
#' @examples
#' ## Examples
#' data(SpotVA1)
#' vbStarts(tl~age,data=SpotVA1)
#' vbStarts(tl~age,data=SpotVA1,type="Original")
#' vbStarts(tl~age,data=SpotVA1,type="GQ")
#' vbStarts(tl~age,data=SpotVA1,type="Mooij")
#' vbStarts(tl~age,data=SpotVA1,type="Weisberg")
#' vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5))
#' vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,5))
#' vbStarts(tl~age,data=SpotVA1,type="Somers")
#' vbStarts(tl~age,data=SpotVA1,type="Somers2")
#' 
#' ## Using a different method to find t0 and L0
#' vbStarts(tl~age,data=SpotVA1,meth0="yngAge")
#' vbStarts(tl~age,data=SpotVA1,type="original",meth0="yngAge")
#' 
#' ## Using a different method to find the L1, L2, and L3
#' vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5),methEV="means")
#' vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,5),methEV="means")
#' 
#' ## Example with a Plot
#' vbStarts(tl~age,data=SpotVA1,plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="original",plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="GQ",plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="Mooij",plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="Weisberg",plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5),plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,5),plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="Somers",plot=TRUE)
#' vbStarts(tl~age,data=SpotVA1,type="Somers2",plot=TRUE)
#' 
#' ## See examples in vbFuns() for use of vbStarts() when fitting Von B models
#' 
#' @export vbStarts
vbStarts <- function(formula,data=NULL,
                     type=c("Typical","typical","Traditional","traditional","BevertonHolt",
                            "Original","original","vonBertalanffy",
                            "GQ","GallucciQuinn","Mooij","Weisberg",
                            "Schnute","Francis","Somers","Somers2"),
                     ages2use=NULL,methEV=c("means","poly"),meth0=c("yngAge","poly"),
                     plot=FALSE,col.mdl="gray70",lwd.mdl=3,lty.mdl=1,
                     cex.main=0.9,col.main="red",dynamicPlot=FALSE,...) {
  ## some checks of arguments
  type <- match.arg(type)
  methEV <- match.arg(methEV)
  meth0 <- match.arg(meth0)
  ## handle the formula with some checkes
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) stop("'vbStarts' must have only one LHS variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'vbStarts' must have only one RHS variable.",call.=FALSE)
  if (!tmp$Eclass %in% c("numeric","integer")) stop("RHS variable must be numeric.",call.=FALSE)
  ## get the length and age vectors
  len <- tmp$mf[,tmp$Rname[1]]
  age <- tmp$mf[,tmp$Enames[1]]
  ## get starting valeus depending on type
  switch(type,
    Typical=,typical=,Traditional=,traditional=,BevertonHolt=    {
      type <- "Typical"
      sv <- iVBStarts.typical(age,len,type,meth0) },
    Original=,original=,vonBertalanffy={
      type <- "Original"
      sv <- iVBStarts.original(age,len,type,meth0) },
    GQ=,GallucciQuinn=       {
      type <- "GQ"
      sv <- iVBStarts.GQ(age,len,type,meth0) },
    Mooij=                   { sv <- iVBStarts.Mooij(age,len,type,meth0) },
    Weisberg=                { sv <- iVBStarts.Weisberg(age,len,type,meth0) },
    Francis=                 { sv <- iVBStarts.Francis(age,len,type,methEV,ages2use) },
    Schnute=                 { sv <- iVBStarts.Schnute(age,len,type,meth0,methEV,ages2use) },
    Somers=                  { sv <- iVBStarts.Somers(age,len,type,meth0) },
    Somers2=                 { sv <- iVBStarts.Somers2(age,len,type,meth0) }
  ) # end 'type' switch
  ## make the static plot if asked for
  if (plot) iVBStartsPlot(age,len,type,sv,ages2use,col.mdl,lwd.mdl,lty.mdl,cex.main,col.main)
  ## Check if user wants to choose starting values from an interactive plot
  if (dynamicPlot) warning("The 'dynamicPlot' functionality has been moved to 'vbStartsDP' in the 'FSASims' package.",call.=FALSE)
  ## return starting values list
  sv
}



##############################################################
# INTERNAL FUNCTIONS
##############################################################
#=============================================================
# Find starting values for Linf and K from a Walford Plot
#=============================================================
iVBStarts.LinfK <- function(age,len,type) {
  ## compute mean lengths-at-age and numbers-at-age
  meanL <- tapply(len,age,mean)
  ns <- tapply(len,age,length)
  ## fit Walford plot regression
  cfs <- stats::coef(stats::lm(meanL[-1]~meanL[-length(meanL)]))
  ## find starting values from Walford plot regression coefficients
  sLinf <- cfs[[1]]/(1-cfs[[2]])
  sK <- -log(cfs[[2]])
  ## check reasonableness of values
  iCheckLinfK(sLinf,sK,type,len)
  ## return the starting values
  c(Linf=sLinf,K=sK)
}

#=============================================================
# Perform some checks for "bad" values of Linf and K
#=============================================================
iCheckLinfK <- function(sLinf,sK,type,len) {
  if ((sLinf<0.5*max(len)) | sLinf>1.5*max(len)) {
    msg <- "Starting value for Linf is very different from the observed maximum length, "
    msg <- paste(msg,"which suggests a model fitting problem.\n",sep="")
    msg <- paste(msg,"See a Walford or Chapman plot to examine the problem.\n",sep="")
    msg <- paste(msg,"Consider manually setting Linf to the maximum observed length\n",sep="")
    msg <- paste(msg,"in the starting value list.\n",sep="")
    warning(msg,call.=FALSE)    
  } 
  if (sK<0) {
    if (type %in% c("Typical","typical","Original","original","BevertonHolt",
                    "vonBertalanffy","GQ","GallucciQuinn","Schnute")) {
      msg <- "The suggested starting value for K is negative, "
    } else {
      msg <- "One  suggested starting value is based on a negative K, "
    }
    msg <- paste(msg,"which suggests a model fitting problem.\n",sep="")
    msg <- paste(msg,"See a Walford or Chapman Plot to examine the problem.\n",sep="")
    msg <- paste(msg,"Consider manually setting K=0.3 in the starting value list.\n",sep="")
    warning(msg,call.=FALSE)
  }
}

#=============================================================
# Find starting values for t0 and L0
#=============================================================
iVBStarts.0 <- function(age,len,type,meth0) {
  ## compute mean lengths-at-age and numbers-at-age
  meanL <- tapply(len,age,mean)
  ns <- tapply(len,age,length)
  ## find ages represented
  ages <- as.numeric(names(meanL))
  ## find values depending on method
  if(meth0=="poly") {
    # fit polynomial regression
    respoly <- stats::lm(meanL~stats::poly(ages,2,raw=TRUE))
    # get real component of roots to polynomial equation
    resroots <- Re(polyroot(stats::coef(respoly)))
    # find starting value for t0 as polynomial root closest to zero
    st0 <- resroots[which(abs(resroots)==min(abs(resroots)))]
    # find starting value for L0 as predicted value from polynomial at age=0
    sL0 <- stats::predict(respoly,data.frame(ages=0))
    # this removes the attributes and will return only the first
    # root if a "double root" was found
    st0 <- st0[[1]]
    sL0 <- sL0[[1]]
  } else {
    # find starting values for Linf and K
    tmp <- iVBStarts.LinfK(age,len,type)
    # find the youngest age with a n>=1
    if (all(ns==1)) yngAge <- min(ages)
      else yngAge <- min(ages[which(ns>=1)])
    # find starting values for t0 from re-arrangement of typical VonB model and yngAge
    st0 <- yngAge+(1/tmp[["K"]])*log((tmp[["Linf"]]-meanL[[which(ages==yngAge)]])/tmp[["Linf"]])
    # find starting values for L0 from re-arrangement of original VonB model and yngAge
    sL0 <- tmp[["Linf"]]+(meanL[[which(ages==yngAge)]]-tmp[["Linf"]])/exp(-tmp[["K"]]*yngAge)
  }
  ## Return starting values
  c(t0=st0,L0=sL0)
}

#=============================================================
# find starting values for L1, L2, and L3 (of the Francis and
# Schnute methods)
#=============================================================
iVBStarts.Ls <- function(age,len,type,methEV,ages2use) {
  ## compute mean lengths-at-age and numbers-at-age
  meanL <- tapply(len,age,mean)
  ns <- tapply(len,age,length)
  ## find ages represented
  ages <- as.numeric(names(meanL))
  ## Handle ages2use
  # if none given then use the min and max
  if (is.null(ages2use)) ages2use <- range(ages)
  # if too many given then send an error
  if (length(ages2use)!=2) stop("'ages2use=' must be NULL or have only two ages.",call.=FALSE)
  # if order is backwards then warn and flip
  if (ages2use[2]<=ages2use[1]) {
    warning("'ages2use' should be in ascending order; order reversed to continue.",call.=FALSE)
    ages2use <- rev(ages2use)
  }
  # if using the Francis parameterization then must find the
  # intermediate age
  if (type=="Francis") ages2use <- c(ages2use[1],mean(ages2use),ages2use[2])
  ## Find mean lengths at ages2use
  if (methEV=="poly") {
    # fit polynomial regression
    respoly <- stats::lm(meanL~stats::poly(ages,2))
    # predict length at ages2use
    vals <- stats::predict(respoly,data.frame(ages=ages2use))
  } else {
    # fit an interpolating functions
    meanFnx <- stats::approxfun(ages,meanL)
    # find ages from that function
    vals <- meanFnx(ages2use)
  }
  ## Check if these values make sense
  if (any(diff(vals)<=0)) warning("At least one of the starting values for an older age\n  is smaller than the starting value for a younger age.",call.=FALSE)
  ## Return the values
  if (type=="Francis") list(L1=vals[[1]],L2=vals[[2]],L3=vals[[3]])
  else list(L1=vals[[1]],L3=vals[[2]])
}


#=============================================================
# Find starting values the typical VB parameterization
#=============================================================
iVBStarts.typical <- function(age,len,type,meth0) {
  as.list(c(iVBStarts.LinfK(age,len,type),iVBStarts.0(age,len,type,meth0)["t0"]))
}

#=============================================================
# Find starting values the original VB parameterization
#=============================================================
iVBStarts.original <- function(age,len,type,meth0) {
  tmp <- iVBStarts.LinfK(age,len,type)
  as.list(c(tmp["Linf"],iVBStarts.0(age,len,type,meth0)["L0"],tmp["K"]))
}

#=============================================================
# Find starting values the GQ VB parameterization
#=============================================================
iVBStarts.GQ <- function(age,len,type,meth0) {
  tmp <- iVBStarts.typical(age,len,type,meth0)
  as.list(c(omega=tmp[["Linf"]]*tmp[["K"]],tmp["K"],tmp["t0"]))
}

#=============================================================
# Find starting values the Mooij VB parameterization
#=============================================================
iVBStarts.Mooij <- function(age,len,type,meth0) {
  tmp <- iVBStarts.original(age,len,type,meth0)
  as.list(c(tmp["Linf"],tmp["L0"],omega=tmp[["Linf"]]*tmp[["K"]]))
}

#=============================================================
# Find starting values the Weisberg VB parameterization
#=============================================================
iVBStarts.Weisberg <- function(age,len,type,meth0) {
  tmp <- iVBStarts.typical(age,len,type,meth0)
  as.list(c(tmp["Linf"],t50=log(2)/tmp[["K"]]+tmp[["t0"]],tmp["t0"]))
}

#=============================================================
# Find starting values the Francis VB parameterization
#=============================================================
iVBStarts.Francis <- function(age,len,type,methEV,ages2use) {
  iVBStarts.Ls(age,len,type,methEV,ages2use)
}

#=============================================================
# Find starting values the Schnute VB parameterization
#=============================================================
iVBStarts.Schnute <- function(age,len,type,meth0,methEV,ages2use) {
  as.list(c(iVBStarts.Ls(age,len,type,methEV,ages2use),iVBStarts.LinfK(age,len,type)["K"]))
}

#=============================================================
# Find starting values the Somers VB parameterization
#=============================================================
iVBStarts.Somers <- function(age,len,type,meth0) {
  as.list(c(iVBStarts.typical(age,len,type,meth0),C=0.9,ts=0.1))
}

#=============================================================
# Find starting values the Somers2 VB parameterization
#=============================================================
iVBStarts.Somers2 <- function(age,len,type,meth0) {
  as.list(c(iVBStarts.typical(age,len,type,meth0),C=0.9,WP=0.9))
}


#=============================================================
# Static plot of starting values
#=============================================================
iVBStartsPlot <- function(age,len,type,sv,ages2use,col.mdl,lwd.mdl,lty.mdl,
                          cex.main,col.main) { # nocov start
  ## attempting to get by bindings warning in RCMD CHECK
  x <- NULL
  ## Plot the data
  # create a transparency value that is the max of 1/2th of the
  # maximum number at any length and age combination or 0.1
  clr <- grDevices::rgb(0,0,0,max(2/max(table(age,len)),0.1))
  # Make the base plot
  graphics::plot(age,len,pch=19,col=clr,xlab="Age",ylab="Length",
                 main=paste0("von B (",type,") STARTING VALUES"),
                 cex.main=cex.main,col.main="red")
  ## Plot the model
  mdl <- vbFuns(type)
  min.age <- min(age,na.rm=TRUE)
  max.age <- max(age,na.rm=TRUE)
  if (!type %in% c("Schnute","Francis")) {
    graphics::curve(mdl(x,unlist(sv)),from=min.age,to=max.age,
                    col=col.mdl,lwd=lwd.mdl,lty=lty.mdl,add=TRUE)
  } else {
    # Schnute/Francis requires t1 argument
    graphics::curve(mdl(x,unlist(sv),t1=ages2use),from=min.age,to=max.age,
                    col=col.mdl,lwd=lwd.mdl,lty=lty.mdl,add=TRUE)
  }
  ## Put the starting values to put on the plot
  legend("bottomright",paste(names(sv),formatC(unlist(sv),format="f",digits=2),sep="="),bty="n")
} # nocov end