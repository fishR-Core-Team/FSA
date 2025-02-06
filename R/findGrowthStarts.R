#' @title Find reasonable starting values for common fish growth functions.
#' 
#' @description Finds reasonable starting values for the parameters in a specific parameterization of common growth functions (von Bertalanffy, Gompertz, logistic, Richards).
#' 
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of common functions used to model fish growth (von Bertalanffy, Gompertz, logistic, Richards). The starting values tend to work well in \code{nls} and related non-linear modeling functions, but there is no guarantee that they are the \sQuote{best} starting values (especially if the model is not appropriate for the data). One should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#' 
#' BLAH-BLAH-BLAH
#' 
#' 
#' @inheritParams makeGrowthFun
#' @param formula A formula of the form \code{len~age}.
#' @param data A data frame that contains the variables in \code{formula}.
#' @param constvals A NAMED vector or list of constant values (either lengths or ages) to be used in some of the von Bertalanffy parameterizations. See details.
#' @param fixed A NAMED vector or list that contains user-defined (i.e., fixed rather than automatically generated) starting values for one or more parameters. See details.
#' @param plot A logical that indicates whether a plot of the data with the superimposed model fit at the starting values should be created. This plot is for diagnostic purposes and, thus, cannot be modified in this function.
#' 
#' @return A list that contains reasonable starting values. Note that the parameters will be listed with the same names in the same order as listed in \code{\link{makeGrowthFun}}.
#' 
#' @note BLAH-BLAH-BLAH
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @section IFAR Chapter: 12-Individual Growth.
#' 
#' @seealso See \code{\link{makeGrowthFun}} to make functions that use these starting values and \code{\link{showGrowthFun}} to display the equations used in \pkg{FSA}. See \code{\link{nlsTracePlot}} for help troubleshooting nonlinear models that don't converge.
#' 
#' @keywords manip
#'
#' @examples
#' #===== Make fake data
#' # Setup ages, sample sizes (general reduction in numbers with
#' #   increasing age), and additive SD to model
#' t <- 0:15
#' n <- c(5,10,40,35,25,12,10,10,8,6,5,3,3,3,2,2)
#' sd <- 15
#' # Expand ages and put in a data.frame
#' df <- data.frame(age=rep(t,n))
#' # Add lengths from 1st parameterization of each model type with
#' #   random error for individuals
#' vb1 <- makeGrowthFun(type="von Bertalanffy")
#' df$tlv <- round(vb1(df$age,Linf=450,K=0.3,t0=-0.5)+rnorm(sum(n),0,sd),0)
#' g1 <- makeGrowthFun(type="Gompertz")
#' df$tlg <- round(g1(df$age,Linf=450,a=1,g=0.3)+rnorm(sum(n),0,sd),0)
#' l1 <- makeGrowthFun(type="logistic")
#' df$tll <- round(l1(df$age,Linf=450,gninf=0.3,ti=2)+rnorm(sum(n),0,sd),0)
#' r1 <- makeGrowthFun(type="Richards")
#' df$tlr <- round(r1(df$age,Linf=450,ti=2,k=0.5,b1=1.5)+rnorm(sum(n),0,sd),0)
#' # brief view of data
#' head(df)
#' 
#' #===== Example starting values for 1st parameterization of each type
#' ( svonb1 <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy") )
#' ( sgomp1 <- findGrowthStarts(tlg~age,data=df,type="Gompertz") )
#' ( slogi1 <- findGrowthStarts(tll~age,data=df,type="logistic") )
#' ( srich1 <- findGrowthStarts(tlr~age,data=df,type="Richards") )
#' 
#' #====== Example starting values at other parameterizations
#' ( svonb4 <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=4) )
#' ( sgomp2 <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=2) )
#' ( slogi3 <- findGrowthStarts(tll~age,data=df,type="logistic",param=3) )
#' ( srich4 <- findGrowthStarts(tlr~age,data=df,type="Richards",param=4) )
#' 
#' #====== Some vonB parameterizations require constant values in constvals=
#' ( svonb8 <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",
#'                              param=8,constvals=c(t1=2,t3=11)) )
#' 
#' #====== Demonstrate use of fixed= with 2nd parameterization of von B as e.g.
#' ( svonb2 <- findGrowthStarts(tlv~age,data=df,param=2) )
#' ( svonb2 <- findGrowthStarts(tlv~age,data=df,param=2,fixed=c(Linf=500)) )
#' ( svonb2 <- findGrowthStarts(tlv~age,data=df,param=2,fixed=c(Linf=500,K=0.25)) )
#' #----- useful with plot=TRUE to iteratively guess "close" starting values
#' svonb2 <- findGrowthStarts(tlv~age,data=df,param=2,fixed=c(Linf=600,K=0.25),plot=TRUE)
#' svonb2 <- findGrowthStarts(tlv~age,data=df,param=2,fixed=c(Linf=450,K=0.25),plot=TRUE)
#' 
#' #===== Starting values with diagnostic plot
#' ( sgomp3 <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=3,plot=TRUE) )
#' 
#' #===== Plot at starting and final values
#' #----- creating growth function corresponding to first param of von B
#' vonb1 <- makeGrowthFun(type="von Bertalanffy")
#' #----- plot data
#' plot(tlv~age,data=df,pch=19,col=col2rgbt("black",0.2))
#' #----- plot von b growth function at starting values (svonb1 from above)
#' curve(vonb1(x,Linf=svonb1),col="blue",lwd=5,add=TRUE)
#' #----- fit growth function to data
#' rvonb1 <- nls(tlv~vonb1(age,Linf,K,t0),data=df,start=svonb1)
#' cvonb1 <- coef(rvonb1)
#' #----- plot growth function at final values ... starting values were very good!
#' curve(vonb1(x,Linf=cvonb1),col="red",lwd=2,add=TRUE)
#' 
#' @rdname findGrowthStarts
#' @export

findGrowthStarts <- function(formula,data,
                           type=c("von Bertalanffy","Gompertz","logistic","Richards"),
                           param=1,constvals=NULL,fixed=NULL,plot=FALSE) {
  #===== Checks
  # Handle the formula with some checks
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) STOP("'vbStarts2' must have only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer")) 
    STOP("LHS variable must be numeric.")
  if (!tmp$metExpNumE) STOP("'vbStarts2' must have only one RHS variable.")
  if (!tmp$Eclass %in% c("numeric","integer")) 
    STOP("RHS variable must be numeric.")
  
  # Correct growth model type
  type <- match.arg(type)
  
  # Correct parameterization ... depends on growth model type
  if (param<1) STOP("'param' must be greater than 1")
  max.param <- c("von Bertalanffy"=19,"Gompertz"=7,"logistic"=4,"Richards"=5,
                 "Schnute"=1,"Schnute-Richards"=1)
  if (param>max.param[[type]])
    STOP("'param' must be between 1 and ",max.param[[type]]," for ",type," model")
  
  # initial checks on constvals
  if (!is.null(constvals)) {
    if (type!="von Bertalanffy")
      STOP("'constvals' only used when 'type=",type,"'. Either\n",
           "  don't use 'constvals' or change 'type'")
    if (is.null(names(constvals)))
      STOP("Values in 'constvals' must be named.")
  }
  
  # Is fixed of correct type ... will check for correct names later
  if (!is.null(fixed)) {
    fxdnms <- names(fixed)
    if (is.null(fxdnms)) STOP("Items in 'fixed' must be named.")
    if (any(fxdnms=="")) STOP("All items in 'fixed' must be named.")
  }
  
  #===== Get starting values
  # get variable names
  lennm <- tmp$Rname[1]
  agenm <- tmp$Enames[1]
  
  # call specific internal function to create starting value list
  switch(type,
         "von Bertalanffy"= { sv <- iVonBStarts(lennm,agenm,data,param,constvals) },
         "Gompertz"=        { sv <- iGompStarts(lennm,agenm,data,param) },
         "logistic"=        { sv <- iLogiStarts(lennm,agenm,data,param) },
         "Richards"=        { sv <- iRichStarts(lennm,agenm,data,param) },
         "Schnute"=         { sv <- NULL },
         "Schnute-Richards"={ sv <- NULL })
  
  # replace with values in fixed (if any & named correctly)
  if (!is.null(fixed)) {
    if (all(fxdnms %in% names(sv))) { # all correctly named
      sv[fxdnms] <- fixed[fxdnms]
    } else {                          # some not correctly named
      fxdnms <- fxdnms[!fxdnms %in% names(sv)]
      STOP("Some names in 'fixed' (",paste(fxdnms,collapse=", "),
                ") are not parameters (",paste(names(sv),collapse=", "),
                ")\n  for ",type," parameterization #",param)
    }
  }
  
  #===== Plot starting values (if asked to)
  if (plot) iPlotGrowStarts(sv,data,lennm,agenm,type,param,constvals)
  
  #===== Return starting value list
  if (is.null(sv)) STOP("Starting values not yet implemented in 'FSA' for",
                        "\n  ",type," paramaterization #",param)
  sv
}


#===============================================================================
#== Internal Functions -- Compute starting values for each type of model
#===============================================================================
iVonBStarts <- function(ynm,xnm,data,param,constvals) {
  # Perform checks on constvals (some checks already in main function)
  if (is.null(constvals) & param %in% c(6,7,8))
    STOP("You must use 'constvals' with von Bertalanffy paramaterization #",param)
  if (!is.null(constvals)) {
    cvnms <- names(constvals)
    if (param==6) { # Ogle-Isermann parameterization
      if (length(cvnms)!=1) 
        STOP("'constvals' must have only one value when 'param=",param,"'" )
      if (!cvnms %in% c("Lr","tr"))
        STOP("Value names in 'constvals' must be 'Lr' or 'tr' when 'param=",param,"'")
    }
    if (param %in% c(7,8)) { # Schnute and Francis parameterizations
      if (length(cvnms)!=2)
        STOP("'constvals' must have exactly two values ('t1' and 't3') when 'param=",
             param,"'" )
      if (!all(cvnms %in% c("t1","t3")))
        STOP("Value names in 'constvals' must be 't1' and 't3' when 'param=",param,"'")
    }
    if (param %in% c(18,19)) { # Schnute and Francis parameterizations
      if (length(cvnms)!=2) 
        STOP("'constvals' must have exactly two values ('L1' and 'L2') when 'param=",
             param,"'" )
      if (!all(cvnms %in% c("L1","L2")))
        STOP("Value names in 'constvals' must be 'L1' and 'L2' when 'param=",param,"'")
    }
  }

  # Get starting values for SSasymp parameters
  ssform <- stats::as.formula(paste0(ynm,"~stats::SSasymp(",
                                     xnm,",Asym,R0,lrc)"))
  sstmp <- stats::getInitial(ssform,data=data)
  
  # Convert SSasymp parameter starting values to VB parameters
  Linf <- sstmp[["Asym"]]
  L0 <- sstmp[["R0"]]
  K <- exp(sstmp[["lrc"]])
  t0 <- -log(Linf/(Linf-L0))/K
  omega <- K*Linf
  t50 <- t0+log(2)/K
  if (param==6) {
    if (cvnms=="tr") tmp <- Linf*(1-exp(-K*(constvals[["tr"]]-t0)))
    if (cvnms=="Lr") tmp <- -log((Linf-constvals[["Lr"]])/(Linf-L0))/K
    sv6 <- list(Linf,K,tmp)
    names(sv6) <- c("Linf","K",ifelse(cvnms=="tr","Lr","tr"))
  }
  if (param %in% c(7,8)) {
    L1 <- Linf*(1-exp(-K*(constvals[["t1"]]-t0)))
    L3 <- Linf*(1-exp(-K*(constvals[["t3"]]-t0)))
    L2 <- Linf*(1-exp(-K*(mean(constvals)-t0)))
  }
  # Perform sanity checks on values ... could indicate model problems
  
  # Create starting value list specific to parameterization
  sv <- NULL
  switch(as.character(param),
          "1"= {  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),K=K,t0=t0)  },
          "2"= {  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),K=K,L0=iChkL0(L0,data[[ynm]]))  },
          "3"= {  sv <- list(omega=omega,K=K,t0=t0)  },
          "4"= {  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),L0=iChkL0(L0,data[[ynm]]),omega=omega)  },
          "5"= {  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),t0=t0,t50=iChkt50(t50))  },
          "6"= {  sv <- sv6  },
          "7"= {  sv <- list(L1=L1,L3=L3,K=K)  },
          "8"= {  sv <- list(L1=L1,L2=L2,L3=L3)  },
         "10"= {  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),K=K,t0=t0,C=0.5,ts=0.3)  },
         "11"= {  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),K=K,t0=t0,C=0.5,WP=0.8)  },
         "12"= {  sv <- list(Kpr=K/(1-0.3),t0=t0,ts=0.3,NGT=0.3)  },
         "13"= {  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),K=K)  },
         "14"= {  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),K=K)  }
  )
  sv
}

iGompStarts <- function(ynm,xnm,data,param) {
  # Get starting values for SSgompertz parameters
  ssform <- stats::as.formula(paste0(ynm,"~stats::SSgompertz(",
                                     xnm,",Asym,b2,b3)"))
  sstmp <- stats::getInitial(ssform,data=data)
  
  # Convert SSgompertz parameter starting values to VB parameters
  Linf <- sstmp[["Asym"]]
  gi <- -log(sstmp[["b3"]])
  b2 <- sstmp[["b2"]]
  
  # Perform sanity checks on values ... could indicate model problems
  
  # Create starting value list specific to parameterization
  sv <- NULL
  switch(param,
         Gompertz1={  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),gi=gi,a1=log(b2))  },
         Gompertz2={  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),gi=gi,ti=iChkti(log(b2)/gi))  },
         Gompertz3={  sv <- list(L0=iChkL0(Linf/exp(b2),data[[ynm]]),gi=gi,a2=b2)  },
         Gompertz4={  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),gi=gi,a2=b2)  },
         Gompertz5={  sv <- list(Linf=iChkLinf(Linf,data[[ynm]]),gi=gi,t0=log(b2*gi)/gi)  },
  )
  
  # Return the starting value list
  sv
}

iLogiStarts <- function(ynm,xnm,data,param) {
  # Get starting values for SSlogis parameters
  ssform <- stats::as.formula(paste0(ynm,"~stats::SSlogis(",
                                     xnm,",Asym,xmid,scal)"))
  sstmp <- stats::getInitial(ssform,data=data)
  
  # Convert SSgompertz parameter starting values to VB parameters
  Linf <- sstmp[["Asym"]]
  xmid <- sstmp[["xmid"]]
  scal <- sstmp[["scal"]]
  gninf <- 1/scal
  
  # Perform sanity checks on values ... could indicate model problems
  Linf <- iChkLinf(Linf,data[[ynm]])
  
  # Create starting value list specific to parameterization
  sv <- NULL
  switch(param,
         logistic1={  sv <- list(Linf=Linf,gninf=gninf,ti=iChkti(xmid))  },
         logistic2={  sv <- list(Linf=Linf,gninf=gninf,a=exp(xmid/scal))  },
         logistic3={  sv <- list(Linf=Linf,gninf=gninf,L0=iChkL0(Linf/(1+exp(xmid/scal)),data[[ynm]]))  }
  )
  
  # Return the starting value list
  sv
  
}

iRichStarts <- function(ynm,xnm,data,param) {
  # Get starting values for SSposnegRichards parameters
  sstmp <- FlexParamCurve::modpar(data[[xnm]],data[[ynm]],
                                  pn.options="sstmp",width.bounds=2,force4par=TRUE,
                                  verbose=FALSE,suppress.text=TRUE)
  
  # Convert SSposnegRichards parameter starting values to Richards parameters
  Linf <- sstmp[["Asym"]]
  k <- sstmp[["K"]]
  ti <- sstmp[["Infl"]]
  M <- sstmp[["M"]]
  t0 <- -(log(1/M)/k)+ti
  L0 <- Linf/((1+M*exp(k*ti)))^(1/M)
  
  # Perform sanity checks on values ... could indicate model problems
  Linf <- iChkLinf(Linf,data[[ynm]]) # do here as all params use Linf
  
  # Create starting value list specific to parameterization
  sv <- NULL
  switch(param,
         Richards1={  sv <- list(Linf=Linf,k=k,ti=iChkti(ti),b1=M)  },
         Richards2={  sv <- list(Linf=Linf,k=k,t0=t0,b2=-(1/M))  },
         Richards3={  sv <- list(Linf=Linf,k=k,L0=iChkL0(L0,data[[ynm]]),b3=1+M)  },
         Richards4={  sv <- list(Linf=Linf,k=k,ti=iChkti(ti),b2=-(1/M))  },
         Richards5={  sv <- list(Linf=Linf,k=k,ti=iChkti(ti),b3=1+M)  }
  )
  
  # Return the starting value list
  sv
}

#===============================================================================
#== Internal Functions -- Parameter sanity checks
#===============================================================================
iChkLinf <- function(sLinf,len) {
  if (is.lt(sLinf,0.5*max(len,na.rm=TRUE)) | is.gt(sLinf,1.5*max(len,na.rm=TRUE))) {
    WARN("Starting value for 'Linf' is very different from the observed maximum\n",
         "length, which suggests a model fitting problem. Try plotting the data\n",
         "with the model at the starting values superimposed with 'plot=TRUE' to\n",
         "potentially diagnose any problems. Also consider using 'fixed=c(Linf=)'\n",
         "to manually set 'Linf' to a reasonable value.\n")
  }
  sLinf
}

iChkL0 <- function(sL0,len) {
  if (is.lt(sL0,0))
    WARN("Starting value for 'L0' is negative, which does not make biological\n",
         "sense and suggests a model fitting problem. Try plotting the data\n",
         "with the model at the starting values superimposed with 'plot=TRUE' to\n",
         "potentially diagnose any problems. Also consider using 'fixed=c(L0=)'\n",
         "to manually set 'L0' to a reasonable value.\n")
  else if (is.gt(sL0,0.25*min(len,na.rm=TRUE)))
    WARN("Starting value for 'L0' is more than 25% of the minimum observed length\n",
         "which is possible but it may also suggests a model fitting problem. Try\n",
         "plotting the data with the model at the starting values superimposed with\n",
         "'plot=TRUE' to potentially diagnose any problems. Also consider using\n",
         "'fixed=c(Linf=)' to manually set 'L0' to a reasonable value.\n")
  sL0
}

iChkti <- function(sti) {
  if (is.lt(sti,0))
    WARN("Starting value for 'ti' is negative, which does not make biological\n",
         "sense and suggests a model fitting problem. Try plotting the data\n",
         "with the model at the starting values superimposed with 'plot=TRUE' to\n",
         "potentially diagnose any problems. Also consider using 'fixed=c(ti=)'\n",
         "to manually set 'ti' to a reasonable value.\n")
  sti
}

iChkt50 <- function(st50) {
  if (is.lt(st50,0))
    WARN("Starting value for 't50' is negative, which does not make biological\n",
         "sense and suggests a model fitting problem. Try plotting the data\n",
         "with the model at the starting values superimposed with 'plot=TRUE' to\n",
         "potentially diagnose any problems. Also consider using 'fixed=c(t50=)'\n",
         "to manually set 't50' to a reasonable value.\n")
  st50
}
#===============================================================================
#== Internal Functions -- Diagnostic plot for starting values
#===============================================================================
iPlotGrowStarts <- function(sv,data,ynm,xnm,type,param,constvals) { # nocov start
  # Trying to fix no visible binding error for x
  x <- NULL

  # create a transparency value that attempts to not be too transparent
  tmp <- max(stats::xtabs(stats::as.formula(paste0("~",ynm,"+",xnm)),data=data))
  clr <- grDevices::rgb(0,0,0,ifelse(tmp>2 & tmp<20,2/tmp,0.1))
  
  # Make the base plot
  graphics::plot(stats::as.formula(paste0(ynm,"~",xnm)),data,
                 pch=19,col=clr,xlab="Age",ylab="Length",
                 main=paste0(type," Paramaterization #",param," at STARTING VALUES"),
                 cex.main=0.9)
  
  ## Plot the model
  # get the appropriate growth function
  mdl <- makeGrowthFun(type=type,param=param)
  
  # plotting must be handled slightly differently for Francis/Schnute VBs
  #   note that age2use will be in consts
  if (type=="von Bertalanffy" & param %in% c(7,8)) {
    graphics::curve(mdl(x,unlist(sv),t1=constvals),col="red",lwd=3,add=TRUE)
  } else {
    # get the argument values (those from sv and from constvals)
    tmp <- unlist(c(sv,constvals))
    # make sure arguments in same order as expected from mdl
    tmp <- tmp[names(formals(mdl)[-1])]
    # add the curve
    graphics::curve(mdl(x,tmp),col="red",lwd=3,add=TRUE)
  }
  # Put the starting values on the plot
  graphics::legend("bottomright",
                   paste(names(sv),formatC(unlist(sv),format="f",digits=2),
                         sep="="),bty="n")
} # nocov end
