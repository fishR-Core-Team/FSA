#' @title Find reasonable starting values for common fish growth functions.
#' 
#' @description Finds reasonable starting values for the parameters in a specific parameterization of common growth functions (von Bertalanffy, Gompertz, logistic, Richards).
#' 
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of common functions used to model fish growth (von Bertalanffy, Gompertz, logistic, Richards). The starting values tend to work well in \code{nls} and related non-linear modeling functions, but there is no guarantee that they are the \sQuote{best} starting values (especially if the model is not appropriate for the data). One should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#' 
#' In some instances it may be beneficial to fix one or more of the starting values to a user-defined choice. This can be done with \code{fixed} as shown in the examples. Note that starting values for other parameters that depend on the value of the fixed parameter may also be affected. For example, a starting value for \eqn{t_0} in the "Typical" von Bertalanffy function depends on values of \eqn{L_\infty} and \eqn{K}. Thus, if, for example, \eqn{K} is fixed by the user then the starting value for \eqn{t_0} will also be affected as it will used the fixed rather than the automatically derived value for \eqn{K}.
#' 
#' It is good practice for two reasons to use \code{plot=TRUE} to superimpose the growth function evaluated at the starting values over a scatterplot of the observed lengths versus ages. First, this will give the user a feel for how well the growth function fits the data given the starting values. If the "model line" does not represent the data well then the starting values are likely poor and the non-linear model may not converge. Second, the user iteratively supply values for the parameters in \code{fixed} with \code{plot=TRUE} to "guess" at useful starting values. This is demonstrated in the examples.
#' 
#' @note Derivation of the starting values is detailed in the \dQuote{Growth_Starting_Values} article on the \code{FSA} webpage. Further note that starting values have not yet been coded for every parameterization of the growth functions available in \code{FSA}. In those instances, you will need to derive starting values by other means.
#' 
#' @inheritParams makeGrowthFun
#' @param formula A formula of the form \code{length~age} for length-at-age models or \code{deltaL~deltat+lengthM} for tag-recapture models. \code{length} and \code{age} generically represent the observed length and annual age, and \code{deltaL}, \code{deltat}, and \code{lengthM} generically represent the observed change in length, observed change in time, and observed length at marking.
#' @param data A data frame that contains the variables in \code{formula}.
#' @param constvals A NAMED numeric vector of constant values (either lengths or ages) to be used in some of the von Bertalanffy parameterizations. See details.
#' @param fixed A NAMED numeric vector that contains user-defined (i.e., fixed rather than automatically generated) starting values for one or more parameters. See details.
#' @param plot A logical that indicates whether a plot of the data with the superimposed model fit at the starting values should be created. This plot is for diagnostic purposes and, thus, cannot be modified in this function.
#' 
#' @return A named vector that contains reasonable starting values. Note that the parameters will be listed with the same names in the same order as listed in \code{\link{makeGrowthFun}}.
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
#' # These examples use the hypothetical length-at-age (annual) data in GrowthData1
#' 
#' #===== Example starting values for 1st parameterization of each type
#' ( svonb1 <- findGrowthStarts(tlV~age,data=GrowthData1,type="von Bertalanffy") )
#' ( sgomp1 <- findGrowthStarts(tlG~age,data=GrowthData1,type="Gompertz") )
#' ( slogi1 <- findGrowthStarts(tlL~age,data=GrowthData1,type="logistic") )
#' ( srich1 <- findGrowthStarts(tlR~age,data=GrowthData1,type="Richards") )
#' 
#' #====== Example starting values at other parameterizations
#' ( svonb4 <- findGrowthStarts(tlV~age,data=GrowthData1,type="von Bertalanffy",param=4) )
#' ( sgomp2 <- findGrowthStarts(tlG~age,data=GrowthData1,type="Gompertz",param=2) )
#' ( slogi3 <- findGrowthStarts(tlL~age,data=GrowthData1,type="logistic",param=3) )
#' ( srich3 <- findGrowthStarts(tlR~age,data=GrowthData1,type="Richards",param=3) )
#' 
#' #' #====== Example using pname instead of param
#' ( svonb4 <- findGrowthStarts(tlV~age,data=GrowthData1,type="von Bertalanffy",pname="Mooij") )
#' ( sgomp2 <- findGrowthStarts(tlG~age,data=GrowthData1,type="Gompertz",pname="Ricker1") )
#' ( slogi3 <- findGrowthStarts(tlL~age,data=GrowthData1,type="logistic",pname="Campana-Jones2") )
#' ( srich3 <- findGrowthStarts(tlR~age,data=GrowthData1,type="Richards",pname="Tjorve7") )
#' 
#' #====== Some vonB parameterizations require constant values in constvals=
#' ( svonb8 <- findGrowthStarts(tlV~age,data=GrowthData1,type="von Bertalanffy",
#'                              pname="Francis",constvals=c(t1=2,t3=11)) )
#' 
#' #====== Demonstrate use of fixed= with 2nd (Original) param of von B as e.g.
#' ( svonb2 <- findGrowthStarts(tlV~age,data=GrowthData1,param=2) )
#' ( svonb2 <- findGrowthStarts(tlV~age,data=GrowthData1,param=2,fixed=c(Linf=500)) )
#' ( svonb2 <- findGrowthStarts(tlV~age,data=GrowthData1,param=2,fixed=c(Linf=500,K=0.25)) )
#' 
#' #===== Starting values with diagnostic plot
#' ( sgomp3 <- findGrowthStarts(tlG~age,data=GrowthData1,type="Gompertz",param=3,plot=TRUE) )
#' 
#' #===== Iteratively guess at starting values (stop when the model seems to "fit")
#' findGrowthStarts(tlV~age,data=GrowthData1,plot=TRUE,fixed=c(Linf=600,K=0.5,t0=0))    #att 1
#' findGrowthStarts(tlV~age,data=GrowthData1,plot=TRUE,fixed=c(Linf=450,K=0.5,t0=0))    #att 2
#' findGrowthStarts(tlV~age,data=GrowthData1,plot=TRUE,fixed=c(Linf=450,K=0.3,t0=0))    #att 3
#' findGrowthStarts(tlV~age,data=GrowthData1,plot=TRUE,fixed=c(Linf=450,K=0.3,t0=-0.5)) #looks OK, stop
#' 
#' #===== Plot at starting and final values
#' #----- creating growth function corresponding to first param of von B
#' vonb1 <- makeGrowthFun(type="von Bertalanffy")
#' #----- plot data
#' plot(tlV~age,data=GrowthData1,pch=19,col=col2rgbt("black",0.2))
#' #----- plot von b growth function at starting values (svonb1 from above)
#' curve(vonb1(x,Linf=svonb1),col="blue",lwd=5,add=TRUE)
#' #----- fit growth function to data
#' rvonb1 <- nls(tlV~vonb1(age,Linf,K,t0),data=GrowthData1,start=svonb1)
#' cvonb1 <- coef(rvonb1)
#' #----- plot growth function at final values ... starting values were very good!
#' curve(vonb1(x,Linf=cvonb1),col="red",lwd=2,add=TRUE)
#' 
#' #===== Example for tag-recapture data (in GrowthData3)
#' #----- Fabens model
#' findGrowthStarts(deltaL~deltat+tlM,data=GrowthData3,pname="Fabens")
#' #----- Francis model
#' findGrowthStarts(deltaL~deltat+tlM,data=GrowthData3,pname="Francis2",
#'                  constvals=c(L1=150,L2=400))
#' 
#' @rdname findGrowthStarts
#' @export

findGrowthStarts <- function(formula,data,
                             type=c("von Bertalanffy","Gompertz","logistic","Richards",
                                    "Schnute","Schnute-Richards"),
                             param=1,pname=NULL,case=NULL,
                             constvals=NULL,fixed=NULL,plot=FALSE) {
  #===== Checks
  # Schnute uses "case" instead of "param" ... convert to "param"
  if (!is.null(case)) {
    if(type=="Schnute") param <- case
    else STOP("'case' only used when 'type' is 'Schnute'")
  }

  # Handle checks on type, param, and pname
  type <- match.arg(type)
  param <- iHndlGrowthModelParams(type,param,pname)
  
  # Handle the formula with some checks ... need to condition for tag-return params
  expNumE <- 1
  if (!is.null(param)) {
    if (type=="von Bertalanffy" & param %in% 13:19) expNumE <- 2
    if (type=="Gompertz" & param %in% 6:7) expNumE <- 2
    if (type=="logistic" & param==4) expNumE <- 2
  }
  
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=expNumE)
  if (!tmp$metExpNumR) STOP("'formula' must have only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer"))STOP("LHS variable must be numeric.")
  if (!tmp$metExpNumE)
    STOP("'formula' must have ",ifelse(expNumE==1,'only ',''),kCounts(expNumE),
         " RHS ",ifelse(expNumE==1,'variable','variables'),".")
  if (!all(tmp$Eclass %in% c("numeric","integer")))
    STOP("RHS ",ifelse(expNumE==1,'variable','variables')," must be numeric.")
  
  # initial checks on constvals
  if (!is.null(constvals)) {
    if (!type %in% c("von Bertalanffy","Schnute"))
      STOP("'constvals' not used with the ",type," model.",
           " Either don't use 'constvals' or change 'type'")
  }
  
  # Is fixed of correct type ... will check for correct names later
  if (!is.null(fixed)) {
    if (is.list(fixed)) STOP("'fixed' should be a vector rather than a list.")
    if (!is.numeric(fixed)) STOP("'fixed' must be numeric.")
    fxdnms <- names(fixed)
    if (is.null(fxdnms)) STOP("Items in 'fixed' must be named.")
    if (any(fxdnms=="")) STOP("All items in 'fixed' must be named.")
  }
  
  #===== Get starting values
  # get variable names
  lennm <- tmp$Rname[1]
  agenm <- tmp$Enames[1]
  Lmnm <- ifelse(expNumE==2,tmp$Enames[2],"")
  
  # call specific internal function to create starting value list
  switch(type,
         "von Bertalanffy"= { sv <- iVonBStarts(lennm,agenm,Lmnm,data,param,constvals,fixed) },
         "Gompertz"=        { sv <- iGompStarts(lennm,agenm,data,param,fixed) },
         "logistic"=        { sv <- iLogiStarts(lennm,agenm,data,param,fixed) },
         "Richards"=        { sv <- iRichStarts(lennm,agenm,data,param,fixed) },
         "Schnute"=         { sv <- iSchnStarts(lennm,agenm,data,param,constvals,fixed) },
         "Schnute-Richards"={ sv <- NULL })
  
  # check if any names in fixed were not in the parameterization
  if (!is.null(fixed)) {
    if (!all(fxdnms %in% names(sv))) { # some not correctly named
      fxdnms <- fxdnms[!fxdnms %in% names(sv)]
      WARN("Some names in 'fixed' (",paste(fxdnms,collapse=", "),
           ") are not parameters (",paste(names(sv),collapse=", "),
           ") for ",type," parameterization #",param," and were ignored.")
    }
  }
  
  #===== Plot starting values (if asked to)
  if (is.null(sv)) WARN("Starting values not yet implemented in 'FSA'",
                        " for ",type," paramaterization #",param)
  else if (plot) {
    if (type=="von Bertalanffy" & param %in% 13:19)
      WARN("'plot' does not work for ",type," parameterization #",param,
           ", or for any other tag-recapture parameterization")
    else iPlotGrowStarts(sv,data,lennm,agenm,type,param,constvals)
  } 
  
  #===== Return starting value list (may be NULL, but warned above)
  sv
}


#===============================================================================
#== Internal Functions -- Compute starting values for each type of model
#===============================================================================
iVonBStarts <- function(ynm,xnm,Lmnm,data,param,constvals,fixed) {
  # Perform checks on constvals (some checks already in main function)
  iChkConstvals("von Bertalanffy",param,constvals)
  # Get names of fixed params, if they exist
  fxdnms <- names(fixed)
  
  if (param %in% 1:8) {          # Length at annual age models
    # Get starting values for SSasymp parameters
    ssform <- stats::as.formula(paste0(ynm,"~stats::SSasymp(",xnm,",Asym,R0,lrc)"))
    sstmp <- stats::getInitial(ssform,data=data)
    # Convert SSasymp parameter starting values to VB parameters
    Linf <- ifelse("Linf" %in% fxdnms,fixed[["Linf"]],sstmp[["Asym"]])
    K <- ifelse("K" %in% fxdnms,fixed[["K"]],exp(sstmp[["lrc"]]))
    L0 <- ifelse("L0" %in% fxdnms,fixed[["L0"]],sstmp[["R0"]])
    t0 <- ifelse("t0" %in% fxdnms,fixed[["t0"]],-log(Linf/(Linf-L0))/K)
    L0 <- ifelse("L0" %in% fxdnms,fixed[["L0"]],sstmp[["R0"]])
    omega <- ifelse("omega" %in% fxdnms,fixed[["omega"]],K*Linf)
    t50 <- ifelse("t50" %in% fxdnms,fixed[["t50"]],t0+log(2)/K)
    if (param==6) {
      if (names(constvals)=="Lr") {
        tr <- ifelse("tr" %in% fxdnms,fixed[["tr"]],-log((Linf-constvals[["Lr"]])/(Linf-L0))/K)
        tr <- iChkParamPos(tr,fxdnms)
        sv6 <- c(Linf=Linf,K=K,tr=tr)
      } else {
        Lr <- ifelse("Lr" %in% fxdnms,fixed[["Lr"]],Linf*(1-exp(-K*(constvals[["tr"]]-t0))))
        Lr <- iChkParamPos(Lr,fxdnms)
        sv6 <- c(Linf=Linf,K=K,Lr=Lr)
      }
    }
    if (param %in% c(7,8)) {
      L1 <- ifelse("L1" %in% fxdnms,fixed[["L1"]],Linf*(1-exp(-K*(constvals[["t1"]]-t0))))
      L3 <- ifelse("L3" %in% fxdnms,fixed[["L3"]],Linf*(1-exp(-K*(constvals[["t3"]]-t0))))
      L2 <- ifelse("L2" %in% fxdnms,fixed[["L2"]],Linf*(1-exp(-K*(mean(constvals)-t0))))
    }
  } else if (param %in% 10:12) { # Length at seasonal age models
    # Get starting values for SSasymp parameters
    ssform <- stats::as.formula(paste0(ynm,"~stats::SSasymp(",xnm,",Asym,R0,lrc)"))
    sstmp <- stats::getInitial(ssform,data=data)
    # Convert SSasymp parameter starting values to VB parameters
    Linf <- ifelse("Linf" %in% fxdnms,fixed[["Linf"]],sstmp[["Asym"]])
    K <- ifelse("K" %in% fxdnms,fixed[["K"]],exp(sstmp[["lrc"]]))
    L0 <- ifelse("L0" %in% fxdnms,fixed[["L0"]],sstmp[["R0"]])
    t0 <- ifelse("t0" %in% fxdnms,fixed[["t0"]],-log(Linf/(Linf-L0))/K)
    C <- ifelse("C" %in% fxdnms, fixed[["C"]],0.5)
    ts <- ifelse("ts" %in% fxdnms, fixed[["ts"]],0.3)
    WP <- ifelse("WP" %in% fxdnms, fixed[["WP"]],ts+0.5)
    NGT <- ifelse("NGT" %in% fxdnms, fixed[["NGT"]],0.3)
    Kpr <- ifelse("Kpr" %in% fxdnms, fixed[["Kpr"]],K/(1-NGT))
  } else if (param %in% 13:18) { # Tag-Recapture models
    # if param does not use constvals then used observe Lm at 10, 90%iles
    if (is.null(constvals)) {
      L1 <- stats::quantile(data[,Lmnm],probs=0.10)[[1]]
      L2 <- stats::quantile(data[,Lmnm],probs=0.90)[[1]]
    } else {
      L1 <- constvals[["L1"]]
      L2 <- constvals[["L2"]]
    }
    tmp <- stats::lm(stats::as.formula(paste0("I(",ynm,"/",xnm,")~",Lmnm)),data=data)
    pdf <- data.frame(c(L1,L2))
    names(pdf) <- Lmnm
    pdf <- stats::predict(tmp,data.frame(pdf))
    g1 <- ifelse("g1" %in% fxdnms, fixed[["g1"]],pdf[[1]])
    g2 <- ifelse("g2" %in% fxdnms, fixed[["g2"]],pdf[[2]])
    Linf <- ifelse("Linf" %in% fxdnms, fixed[["Linf"]],(L2*g1-L1*g2)/(g1-g2))
    K <- ifelse("K" %in% fxdnms, fixed[["K"]],-log(1+(g1-g2)/(L1-L2)))
    b <- ifelse("b" %in% fxdnms, fixed[["b"]],0.1)
    a <- ifelse("a" %in% fxdnms, fixed[["a"]],Linf-mean(data[,Lmnm],na.rm=TRUE))
  }

  # Create starting value list specific to parameterization
  sv <- NULL
  switch(as.character(param),
          "1"= {  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                          K=iChkK(K,fxdnms),
                          t0=iChkt0(t0,fxdnms))  },
          "2"= {  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                          K=iChkK(K,fxdnms),
                          L0=iChkL0(L0,data[[ynm]],data[[xnm]],fxdnms))  },
          "3"= {  sv <- c(omega=iChkParamPos(omega,fxdnms),
                          K=iChkK(K,fxdnms),
                          t0=iChkt0(t0,fxdnms))  },
          "4"= {  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                          L0=iChkL0(L0,data[[ynm]],data[[xnm]],fxdnms),
                          omega=iChkParamPos(omega,fxdnms))  },
          "5"= {  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                          t0=iChkt0(t0,fxdnms),
                          t50=iChkParamPos(t50,fxdnms))  },
          "6"= {  sv <- sv6  },
          "7"= {  sv <- c(L1=iChkParamPos(L1,fxdnms),
                          L3=iChkParamPos(L3,fxdnms),
                          K=iChkK(K,fxdnms))  },
          "8"= {  sv <- c(L1=iChkParamPos(L1,fxdnms),
                          L2=iChkParamPos(L2,fxdnms),
                          L3=iChkParamPos(L3,fxdnms))  },
         "10"= {  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                          K=iChkK(K,fxdnms),
                          t0=iChkt0(t0,fxdnms),
                          C=iChkParamBtwn(C,fxdnms),
                          ts=iChkParamBtwn(ts,fxdnms))  },
         "11"= {  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                          K=iChkK(K,fxdnms),
                          t0=iChkt0(t0,fxdnms),
                          C=iChkParamBtwn(C,fxdnms),
                          WP=iChkParamBtwn(WP,fxdnms))  },
         "12"= {  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                          Kpr=iChkParamPos(Kpr,fxdnms),
                          t0=iChkt0(t0,fxdnms),
                          ts=iChkParamBtwn(ts,fxdnms),
                          NGT=iChkParamBtwn(NGT,fxdnms))  },
         "13"= {  sv <- c(Linf=iChkParamPos(Linf,fxdnms),
                          K=iChkParamPos(K,fxdnms))  },
         "14"= {  sv <- c(Linf=iChkParamPos(Linf,fxdnms),
                          K=iChkParamPos(K,fxdnms))  },
         "15"= {  sv <- c(Linf=iChkParamPos(Linf,fxdnms),
                          K=iChkParamPos(K,fxdnms),
                          b=b)  },
         "16"= {  sv <- c(K=iChkParamPos(K,fxdnms),
                          a=a,
                          b=b)  },
         "17"= {  sv <- c(K=iChkParamPos(K,fxdnms),
                          a=a,
                          b=b)  },
         "18"= {  sv <- c(g1=iChkParamPos(g1,fxdnms),
                          g2=iChkParamPos(g2,fxdnms))  }
  )
  sv
}

iGompStarts <- function(ynm,xnm,data,param,fixed) {
  # Get names of fixed params, if they exist
  fxdnms <- names(fixed)
  
  # Get starting values for SSgompertz parameters
  ssform <- stats::as.formula(paste0(ynm,"~stats::SSgompertz(",
                                     xnm,",Asym,b2,b3)"))
  sstmp <- stats::getInitial(ssform,data=data)
  
  # Convert SSgompertz parameter starting values to VB parameters
  Linf <- ifelse("Linf" %in% fxdnms,fixed[["Linf"]],sstmp[["Asym"]])
  gi <- ifelse("gi" %in% fxdnms,fixed[["gi"]],-log(sstmp[["b3"]]))
  b2 <- sstmp[["b2"]]
  a1 <- ifelse("a1" %in% fxdnms,fixed[["a1"]],log(b2))
  a2 <- ifelse("a2" %in% fxdnms,fixed[["a2"]],b2)
  ti <- ifelse("ti" %in% fxdnms,fixed[["ti"]],log(b2)/gi)
  t0 <- ifelse("t0" %in% fxdnms,fixed[["t0"]],log(b2*gi)/gi)
  L0 <- ifelse("L0" %in% fxdnms,fixed[["L0"]],Linf/exp(b2))
  
  # Create starting value list specific to parameterization
  sv <- NULL
  switch(param,
         Gompertz1={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              gi=iChkParamPos(gi,fxdnms),
                              a1=a1)  },
         Gompertz2={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              gi=iChkParamPos(gi,fxdnms),
                              ti=iChkParamPos(ti,fxdnms))  },
         Gompertz3={  sv <- c(L0=iChkL0(L0,data[[ynm]],data[[xnm]],fxdnms),
                              gi=iChkParamPos(gi,fxdnms),
                              a2=a2)  },
         Gompertz4={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              gi=iChkParamPos(gi,fxdnms),
                              a2=a2)  },
         Gompertz5={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              gi=iChkParamPos(gi,fxdnms),
                              t0=t0)  }
  )
  
  # Return the starting value list
  sv
}

iLogiStarts <- function(ynm,xnm,data,param,fixed) {
  # Get names of fixed params, if they exist
  fxdnms <- names(fixed)
  
  # Get starting values for SSlogis parameters
  ssform <- stats::as.formula(paste0(ynm,"~stats::SSlogis(",
                                     xnm,",Asym,xmid,scal)"))
  sstmp <- stats::getInitial(ssform,data=data)
  
  # Convert SSgompertz parameter starting values to VB parameters
  Linf <- ifelse("Linf" %in% fxdnms,fixed[["Linf"]],sstmp[["Asym"]])
  xmid <- sstmp[["xmid"]]
  scal <- sstmp[["scal"]]
  gninf <- ifelse("gninf" %in% fxdnms,fixed[["gninf"]],1/scal)
  ti <- ifelse("ti" %in% fxdnms,fixed[["ti"]],xmid)
  a <- ifelse("a" %in% fxdnms,fixed[["a"]],xmid/scal)
  L0 <- ifelse("L0" %in% fxdnms,fixed[["L0"]],Linf/(1+exp(xmid/scal)))
  
  # Create starting value list specific to parameterization
  sv <- NULL
  switch(param,
         logistic1={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              gninf=iChkParamPos(gninf,fxdnms),
                              ti=iChkParamPos(ti,fxdnms))  },
         logistic2={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              gninf=iChkParamPos(gninf,fxdnms),
                              a=a)  },
         logistic3={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              gninf=iChkParamPos(gninf,fxdnms),
                              L0=iChkL0(L0,data[[ynm]],data[[xnm]],fxdnms))  }
  )
  
  # Return the starting value list
  sv
  
}

iRichStarts <- function(ynm,xnm,data,param,fixed) {
  # Get names of fixed params, if they exist
  fxdnms <- names(fixed)
  
  # Get starting values for SSposnegRichards parameters
  sstmp <- FlexParamCurve::modpar(data[[xnm]],data[[ynm]],
                                  pn.options="sstmp",width.bounds=2,force4par=TRUE,
                                  verbose=FALSE,suppress.text=TRUE)
  
  # Convert SSposnegRichards parameter starting values to Richards parameters
  Linf <- ifelse("Linf" %in% fxdnms,fixed[["Linf"]],sstmp[["Asym"]])
  k <- ifelse("k" %in% fxdnms,fixed[["k"]],sstmp[["K"]])
  ti <- ifelse("ti" %in% fxdnms,fixed[["ti"]],sstmp[["Infl"]])
  M <- sstmp[["M"]]
  t0 <- ifelse("t0" %in% fxdnms,fixed[["t0"]],-(log(1/M)/k)+ti)
  L0 <- ifelse("L0" %in% fxdnms,fixed[["L0"]],Linf*((1+M*exp(k*ti))^(-1/M)))
  b <- ifelse("b" %in% fxdnms,fixed[["b"]],-(1/M))

  
  # Create starting value list specific to parameterization
  sv <- NULL
  switch(param,
         Richards1={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              k=iChkParamPos(k,fxdnms),
                              ti=iChkParamPos(ti,fxdnms),
                              b=b)  },
         Richards2={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              k=iChkParamPos(k,fxdnms),
                              t0=t0,
                              b=b)  },
         Richards3={  sv <- c(Linf=iChkLinf(Linf,data[[ynm]],fxdnms),
                              k=iChkParamPos(k,fxdnms),
                              L0=iChkL0(L0,data[[ynm]],data[[xnm]],fxdnms),
                              b=b)  }
  )
  
  # Return the starting value list
  sv
}

iSchnStarts <- function(ynm,xnm,data,param,constvals,fixed) {
  # Perform checks on constvals (some checks already in main function)
  iChkConstvals("Schnute",param,constvals)
  # Get names of fixed params, if they exist
  fxdnms <- names(fixed)
  
  # Get mean lengths-at-ages in constvals from a loess smoother
  tmp <- stats::loess(stats::as.formula(paste0(ynm,"~",xnm)),data=data)
  pdf <- data.frame(x=constvals)
  names(pdf) <- xnm
  pdf <- stats::predict(tmp,pdf)
  L1 <- ifelse("L1" %in% fxdnms,fixed[["L1"]],pdf[[1]])
  L3 <- ifelse("L3" %in% fxdnms,fixed[["L3"]],pdf[[2]])
  
  # Set a and b as noted in Starting Values article
  a <- ifelse("a" %in% fxdnms,fixed[["a"]],0.3)
  b <- ifelse("b" %in% fxdnms,fixed[["b"]],ifelse(param==3,3,0.5))
  
  if ((!"a" %in% fxdnms) & (param %in% c(1,2)))
    WARN("Automated starting values for 'a' are ad hoc and may not work well. ",
         "If 'nls' model does not converge consider other values for 'a' by ",
         "using 'fixed='.")
  if ((!"b" %in% fxdnms) & (param %in% c(1,3)))
    WARN("Automated starting values for 'b' are ad hoc and may not work well ",
         ", especially when 'case=3' (or 'param=3'). ",
         "If 'nls' model does not converge consider other values for 'b' by ",
         "using 'fixed='.")
  
  # Create starting value list specific to case/parameterization
  sv <- NULL
  switch(as.character(param),
         "1"={  sv <- c(L1=iChkParamPos(L1,fxdnms),
                        L3=iChkParamPos(L3,fxdnms),
                        a=a,
                        b=b)  },
         "2"={  sv <- c(L1=iChkParamPos(L1,fxdnms),
                        L3=iChkParamPos(L3,fxdnms),
                        a=a)  },
         "3"={  sv <- c(L1=iChkParamPos(L1,fxdnms),
                        L3=iChkParamPos(L3,fxdnms),
                        b=b)  },
         "4"={  sv <- c(L1=iChkParamPos(L1,fxdnms),
                        L3=iChkParamPos(L3,fxdnms))  }
  )
  
  # Return the starting value list
  sv
}


#===============================================================================
#== Internal Functions -- Parameter sanity checks
#===============================================================================
iChkConstvals <- function(type,param,constvals) {
  if (type=="Schnute") {
    if (is.null(constvals))
      STOP("You must use 'constvals' with Schnute model")
    if (!is.null(constvals)) {
      # check type and whether named first
      if (is.list(constvals)) STOP("'constvals' should be a vector rather than a list.")
      if (!is.numeric(constvals)) STOP("'constvals' must be numeric.")
      if (is.null(names(constvals))) STOP("Values in 'constvals' must be named.")
      cvnms <- names(constvals)
      if (length(cvnms)!=2)
        STOP("'constvals' must have exactly two values ('t1' and 't3') for 'Schnute' model." )
      if (!all(cvnms %in% c("t1","t3")))
        STOP("Value names in 'constvals' must be 't1' and 't3' for 'Schnute' model.")
    }
  } else {
    if (is.null(constvals) & param %in% c(6,7,8,18))
      STOP("You must use 'constvals' with von Bertalanffy paramaterization #",param)
    if (!is.null(constvals)) {
      # check type and whether named first
      if (is.list(constvals)) STOP("'constvals' should be a vector rather than a list.")
      if (!is.numeric(constvals)) STOP("'constvals' must be numeric.")
      if (is.null(names(constvals))) STOP("Values in 'constvals' must be named.")
      
      # Ok type and named, now check if used properly
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
      if (param %in% 13:17) { # tag-recapture but not the next two
        if (length(cvnms)!=2) 
          STOP("'constvals' not required when 'param=",param,
               "', but if used then there must be exactly two values ('L1' and 'L2')")
        if (!all(cvnms %in% c("L1","L2")))
          STOP("'constvals' not required when 'param=",param,
               "', but if used then names in 'constvals' must be 'L1' and 'L2'")
      }
      if (param %in% c(18,19)) { # Francis parameterizations
        if (length(cvnms)!=2) 
          STOP("'constvals' must have exactly two values ('L1' and 'L2') when 'param=",
               param,"'" )
        if (!all(cvnms %in% c("L1","L2")))
          STOP("Value names in 'constvals' must be 'L1' and 'L2' when 'param=",param,"'")
      }
    }
  }
}

iChkLinf <- function(sLinf,len,fxdnms) {
  tmp <- NULL
  if (is.lte(sLinf,0))
    tmp <- "Starting value for 'Linf' is negative which does not make biological sense."
  else if (is.lt(sLinf,0.5*max(len,na.rm=TRUE)) | is.gt(sLinf,1.5*max(len,na.rm=TRUE)))
    tmp <- "Starting value for 'Linf' is very different from the observed maximum length."
  if (!is.null(tmp)) {
    wasFixed <- "Linf" %in% fxdnms
    if (wasFixed) tmp <- paste(tmp,"This suggests a problem with your fixed 'Linf' value.")
      else tmp <- paste(tmp,"This suggests a model fitting problem.")
    tmp <- paste(tmp,"Try plotting the data with the model at the starting values",
                 "superimposed with 'plot=TRUE' to potentially diagnose any problems.")
    if (!wasFixed) tmp <- paste(tmp,"Also consider using 'fixed=c(Linf=)'",
                                "to manually set 'Linf' to a reasonable value.\n")
    WARN(tmp)
  }
  # return starting value
  sLinf
}

iChkL0 <- function(sL0,len,age,fxdnms) {
  tmp <- NULL
  if (is.lt(sL0,0))
    tmp <- paste("Starting value for 'L0' is negative, which does not make biolgical sense.")
  else if (is.gt(sL0,1.25*min(tapply(len,age,mean,na.rm=TRUE))))
    tmp <- paste("Starting value for 'L0' is more than 25% greater than the minimum observed",
                 "mean length-at-age, which is possible but unlikely.")
  if (!is.null(tmp)) {
    wasFixed <- "L0" %in% fxdnms
    if (wasFixed) tmp <- paste(tmp,"This suggests a problem with your fixed 'L0' value.")
    else tmp <- paste(tmp,"This suggests a model fitting problem.")
    tmp <- paste(tmp,"Try plotting the data with the model at the starting values",
                 "superimposed with 'plot=TRUE' to potentially diagnose any problems.")
    if (!wasFixed) tmp <- paste(tmp,"Also consider using 'fixed=c(L0=)'",
                                "to manually set 'L0' to a reasonable value.\n")
    WARN(tmp)
  }
  # return starting value
  sL0
}

iChkK <- function(sK,fxdnms) {
  tmp <- NULL
  if (is.lt(sK,0))
    tmp <- paste("Starting value for 'K' is negative, which does not make biolgical sense.")
  else if (is.gt(sK,1.5))
    tmp <- paste("Starting value for 'K' is greater than 1.5; which is possible, but unlikely.")
  if (!is.null(tmp)) {
    wasFixed <- "K" %in% fxdnms
    if (wasFixed) tmp <- paste(tmp,"This suggests a problem with your fixed 'K' value.")
      else tmp <- paste(tmp,"This suggests a model fitting problem.")
    tmp <- paste(tmp,"Try plotting the data with the model at the starting values",
                 "superimposed with 'plot=TRUE' to potentially diagnose any problems.")
    if (!wasFixed) tmp <- paste(tmp,"Also consider using 'fixed=c(K=)'",
                                "to manually set 'K' to a reasonable value.\n")
    WARN(tmp)
  }
  # return starting value
  sK
}

iChkt0 <- function(st0,fxdnms) {
  tmp <- NULL
  if (is.lt(st0,-6))
    tmp <- paste("Starting value for 't0' is less than -6, which is possible but unlikely.")
  else if (is.gt(st0,2))
    tmp <- paste("Starting value for 't0' is greater than 2, which is possible but unlikely.")
  if (!is.null(tmp)) {
    wasFixed <- "t0" %in% fxdnms
    if (wasFixed) tmp <- paste(tmp,"This suggests a problem with your fixed 't0' value.")
      else tmp <- paste(tmp,"This suggests a model fitting problem.")
    tmp <- paste(tmp,"Try plotting the data with the model at the starting values",
                 "superimposed with 'plot=TRUE' to potentially diagnose any problems.")
    if (!wasFixed) tmp <- paste(tmp,"Also consider using 'fixed=c(t0=)'",
                                "to manually set 't0' to a reasonable value.\n")
    WARN(tmp)
  }
  # return starting value
  st0
}

iChkParamPos <- function(x,fxdnms) {
  # Determine if x value was negative
  if (is.lt(x,0)) {
    # Extract name of the parameter
    xnm <- deparse(substitute(x))
    # Determine if the starting value was fixed or not
    wasFixed <- xnm %in% fxdnms
    # start warning message
    tmp <- paste0("Starting value for '",xnm,"' is negative, ",
                  "which does not make biolgical sense. ")
    # Warning message depends on whether parameter was fixed or not
    if (wasFixed) tmp <- paste0(tmp,"This suggests a problem with your fixed '",
                                xnm,"' value. ")
    else tmp <- paste0(tmp,"This suggests a model fitting problem. ")
    # Continue message with a suggestion
    tmp <- paste0(tmp,"Try plotting the data with the model at the starting values ",
                 "superimposed with 'plot=TRUE' to potentially diagnose any problems.")
    if (!wasFixed) tmp <- paste0(tmp," Also consider using 'fixed=c(",xnm,"=)' ",
                                "to manually set '",xnm,"' to a reasonable value.")
    # Add return to end of string
    tmp <- paste0(tmp,"\n")
    # Issue the warning
    WARN(tmp)
  }
  # return x value
  x
}

iChkParamBtwn <- function(x,fxdnms,low=0,high=1) {
  # Determine if x value was negative
  if (is.lt(x,low) | is.gt(x,high)) {
    # Extract name of the parameter
    xnm <- deparse(substitute(x))
    # Determine if the starting value was fixed or not
    wasFixed <- xnm %in% fxdnms
    # start warning message
    tmp <- paste0("Starting value for '",xnm,"' must be bewteen ",
                  low," and ",high,". ")
    # Warning message depends on whether parameter was fixed or not
    if (wasFixed) tmp <- paste0(tmp,"This suggests a problem with your fixed '",
                                xnm,"' value. ")
    else tmp <- paste0(tmp,"This suggests a model fitting problem. ")
    # Continue message with a suggestion
    tmp <- paste0(tmp,"Try plotting the data with the model at the starting values ",
                  "superimposed with 'plot=TRUE' to potentially diagnose any problems.")
    if (!wasFixed) tmp <- paste0(tmp," Also consider using 'fixed=c(",xnm,"=)' ",
                                 "to manually set '",xnm,"' to a reasonable value ",
                                 "between ",low," and ",high,".")
    # Add return to end of string
    tmp <- paste0(tmp,"\n")
    # Issue the warning
    WARN(tmp)
  }
  # return x value
  x
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
  
  # Plotting must be handled slightly differently for paramaterizations that
  #   have constvals to be given to mdl arguments
  if ((type=="von Bertalanffy" & param %in% c(7,8)) | (type=="Schnute")) {
    graphics::curve(mdl(x,sv,t1=constvals),col="red",lwd=3,add=TRUE)
  } else {
    # get the argument values (those from sv and from constvals)
    tmp <- c(sv,constvals)
    # make sure arguments in same order as expected from mdl
    tmp <- tmp[names(formals(mdl)[-1])]
    # add the curve
    graphics::curve(mdl(x,tmp),col="red",lwd=3,add=TRUE)
  }
  # Put the starting values on the plot
  graphics::legend("bottomright",
                   paste(names(sv),formatC(sv,format="f",digits=2),
                         sep="="),bty="n")
} # nocov end
