#' @title Creates a string or an expression for a specific growth function.
#' 
#' @description Creates a string or expression for a specific parameterization of the von Bertalanffy, Gompertz, Richards, and logistic growth functions, as well as the Schnute and Schnute-Richards growth functions. Parameters may be replaced with values from a model fit. The string or expression can be added to user plots as titles, annotations, etc. The string/expression can also be plotted to a blank plot with \code{plot=TRUE} to see the equation of the growth function.
#'
#' @inheritParams makeGrowthFun
#' @param case A numeric that indicates the specific case of the Schnute function to use.
#' @param parse A logical indicating whether a string (\code{FALSE}; default) or an expression (\code{TRUE}) should be returned.
#' @param yvar A string that represents the right-hand-side (or y-variable) of the equation. Defaults to \code{NULL} such that a reasonable default for the model type will be chosen.
#' @param xvar A string that represents the left-hand-side (or x) variable) of the equation. Defaults to \code{NULL} such that \eqn{t} will be used for models with ages and \eqn{Delta*t} will be used for models with tag-recapture data.
#' @param fit An optional \code{nls} (or related) object from fitting data to the growth function. If \code{NULL} then a string/expression with symbols for parameters will be returned. If an \code{nls} object then values for the parameters will be extracted from \code{fit} and put in place of the parameters symbols.
#' @param constvals A NAMED numeric vector of constant values (either lengths or ages) to be used in some of the von Bertalanffy parameterizations. See details.
#' @param digits An optional numerical vector for which to round the parameter values. Only used if \code{fit} is not \code{NULL}. Digits must be in the same order as the order of parameters for the growth model as in \code{\link{makeGrowthFun}} and should include values for the model constants given in \code{constvals} (if so used).
#' @param stackWhere A logical that indicates whether strings/expressions that use \dQuote{where} to explain a constant or function that simplifies the expression of the equation should be shown in \dQuote{inline} (\code{FALSE}; default) or \dQuote{stacked} (\code{TRUE}). See examples.
#' @param plot A logical for whether the expression should be shown on a \dQuote{blank} plot. See examples.
#' @param \dots Arguments for \code{plot}. In particular use \code{cex=} to make the expression larger and easier to read. See examples. 
#'
#' @returns A string or expression representing the equation of the growth function given in \code{type} and \code{param}/\code{pname}.
#'
#' @seealso See \code{\link{makeGrowthFun}} to make functions that correspond to these expressions.
#' 
#' @examples
#' #===== The string (first) and expression (second) for default type="von Bertalanffy")
#' showGrowthFun()
#' showGrowthFun(parse=TRUE)
#' showGrowthFun(pname="Typical")
#' 
#' #===== Show on a plot, and then larger
#' showGrowthFun(plot=TRUE)
#' showGrowthFun(plot=TRUE,cex=2)
#' 
#' #===== Other growth functions
#' showGrowthFun(type="Richards",param=3,plot=TRUE,cex=1.5)
#' showGrowthFun(type="Schnute",case=2,plot=TRUE,cex=1.5)
#' 
#' #===== Growth functions which use "where" to define simplifying constants/functions
#' showGrowthFun(pname="Somers",plot=TRUE)
#' showGrowthFun(pname="Somers",stackWhere=TRUE,plot=TRUE,cex=1.25)
#' 
#' #===== Multiple expressions in one plot (need to use parse=TRUE here)
#' op <- par(mar=c(0.1,0.1,0.1,0.1))
#' plot(0,type="n",xlab="",ylab="",xlim=c(0,1),ylim=c(0,3),xaxt="n",yaxt="n")
#' text(0,2.5,"Original:",pos=4)
#' text(0.5,2.5,showGrowthFun(type="von Bertalanffy",pname="Original",parse=TRUE))
#' text(0,1.5,"Typical:",pos=4)
#' text(0.5,1.5,showGrowthFun(type="von Bertalanffy",pname="Typical",parse=TRUE))
#' text(0,0.5,"Francis:",pos=4)
#' text(0.5,0.5,showGrowthFun(type="von Bertalanffy",pname="Francis",parse=TRUE))
#' par(op)
#' 
#' #===== Put expression in title or otherwise on the plot
#' # Make a von Bertalanffy function
#' vb1 <- makeGrowthFun()
#'  # Get and save the expression of the von Bertalanffy growth function
#' tmp <- showGrowthFun(parse=TRUE)
#' 
#' # Make plot and put expression in plot title
#' ages <- 1:20
#' plot(vb1(ages,Linf=20,K=0.3,t0=-0.2)~ages,type="b",pch=19,ylab="Length",main=tmp)
#' 
#' # Put expression in plot body (as demo)
#' text(15,10,tmp)
#'
#' #===== Fill expression with values from model fit
#' # Fit von Bertalanffy to GrowthData1 data
#' sv <- findGrowthStarts(tlV~age,data=GrowthData1)
#' rv <- nls(tlV~vb1(age,Linf,K,t0),data=GrowthData1,start=sv)
#' 
#' # Show expression with values
#' showGrowthFun(fit=rv,plot=TRUE)
#' # Same, but control decimals (Linf, K, and then t0 order as in vb1())
#' showGrowthFun(fit=rv,digits=c(1,5,3),plot=TRUE)
#' # Same, but change variables
#' showGrowthFun(fit=rv,yvar="Length",xvar="Age",plot=TRUE)
#' 
#' # Put on a plot
#' plot(tlV~age,data=GrowthData1,ylab="Length (mm)",xlab="Age (yrs)")
#' curve(vb1(x,Linf=coef(rv)),from=0,to=15,col="blue",lwd=2,add=TRUE)
#' text(10,150,showGrowthFun(fit=rv,parse=TRUE))
#' 
#' # Put on a ggplot (note parse=TRUE is outside showGrowthFun)
#' \dontrun{
#' library(ggplot2)
#' ggplot(data=GrowthData1,mapping=aes(y=tlV,x=age)) +
#' geom_point() +
#' stat_function(fun=vb1,args=list(Linf=coef(rv)),color="blue",linewidth=1) +
#' annotate(geom="text",label=showGrowthFun(fit=rv),parse=TRUE,size=4,x=10,y=150) +
#' labs(y="Length (mm)",x="Age (yrs)") +
#' theme_bw()
#' }
#' 
#' @rdname showGrowthFun
#' @export

showGrowthFun <- function(type=c("von Bertalanffy","Gompertz","Richards",
                                 "logistic","Schnute","Schnute-Richards"),
                          param=1,pname=NULL,case=NULL,constvals=NULL,
                          parse=FALSE,yvar=NULL,xvar=NULL,
                          fit=NULL,digits=NULL,stackWhere=FALSE,plot=FALSE,...) {
  #===== Checks
  # Schnute uses "case" instead of "param" ... convert to "param"
  if (!is.null(case)) {
    if(type=="Schnute") param <- case
    else STOP("'case' only used when 'type' is 'Schnute'")
  }
  
  # Handle checks on type, param, and pname
  type <- match.arg(type)
  param <- iHndlGrowthModelParams(type,param,pname)
  
  #===== Send to proper internal function to make the string
  # make a combined parameter name ... remove spaces and hyphens from type
  pnm <- paste0(gsub(" ","",type),param)
  pnm <- gsub("-","",pnm)
    
  switch(pnm,
         "vonBertalanffy1"  = {res <- iMakeEqnVB1(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy2"  = {res <- iMakeEqnVB2(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy3"  = {res <- iMakeEqnVB3(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy4"  = {res <- iMakeEqnVB4(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy5"  = {res <- iMakeEqnVB5(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy6"  = {res <- iMakeEqnVB6(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy7"  = {res <- iMakeEqnVB7(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy8"  = {res <- iMakeEqnVB8(yvar,xvar,fit,constvals,digits,stackWhere)},
         "vonBertalanffy9"  = {res <- NULL},
         "vonBertalanffy10" = {res <- iMakeEqnVB10(yvar,xvar,fit,constvals,digits,stackWhere)},
         "vonBertalanffy11" = {res <- iMakeEqnVB11(yvar,xvar,fit,constvals,digits,stackWhere)},
         "vonBertalanffy12" = {res <- iMakeEqnVB12(yvar,xvar,fit,constvals,digits,stackWhere)},
         "vonBertalanffy13" = {res <- iMakeEqnVB13(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy14" = {res <- iMakeEqnVB14(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy15" = {res <- iMakeEqnVB15(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy16" = {res <- iMakeEqnVB16(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy17" = {res <- iMakeEqnVB17(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy18" = {res <- iMakeEqnVB18(yvar,xvar,fit,constvals,digits)},
         "vonBertalanffy19" = {res <- NULL},
         "Gompertz1" = {res <- iMakeEqnGomp1(yvar,xvar,fit,constvals,digits)},
         "Gompertz2" = {res <- iMakeEqnGomp2(yvar,xvar,fit,constvals,digits)},
         "Gompertz3" = {res <- iMakeEqnGomp3(yvar,xvar,fit,constvals,digits)},
         "Gompertz4" = {res <- iMakeEqnGomp4(yvar,xvar,fit,constvals,digits)},
         "Gompertz5" = {res <- iMakeEqnGomp5(yvar,xvar,fit,constvals,digits)},
         "Gompertz6" = {res <- iMakeEqnGomp6(yvar,xvar,fit,constvals,digits)},
         "Gompertz7" = {res <- iMakeEqnGomp7(yvar,xvar,fit,constvals,digits)},
         "logistic1" = {res <- iMakeEqnLogi1(yvar,xvar,fit,constvals,digits)},
         "logistic2" = {res <- iMakeEqnLogi2(yvar,xvar,fit,constvals,digits)},
         "logistic3" = {res <- iMakeEqnLogi3(yvar,xvar,fit,constvals,digits)},
         "logistic4" = {res <- iMakeEqnLogi4(yvar,xvar,fit,constvals,digits)},
         "Richards1" = {res <- iMakeEqnRich1(yvar,xvar,fit,constvals,digits)},
         "Richards2" = {res <- iMakeEqnRich2(yvar,xvar,fit,constvals,digits)},
         "Richards3" = {res <- iMakeEqnRich3(yvar,xvar,fit,constvals,digits)},
         "Schnute1"  = {res <- iMakeEqnSchnute(param,yvar,xvar,fit,constvals,digits)},
         "Schnute2"  = {res <- iMakeEqnSchnute(param,yvar,xvar,fit,constvals,digits)},
         "Schnute3"  = {res <- iMakeEqnSchnute(param,yvar,xvar,fit,constvals,digits)},
         "Schnute4"  = {res <- iMakeEqnSchnute(param,yvar,xvar,fit,constvals,digits)},
         "SchnuteRichards" = {res <- iMakeEqnSchnRich(yvar,xvar,fit,constvals,digits)}
         )
  
  if (is.null(res)) STOP("Functionality not yet implemented for ",type,
                         " parameterization #",param)

  #===== Write expression on a plot if asked for 
  if (plot) {
    withr::local_par(list(mar=c(0.1,0.1,0.1,0.1)))
    graphics::plot(0,type="n",ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",
                   xlab="",ylab="",bty="n",...)
    graphics::text(0.5,0.5,parse(text=res),...)
  }
  #===== Return result
  # parse to an expression if asked for
  if (parse) res <- parse(text=res)
  res
}

iRoundCoefs <- function(fit,constvals,digits,def_digits) {
  # Get coefficients
  cfs <- stats::coef(fit)
  # Append on constvals if they exist
  if (!is.null(constvals)) cfs <- c(cfs,constvals)
  # If no digits declared by user then use supplied default digits
  if (is.null(digits)) digits <- def_digits
  # Check that digits are correct length
  if (!length(digits) %in% c(1,length(cfs)))
    STOP("Length of 'digits' must equal 1 or number of function ",
         "parameters plus constant values (=",length(cfs),").")
  if (length(digits)==1) digits <- rep(digits,length(cfs))
  # Round coefficients to digits and return
  purrr::map2_chr(cfs,digits,\(x,y) formatC(x,format="f",digits=y))
}

iHndlt0 <- function(t0) {
  # Create a list with the sign before t0 and value of t0
  # Start by assuming t0 is positive (and thus keep the negative sign)
  tmp <- list("-",t0)
  # If t0 is neg then sign should be pos, and neg removed from val (i.e., minus a neg)
  if (grepl("-",t0)) tmp <- list("+",gsub("-","",t0))
  # name list items and return
  names(tmp) <- c("sgn","val")
  tmp
}

iHndlbeta <- function(beta) {
  # Create a list with the sign before beta and value of beta
  # Start by assuming beta is positive (and thus keep the positive sign in front of beta)
  tmp <- list("+",beta)
  # If beta is neg then sign should be neg, and neg removed from val (i.e., minus a neg)
  if (grepl("-",beta)) tmp <- list("-",gsub("-","",beta))
  # name list items and return
  names(tmp) <- c("sgn","val")
  tmp
}

iMakeEqnVB1 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*~bgroup('[',1-e^{-K*(",xvar,"~-~t[0])},']')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    K <- cfs[["K"]]
    t0 <- iHndlt0(cfs[["t0"]])
    # Put together and return
    paste0(yvar,"=='",Linf,"'~bgroup('[',1-e^{-'",K,"'*(",xvar,t0$sgn,"'",t0$val,"')},']')")
  }
}

iMakeEqnVB2 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]~-~(L[infinity]-L[0])*~e^{-K",xvar,"}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,1))
    # Isolate coefficients (and control decimals)
    Linf <- formatC(cfs[["Linf"]],format="f",digits=digits[1])
    K <- formatC(cfs[["K"]],format="f",digits=digits[2])
    L0 <- formatC(cfs[["L0"]],format="f",digits=digits[3])
    # Put together and return
    paste0(yvar,"=='",Linf,"'~-~('",Linf,"'-'",L0,"')*~e^{-'",K,"'*",xvar,"}")
  }
}

iMakeEqnVB3 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==frac(omega,K)*~bgroup('[',1-e^{-K*(",xvar,"~-~t[0])},']')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(1,3,2))
    # Isolate coefficients
    omega <- cfs[["omega"]]
    K <- cfs[["K"]]
    t0 <- iHndlt0(cfs[["t0"]])
    # Put together and return
    paste0(yvar,"==frac('",omega,"','",K,"')*~bgroup('[',1-e^{-'",K,"'*(",xvar,t0$sgn,"'",t0$val,"')},']')")
   }
}

iMakeEqnVB4 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]~-~(L[infinity]-L[0])*~e^{-~frac(omega,L[infinity])*~",xvar,"}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,0,1))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    L0 <- cfs[["L0"]]
    omega <- cfs[["omega"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'~-~('",Linf,"'-'",L0,"')*~e^{-~frac('",omega,"','",Linf,"')*~",xvar,"}")
  }
}

iMakeEqnVB5 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*~bgroup('[',1-e^{-log(2)*~frac(",xvar,"~-~t[0],t[50]~-~t[0])},']')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,2,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    t50 <- cfs[["t50"]]
    t0 <- iHndlt0(cfs[["t0"]])
    # Put together and return
    paste0(yvar,"=='",Linf,"'*~bgroup('[',1-e^{-log(2)*~frac(",xvar,t0$sgn,"'",t0$val,"','",t50,"'",t0$sgn,"'",t0$val,"')},']')")
  }
}

iMakeEqnVB6 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]~-~(L[infinity]-L[r])*~e^{-K(",xvar,"~-~t[r])}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    if (names(constvals)=="tr") def_digits <- c(0,3,0,0)
      else def_digits <- c(0,3,2,0)
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=def_digits)
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    K <- cfs[["K"]]
    tr <- cfs[["tr"]]
    Lr <- cfs[["Lr"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'~-~('",Linf,"'-'",Lr,"')*~e^{-'",K,"'(",xvar,"-'",tr,"')}")
  }
}

iMakeEqnVB7 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[1]+(L[3]-L[1])*~frac(1-e^{-K*(",xvar,"~-~t[1])},1-e^{-K*(t[3]~-~t[1])})")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,0,3,0,0))
    # Isolate coefficients
    L1 <- cfs[["L1"]]
    L3 <- cfs[["L3"]]
    K <- cfs[["K"]]
    t1 <- cfs[["t1"]]
    t3 <- cfs[["t3"]]
    # Put together and return
    paste0(yvar,"=='",L1,"'+('",L3,"'-'",L1,"')*~frac(1-e^{-'",K,"'*(",xvar,"~-~'",t1,"')},1-e^{-'",K,"'*('",t3,"'~-~'",t1,"')})")
  }
}

iMakeEqnVB8 <- function(yvar,xvar,fit,constvals,digits,stackWhere) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    pt1 <- paste0(yvar,"==L[1]+(L[3]-L[1])*~frac(1-r^{~2~frac(",xvar,
                  "-t[1],t[3]-t[1])},1-r^{~2})~plain(',')")
    pt2 <- paste0("plain('   where ')~r==frac(L[3]-L[2],L[2]-L[1])")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,0,0,0,0))
    # Isolate coefficients
    L1 <- cfs[["L1"]]
    L2 <- cfs[["L2"]]
    L3 <- cfs[["L3"]]
    t1 <- cfs[["t1"]]
    t3 <- cfs[["t3"]]
    # Put together and return
    pt1 <- paste0(yvar,"=='",L1,"'+('",L3,"'-'",L1,"')*~frac(1-r^{~2~frac(",xvar,
                  "-'",t1,"','",t3,"'-'",t1,"')},1-r^{~2})~plain(',')")
    pt2 <- paste0("plain('   where ')~r==frac('",L3,"'-'",L2,"','",L2,"'-'",L1,"')")
  }
  if (!stackWhere) paste(pt1,pt2,sep="~")
    else paste("atop(",pt1,",",pt2,")")
}

iMakeEqnVB10 <- function(yvar,xvar,fit,constvals,digits,stackWhere) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    pt1 <- paste0(yvar,"==L[infinity]*~bgroup('[',1-e^{-K*(",xvar,
                  "~-~t[0])-S(",xvar,")+S(t[0])},']')~plain(',')")
    pt2 <- paste0("plain(' where  ')~S(t)==frac(C*K,2*pi)*~sin*bgroup('(',2*pi*(t-t[s]),')')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2,2,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    K <- cfs[["K"]]
    t0 <- iHndlt0(cfs[["t0"]])
    t02 <- cfs[["t0"]]  # needed for actual t0 in S()
    C <- cfs[["C"]]
    ts <- cfs[["ts"]]
    # Put together and return
    pt1 <- paste0(yvar,"=='",Linf,"'*~bgroup('[',1-e^{-'",K,"'*(",xvar,t0$sgn,"'",
                  t0$val,"')-S(",xvar,")+S('",t02,"')},']')~plain(',')")
    pt2 <- paste0("plain(' where  ')~S(t)==frac('",C,"'%*%'",K,
                  "',2*pi)*~sin*bgroup('(',2*pi*(t-'",ts,"'),')')")
  }
  if (!stackWhere) paste(pt1,pt2,sep="~")
    else paste("atop(",pt1,",",pt2,")")
}

iMakeEqnVB11 <- function(yvar,xvar,fit,constvals,digits,stackWhere) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    pt1 <- paste0(yvar,"==L[infinity]*~bgroup('[',1-e^{-K*(",xvar,
                  "~-~t[0])-R(",xvar,")+R(t[0])},']')~plain(',')")
    pt2 <- paste0("plain(' where  ')~R(t)==frac(C*K,2*pi)*~sin*bgroup('(',2*pi*(t-WP+0.5),')')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2,2,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    K <- cfs[["K"]]
    t0 <- iHndlt0(cfs[["t0"]])
    t02 <- cfs[["t0"]]  # needed for actual t0 in R()
    C <- cfs[["C"]]
    WP <- cfs[["WP"]]
    # Put together and return
    pt1 <- paste0(yvar,"=='",Linf,"'*~bgroup('[',1-e^{-'",K,"'*(",xvar,t0$sgn,"'",
                  t0$val,"')-R(",xvar,")+R('",t02,"')},']')~plain(',')")
    pt2 <- paste0("plain(' where  ')~R(t)==frac('",C,"'%*%'",K,
                  "',2*pi)*~sin*bgroup('(',2*pi*(t-'",WP,"'+0.5),')')")
  }
  if (!stackWhere) paste(pt1,pt2,sep="~")
  else paste("atop(",pt1,",",pt2,")")
}

iMakeEqnVB12 <- function(yvar,xvar,fit,constvals,digits,stackWhere) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "tpr"
  if (is.null(fit)) { # Make generic equation and return
    pt1 <- paste0(yvar,"==L[infinity]*~bgroup('[',1-e^{-Kpr*(",xvar,
                  "~-~t[0])-V(",xvar,")+V(t[0])},']')~plain(',')")
    pt2 <- paste0("plain(' where  ')~V(t)==frac(Kpr(1-NGT),2*pi)*~sin~bgroup('(',frac(2*pi*(t-t[s]),1-NGT),')')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,1,2,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    Kpr <- cfs[["Kpr"]]
    t0 <- iHndlt0(cfs[["t0"]])
    t02 <- cfs[["t0"]]  # needed for actual t0 in R()
    ts <- cfs[["ts"]]
    NGT <- cfs[["NGT"]]
    # Put together and return
    pt1 <- paste0(yvar,"=='",Linf,"'*~bgroup('[',1-e^{-'",Kpr,"'*(",xvar,t0$sgn,
                  "'",t0$val,"')-V(",xvar,")+V('",t02,"')},']')~plain(',')")
    pt2 <- paste0("plain(' where  ')~V(t)==frac('",Kpr,"'(1-'",NGT,
                  "'),2*pi)*~sin~bgroup('(',frac(2*pi*(t-'",ts,"'),1-'",NGT,"'),')')")
  }
  if (!stackWhere) paste(pt1,pt2,sep="~")
  else paste("atop(",pt1,",",pt2,")")
}

iMakeEqnVB13 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r]-L[m])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==(L[infinity]-L[m])*~bgroup('(',1-e^{-K*",xvar,"},')')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    K <- cfs[["K"]]
    # Put together and return
    paste0(yvar,"==('",Linf,"'-L[m])*~bgroup('(',1-e^{-'",K,"'*",xvar,"},')')")
  }
}

iMakeEqnVB14 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[m]+(L[infinity]-L[m])*~bgroup('(',1-e^{-K*",xvar,"},')')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    K <- cfs[["K"]]
    # Put together and return
    paste0(yvar,"==L[m]+('",Linf,"'-L[m])*~bgroup('(',1-e^{-'",K,"'*",xvar,"},')')")
  }
}

iMakeEqnVB15 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r]-L[m])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==bgroup('(',L[infinity]+b*(bar(L)[m]-L[m])-L[m],')')*~bgroup('(',1-e^{-K*",xvar,"},')')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,3))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    K <- cfs[["K"]]
    b <- iHndlbeta(cfs[["b"]])
    # Put together and return
    paste0(yvar,"==bgroup('(','",Linf,"'",b$sgn,"'",b$val,"'*(bar(L)[m]-L[m])-L[m],')')*~bgroup('(',1-e^{-'",K,"'*",xvar,"},')')")
  }
}

iMakeEqnVB16 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r]-L[m])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==bgroup('(',a+b*L[m],')')*~bgroup('(',1-e^{-K*",xvar,"},')')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(3,1,3))
    # Isolate coefficients
    K <- cfs[["K"]]
    a <- cfs[["a"]]
    b <- iHndlbeta(cfs[["b"]])
    # Put together and return
    paste0(yvar,"==bgroup('(','",a,"'",b$sgn,"'",b$val,"'*L[m],')')*~bgroup('(',1-e^{-'",K,"'*",xvar,"},')')")
  }
}

iMakeEqnVB17 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[m]+bgroup('(',a+b*L[m],')')*~bgroup('(',1-e^{-K*",xvar,"},')')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(3,1,3))
    # Isolate coefficients
    K <- cfs[["K"]]
    a <- cfs[["a"]]
    b <- iHndlbeta(cfs[["b"]])
    # Put together and return
    paste0(yvar,"==L[m]+bgroup('(','",a,"'",b$sgn,"'",b$val,"'*L[m],')')*~bgroup('(',1-e^{-'",K,"'*",xvar,"},')')")
  }
}

iMakeEqnVB18 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r]-L[m])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==bgroup('[',frac(L[2]*g[1]-L[1]*g[2],g[1]-g[2])~-L[m],']')~bgroup('[',1-bgroup('(',1+frac(g[1]-g[2],L[1]-L[2]),')')^{",xvar,"},']')")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,0,3,3))
    # Isolate coefficients
    L1 <- cfs[["L1"]]
    L2 <- cfs[["L2"]]
    g1 <- cfs[["g1"]]
    g2 <- cfs[["g2"]]
    # Put together and return
    paste0(yvar,"==bgroup('[',frac('",L2,"'%*%'",g1,"'-'",L1,"'%*%'",g2,"','",g1,"'-'",g2,"')~-L[m],']')~bgroup('[',1-bgroup('(',1+frac('",g1,"'-'",g2,"','",L1,"'-'",L2,"'),')')^{",xvar,"},']')")
  }
}

iMakeEqnGomp1 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*e^{-e^{a[1]-g[i]*",xvar,"}}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gi <- cfs[["gi"]]
    a1 <- cfs[["a1"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*e^{-e^{'",a1,"'-'",gi,"'*",xvar,"}}")
  }
}

iMakeEqnGomp2 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*e^{-e^{-g[i]*(",xvar,"~-~t[i])}}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gi <- cfs[["gi"]]
    ti <- cfs[["ti"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*e^{-e^{-'",gi,"'*(",xvar,"~-~'",ti,"')}}")
  }
}

iMakeEqnGomp3 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[0]*e^{a[2]*~bgroup('(',1-e^{-g[i]*",xvar,"},')')}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2))
    # Isolate coefficients
    L0 <- cfs[["L0"]]
    gi <- cfs[["gi"]]
    a2 <- cfs[["a2"]]
    # Put together and return
    paste0(yvar,"=='",L0,"'*e^{'",a2,"'*bgroup('(',1-e^{-'",gi,"'*",xvar,"},')')}")
  }
}

iMakeEqnGomp4 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*e^{-a[2]*~e^{-g[i]*",xvar,"}}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gi <- cfs[["gi"]]
    a2 <- cfs[["a2"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*e^{-'",a2,"'*e^{-'",gi,"'*",xvar,"}}")
  }
}

iMakeEqnGomp5 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*e^{-~frac(1,g[i])*~e^{-g[i]*~(",xvar,"~-~t[0])}}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gi <- cfs[["gi"]]
    t0 <- iHndlt0(cfs[["t0"]])
    # Put together and return
    paste0(yvar,"=='",Linf,"'*e^{-~frac(1,'",gi,"')*~e^{-'",gi,"'*(",xvar,t0$sgn,"'",t0$val,"')}}")
  }
}

iMakeEqnGomp6 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r]-L[m])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*~bgroup('[',frac(L[m],L[infinity]),']')^{~e^{-g[i]*",xvar,"}}-L[m]")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gi <- cfs[["gi"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*~bgroup('[',frac(L[m],'",Linf,"'),']')^{~e^{-'",gi,"'*",xvar,"}}-L[m]")
  }
}

iMakeEqnGomp7 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*~bgroup('[',frac(L[m],L[infinity]),']')^{~e^{-g[i]*",xvar,"}}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gi <- cfs[["gi"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*~bgroup('[',frac(L[m],'",Linf,"'),']')^{~e^{-'",gi,"'*",xvar,"}}")
  }
}

iMakeEqnLogi1 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==frac(L[infinity],1+e^{-g[-infinity]*(",xvar,"~-~t[i])})")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gninf <- cfs[["gninf"]]
    ti <- cfs[["ti"]]
    # Put together and return
    paste0(yvar,"==frac('",Linf,"',1+e^{-'",gninf,"'*(",xvar,"~-~'",ti,"')})")
  }
}

iMakeEqnLogi2 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==frac(L[infinity],1+~ae^{-g[-infinity]*",xvar,"})")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gninf <- cfs[["gninf"]]
    a <- cfs[["a"]]
    # Put together and return
    paste0(yvar,"==frac('",Linf,"',1+'",a,"'*e^{-'",gninf,"'*",xvar,"})")
  }
}

iMakeEqnLogi3 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==frac(L[0]*L[infinity],L[0]+(L[infinity]~-~L[0])*e^{-g[-infinity]*",xvar,"})")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,0))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gninf <- cfs[["gninf"]]
    L0 <- cfs[["L0"]]
    # Put together and return
    paste0(yvar,"==frac('",L0,"'%*%'",Linf,"','",L0,"'+('",Linf,"'~-~'",L0,"')*e^{-'",gninf,"'*",xvar,"})")
  }
}

iMakeEqnLogi4 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[r]-L[M])"
  if (is.null(xvar)) xvar <- "Delta*t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==frac(Delta*L[max],1+e^{log(19)*frac(L[m]~-~L[50],L[95]~-~L[50])})")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,0))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    gninf <- cfs[["gninf"]]
    L0 <- cfs[["L0"]]
    # Put together and return
    paste0(yvar,"==frac(Delta*L[max],1+e^{log(19)*frac(L[m]~-~L[50],L[95]~-~L[50])})")
  }
}

iMakeEqnRich1 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*~bgroup('[',1-frac(1,b)*~e^{-k*(",xvar,"~-~t[i])},']')^{~b}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    k <- cfs[["k"]]
    ti <- cfs[["ti"]]
    b <- cfs[["b"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*~bgroup('[',1-frac(1,'",b,"')*~e^{-'",k,"'*(",xvar,"~-~'",ti,"')},']')^{~'",b,"'}")
  }
}

iMakeEqnRich2 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*~bgroup('[',1+e^{-k*(",xvar,"~-~t[0])},']')^{~b}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    k <- cfs[["k"]]
    t0 <- iHndlt0(cfs[["t0"]])
    b <- cfs[["b"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*~bgroup('[',1+e^{-'",k,"'*(",xvar,"~",t0$sgn,"~'",t0$val,"')},']')^{~'",b,"'}")
  }
}

iMakeEqnRich3 <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*~bgroup('[',1+bgroup('(',bgroup('(',frac(L[0],L[infinity]),')')^{~frac(1,b)}-1,')')*~e^{-k*",xvar,"},']')^{~b}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,0,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    k <- cfs[["k"]]
    L0 <- cfs[["L0"]]
    b <- cfs[["b"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*~bgroup('[',1+bgroup('(',bgroup('(',frac('",L0,"','",Linf,"'),')')^{~frac(1,'",b,"')}-1,')')*~e^{-'",k,"'*",xvar,"},']')^{~'",b,"'}")
  }
}

iMakeEqnSchnute <- function(param,yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    switch(param,
      "Schnute1" = { paste0(yvar,"==bgroup('[',L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(1-e^{-a*(",
                            xvar,"~-~t[1])},1-e^{-a*(t[3]~-~t[1])}),']')^{~frac(1,b)}") },
      "Schnute2" = { paste0(yvar,"==L[1]*e^{log~bgroup('(',frac(L[3],L[1]),')')*~frac(1-e^{-a*(",
                            xvar,"~-~t[1])},1-e^{-a*(t[3]~-~t[1])})}") },
      "Schnute3" = { paste0(yvar,"==bgroup('[',L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(",
                            xvar,"~-~t[1],t[3]~-~t[1]),']')^{~frac(1,b)}") },
      "Schnute4" = { paste0(yvar,"==L[1]*e^{log~bgroup('(',frac(L[3],L[1]),')')*~frac(",
                           xvar,"~-~t[1],t[3]~-~t[1])}") } )
  } else { # Substitute in coefficient values
    # Get rounded coefficients (need to handle differently b/c of potentially missing a and b)
    # Get coefficients
    cfs <- stats::coef(fit)
    # If no a or b then append and set to 0 (as user would have done when fitting model)
    miss_a <- ifelse("a" %in% names(cfs),FALSE,TRUE)
    miss_b <- ifelse("b" %in% names(cfs),FALSE,TRUE)
    # Append on constvals
    cfs <- c(cfs,constvals)
    # If no digits declared by user then set default digits
    if (is.null(digits)) {
      digits <- c(0,0,ifelse(miss_a,NA,2),ifelse(miss_b,NA,2),0,0)
      digits <- digits[!is.na(digits)]
    }
    # Check that digits are correct length
    if (!length(digits) %in% c(1,length(cfs)))
      STOP("Length of 'digits' must equal 1 or number of function ",
           "parameters plus constant values (=",length(cfs),").")
    if (length(digits)==1) digits <- rep(digits,length(cfs))
    # Round coefficients to digits
    cfs <- purrr::map2_chr(cfs,digits,\(x,y) formatC(x,format="f",digits=y))
    # Isolate coefficients
    L1 <- cfs[["L1"]]
    L3 <- cfs[["L3"]]
    if (!miss_a) a <- cfs[["a"]]
    if (!miss_b) b <- cfs[["b"]]
    t1 <- cfs[["t1"]]
    t3 <- cfs[["t3"]]
    # Determine case to return based on values of a and b
    if (!miss_a & !miss_b) {
      paste0(yvar,"==bgroup('[','",L1,"'^{'",b,"'}+('",L3,"'^{'",b,"'}-'",L1,"'^{'",b,"'})*~frac(1-e^{-'",a,"'*(",
             xvar,"~-~'",t1,"')},1-e^{-'",a,"'*('",t3,"'~-~'",t1,"')}),']')^{~frac(1,'",b,"')}")
    } else if (!miss_a & miss_b) {
      paste0(yvar,"=='",L1,"'*e^{log~bgroup('(',frac('",L3,"','",L1,"'),')')*~frac(1-e^{-'",a,"'*(",
             xvar,"~-~'",t1,"')},1-e^{-'",a,"'*('",t3,"'~-~'",t1,"')})}")
    } else if (miss_a & !miss_b) {
      paste0(yvar,"==bgroup('[','",L1,"'^{'",b,"'}+('",L3,"'^{'",b,"'}-'",L1,"'^{'",b,"'})*~frac(",
             xvar,"~-~'",t1,"','",t3,"'~-~'",t1,"'),']')^{~frac(1,'",b,"')}")
    } else 
      paste0(yvar,"=='",L1,"'*e^{log~bgroup('(',frac('",L3,"','",L1,"'),')')*~frac(",
             xvar,"~-~'",t1,"','",t3,"'~-~'",t1,"')}")
  }
}

iMakeEqnSchnRich <- function(yvar,xvar,fit,constvals,digits) {
  if (is.null(yvar)) yvar <- "E(L[t])"
  if (is.null(xvar)) xvar <- "t"
  if (is.null(fit)) { # Make generic equation and return
    paste0(yvar,"==L[infinity]*~bgroup('(',1-a*e^{-k",xvar,"^{c}},')')^{frac(1,b)}")
  } else { # Substitute in coefficient values
    # Get rounded coefficients
    cfs <- iRoundCoefs(fit,constvals,digits,def_digits=c(0,3,2,2,2))
    # Isolate coefficients
    Linf <- cfs[["Linf"]]
    k <- cfs[["k"]]
    a <- cfs[["a"]]
    b <- cfs[["b"]]
    c <- cfs[["c"]]
    # Put together and return
    paste0(yvar,"=='",Linf,"'*~bgroup('(',1-'",a,"'*e^{-'",k,"'",xvar,"^{'",c,"'}},')')^{frac(1,'",b,"')}")
  }
}