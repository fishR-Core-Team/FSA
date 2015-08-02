#' @title Finds reasonable starting values for parameters in specific paramaterizations of common stock-recruitment models.
#'
#' @description Finds reasonable starting values for parameters in specific parameterizations of the \dQuote{Beverton-Holt}, \dQuote{Ricker},  \dQuote{Shepherd}, or \dQuote{Saila-Lorda} stock-recruitment models.  Type \code{srModels()} for the equations of each model.
#'
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of the \dQuote{Beverton-Holt}, \dQuote{Ricker},  \dQuote{Shepherd}, or \dQuote{Saila-Lorda} stock-recruitment models.  There is no guarantee that these starting values are the \sQuote{best} starting values.  One should use them with caution and should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#'
#' Starting values for the first parameterization of the Beverton-Holt model were derived by linearizing the function (inverting both sides and simplifying), fitting a linear model to the observed data, and extracting parameter values from the corresponding linear model parameters.  Starting values for the other parameterizations of the Beverton-Holt model were derived from known relationships between the parameters of each parameterization and the first parameterization.  If the computed starting value for the \code{Rp} parameter was larger than the largest observed recruitment value, then the starting value for \code{Rp} was set to the largest observed recruitment value.
#' 
#' Starting values for the Shepherd function were the same as those for the first parameterization of the Beverton-Holt function with the addition that \code{c=1}.
#'
#' Starting values for the Ricker parameterizations followed the same general procedure as described for the Beverton-Holt parameterizations.  If the computed starting value for \code{atilde} was less than zero then the starting value was set to 0.00001.
#' 
#' Starting values for the Saila-Lorda function were the same as those for the first parameterization of the Ricker function with the addition that \code{c=1}.
#' 
#' Starting values can also be obtained by plotting the data with the model superimposed but tied to slider bars for changing parameters.  One can change the parameters until a reasonable fit is observed and then use those valeus as starting values.  Such a plot can be constructed by including \code{dynamicPlot=TRUE}.  The initial parameters for the slider bars are the starting values constructed as described above.  The range for the sliders will have a minimum that is \code{min,prop} times the initial value and a maximum that is \code{max.mult} times the initial value.  The step or interval of the slider bar is \code{delta.mult} times the initial value.  It should be noted that the dynamic plot may show an error of \dQuote{[tcl] can't get device image} but the plot will correctly update if the slider bar is adjusted.
#'
#' @param formula A formula of the form \code{Recruits~Stock}.
#' @param data A data frame in which \code{Recruits} and \code{Stock} are found.
#' @param type A string that indicates the type of the stock-recruitment model.  Must be one of \code{"BevertonHolt"}, \code{"Ricker"}, \code{"Shepherd"}, or \code{"SailaLorda"}.
#' @param param A numeric that indicates the parameterization of the stock-recruitment model type.  This is ignored if \code{type="Shepherd"} or \code{type="SailaLorda"}
#' @param plot A logical that indicates whether or not a plot of the data with the model fit at the starting values superimposed is created.
#' @param col.mdl A color for the model when \code{plot=TRUE}.
#' @param lwd.mdl A line width for the model when \code{plot=TRUE}.
#' @param lty.mdl A line type for the model when \code{plot=TRUE}.
#' @param dynamicPlot A logical that indicates where a plot with dynamically linked slider bars should be constructed for finding starting values.
#' @param min.prop A single numeric that is used to set the minimum values for the slider bars in the dynamic plot.  see details.
#' @param max.mult A single numeric that is used to set the maximum values for the slider bars in the dynamic plot.  see details.
#' @param delta.prop A single numeric that is used to set the step value for the slider bars in the dynamic plots.  See details.
#' @param \dots Further arguments passed to the methods.
#'
#' @return A list that contains reasonable starting values.  Note that the parameters will be listed in the same order and with the same names as listed in \code{\link{srFuns}}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{13-Recruitment}.
#'
#' @seealso See \code{\link{srModels}} and \code{\link{srFuns}} for related functionality.
#'
#' @references Ogle, D.H.  2015.  Introductory Fisheries Analyses with R.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Beverton, R.J.H. and S.J. Holt.  1957.  On the dynamics of exploited fish populations, Fisheries Investigations (Series 2), volume 19.  United Kingdom Ministry of Agriculture and Fisheries, 533 pp.
#' 
#' Iles, T.C.  1994.  A review of stock-recruitment relationships with reference to flatfish populations.  Netherlands Journal of Sea Research 32:399-420.
#'
#' Quinn II, T.J. and R.B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press.
#'
#' Ricker, W.E. 1954. Stock and recruitment. Journal of the Fisheries Research Board of Canada 11:559-623.
#'
#' Ricker, W.E. 1975. \href{http://www.dfo-mpo.gc.ca/Library/1485.pdf}{Computation and interpretation of biological statistics of fish populations}. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.
#' 
#' Shepherd, J. 1982. A versatile new stock-recruitment relationship for fisheries and construction of sustainable yield curves. Journal du Conseil International pour l'Exploration de la Mar 40:67-75.
#'
#' @keywords manip
#'
#' @examples
#' ## Simple Examples
#' data(CodNorwegian)
#' srStarts(recruits~stock,data=CodNorwegian)
#' srStarts(recruits~stock,data=CodNorwegian,param=2)
#' srStarts(recruits~stock,data=CodNorwegian,param=3)
#' srStarts(recruits~stock,data=CodNorwegian,param=4)
#' srStarts(recruits~stock,data=CodNorwegian,type="Ricker")
#' srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=2)
#' srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=3)
#' srStarts(recruits~stock,data=CodNorwegian,type="Shepherd")
#' srStarts(recruits~stock,data=CodNorwegian,type="SailaLorda")
#' srStarts(recruits~stock,data=CodNorwegian,type="independence")
#'
#' ## Simple Examples with a Plot
#' srStarts(recruits~stock,data=CodNorwegian,type="Ricker",plot=TRUE)
#' srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",plot=TRUE)
#' srStarts(recruits~stock,data=CodNorwegian,type="Shepherd",plot=TRUE)
#' srStarts(recruits~stock,data=CodNorwegian,type="SailaLorda",plot=TRUE)
#' srStarts(recruits~stock,data=CodNorwegian,type="independence",plot=TRUE)
#' 
#' ## Dynamic Plots Method -- ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#'   # Beverton-Holt
#'   srStarts(recruits~stock,data=CodNorwegian,dynamicPlot=TRUE)
#'   srStarts(recruits~stock,data=CodNorwegian,param=2,dynamicPlot=TRUE)
#'   srStarts(recruits~stock,data=CodNorwegian,param=3,dynamicPlot=TRUE)
#'   srStarts(recruits~stock,data=CodNorwegian,param=4,dynamicPlot=TRUE)
#'   # Ricker Models
#'   srStarts(recruits~stock,data=CodNorwegian,type="Ricker",dynamicPlot=TRUE)
#'   srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=2,dynamicPlot=TRUE)
#'   srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=3,dynamicPlot=TRUE)
#'   # Shepherd, Saila-Lorda, and Independence Models
#'   srStarts(recruits~stock,data=CodNorwegian,type="Shepherd",dynamicPlot=TRUE)
#'   srStarts(recruits~stock,data=CodNorwegian,type="SailaLorda",dynamicPlot=TRUE)
#'   srStarts(recruits~stock,data=CodNorwegian,type="independence",dynamicPlot=TRUE)
#' } ## END .. ONLY INTERACTIVE
#' 
#' ## See examples in srFuns() for use of srStarts() when fitting stock-recruit models
#'
#' @rdname srStarts
#' @export
srStarts <- function(formula,data=NULL,type=c("BevertonHolt","Ricker","Shepherd","SailaLorda","independence"),
                     param=1,plot=FALSE,col.mdl="gray70",lwd.mdl=3,lty.mdl=1,
                     dynamicPlot=FALSE,min.prop=0.1,max.mult=3,delta.prop=0.005,...) {
  ## some checks
  type <- match.arg(type)
  if (length(param)!=1) stop("Only one 'param' is allowed.",call.=FALSE)
  ## some checks on the formula
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (tmp$vnum!=2) {
    if (tmp$vnum==1) stop("'srstarts' must have a formula with both LHS and RHS.",call.=FALSE)
    if (tmp$vnum>2) stop("'srstarts' must have only one LHS and only one RHS variable.",call.=FALSE)
  }
  if (!tmp$metExpNumR) stop("'srStarts' must have one and only one LHS variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'srStarts' must have only one RHS variable.",call.=FALSE)
  if (!tmp$Eclass %in% c("numeric","integer")) stop("RHS variable must be numeric.",call.=FALSE)
  ## get the R and S vectors
  R <- tmp$mf[,tmp$Rname[1]]
  S <- tmp$mf[,tmp$Enames[1]]
  ## find starting values for the type and param
  switch(type,
    BevertonHolt=  { sv <- iSRStartsBH(S,R,param) },
    Ricker=        { sv <- iSRStartsR(S,R,param) },
    Shepherd =     { sv <- iSRStartsS(S,R) },
    SailaLorda =   { sv <- iSRStartsSL(S,R) },
    independence = { sv <- iSRStartsI(S,R) }
  ) # end type switch
  ## Check if user wants to choose starting values from an interactive plot
  if (dynamicPlot) {
    if (!requireNamespace("relax")) stop("'srStars' requires the 'relax' package to be installed to construct the dynamic plot.",call.=FALSE)
    else iSRStartsDynPlot(S,R,type,param,sv,min.prop,max.mult,delta.prop)
  } else {
    # make the static plot if asked for
    if (plot) iSRStartsPlot(S,R,type,param,sv,col.mdl,lwd.mdl,lty.mdl)
    # return the list of starting values
    return(sv)
  }
}

##############################################################
# INTERNAL FUNCTIONS
##############################################################
#=============================================================
# Find starting values for Beverton-Holt models
#=============================================================
iSRStartsBH <- function(S,R,param) {
  ## some checks
  if (!param %in% 1:4) stop("'param' must be in 1:4 when type='BevertonHolt'.",call.=FALSE)
  ## Linearized B-H #1 model without lognormal errors for starting values
  cfs <- stats::coef(stats::lm(I(1/R)~I(1/S)))
  ## Extract parameter values
  a <- 1/cfs[[2]]
  b <- cfs[[1]]/cfs[[2]]
  Rp <- a/b
  # don't let Rp be more than max of R
  if (Rp>max(R)) Rp <- max(R,na.rm=TRUE)
  atilde <- 1/a
  btilde <- b/a
  ## Return a list of starting values
  if (param==1) sv <- list(a=a,b=b)
  else if (param==2) sv <- list(a=a,Rp=Rp)
  else if (param==3) sv <- list(a=atilde,b=btilde)
  else sv <- list(a=atilde,Rp=Rp)
  sv
} # end internal iSRStartsBH

#=============================================================
# Find starting values for Ricker models
#=============================================================
iSRStartsR <- function(S,R,param) {
  ## some checks
  if (!param %in% 1:3) stop("'param' must be in 1:3 when type='Ricker'.",call.=FALSE)
  ## Linearized Ricker #1 model with lognormal errors for starting values
  cfs <- stats::coef(stats::lm(log(R/S)~S))
  ## Extract parameter values
  a <- exp(cfs[[1]])
  b <- -cfs[[2]]
  atilde <- log(a)
  # don't allow negative atilde
  if (atilde<0) atilde <- 0.00001
  Rp <- a/(b*exp(1))
  ## Return a list of starting values
  if (param==1) sv <- list(a=a,b=b)
  else if (param==2) sv <- list(a=atilde,b=b)
  else sv <- list(a=a,Rp=Rp)
  sv
} # end internal iSRStartsR

#=============================================================
# Find starting values for Shepherd model
#=============================================================
iSRStartsS <- function(S,R) c(iSRStartsBH(S,R,param=1),c=1)

#=============================================================
# Find starting values for Saila-Lorda models
#=============================================================
iSRStartsSL <- function(S,R) c(iSRStartsR(S,R,param=1),c=1)

#=============================================================
# Find starting values for independence model
#=============================================================
iSRStartsI <- function(S,R) c(a=stats::coef(stats::lm(R~0+S))[[1]])


#=============================================================
# Static plot of starting values
#=============================================================
iSRStartsPlot <- function(S,R,type,param,sv,col.mdl,lwd.mdl,lty.mdl) {
  ## attempting to get by bindings warning in RCMD CHECK
  x <- NULL
  ## Make a title
  ttl1 <- ifelse(type %in% c("BevertonHolt","Ricker"),paste0(type," #",param),type)
  ttl2 <- paste(paste(names(sv),formatC(unlist(sv),format="f",digits=5),sep="="),collapse=", ")
  ttl <- paste(ttl1,"at",ttl2)
  ## Plot the data
  max.S <- max(S,na.rm=TRUE)
  graphics::plot(S,R,pch=19,xlim=c(0,max.S),xlab="Stock Level",ylab="Recruitment Level",main=ttl)
  ## Plot the model
  mdl <- srFuns(type,param)
  graphics::curve(mdl(x,unlist(sv)),from=0,to=max.S,col=col.mdl,lwd=lwd.mdl,lty=lty.mdl,add=TRUE)
}


#=============================================================
# Dynamics plots for finding starting values -- main function
#=============================================================
iSRStartsDynPlot <- function(S,R,type,param,sv,min.prop,max.mult,delta.prop) {
  ## internal refresh function for the dialog box
  refresh <- function(...) {
    p1 <- relax::slider(no=1)
    if (type!="independence") p2 <- relax::slider(no=2)
      else p2=NULL
    if (type %in% c("Shepherd","SailaLorda")) p3 <- relax::slider(no=3)
      else p3=NULL
    iSRDynPlot(S,R,type,param,p1,p2,p3)
  } # end internal refresh
  
  ## Main function
  ## Set the minimum value as a proportion (from min.prop)
  ## of the starting values, the maximum value at a multiple
  ## (from max.mult) of the starting values, and the delta
  ## value at a proportion (from delta.prop) of the starting
  ## values.  Unlist first to make as a vector.
  sl.defaults <- unlist(sv)
  sl.mins <- sl.defaults*min.prop
  sl.maxs <- sl.defaults*max.mult
  sl.deltas <- sl.defaults*delta.prop
  ## Grab names from the sv vector
  sl.names <- names(sl.defaults)
  ## Make a title
  sl.ttl <- paste(type,"Stock-Recruit Model")
  if (type %in% c("BevertonHolt","Ricker")) sl.ttl <- paste0(sl.ttl," #",param)
  ## Set up names that are specific to type and param
  relax::gslider(refresh,prompt=TRUE,hscale=2,pos.of.panel="left",
                 title=sl.ttl,sl.names=sl.names,
                 sl.mins=sl.mins,sl.maxs=sl.maxs,
                 sl.deltas=sl.deltas,sl.defaults=sl.defaults)
}

#=============================================================
# Constructs the actual plot in the dynamics plots for finding
# starting values
#=============================================================
iSRDynPlot <- function(S,R,type,param,p1,p2=NULL,p3=NULL) {
  ## create a sequence of spawning stock values
  max.S <- max(S,na.rm=TRUE)
  x <- c(seq(0,0.2*max.S,length.out=500),seq(0.2*max.S,max.S,length.out=500)) 
  ## create a sequence of matching recruit values according to
  ## the model type and param
  y <- iSRDynPlot_makeR(x,type,param,p1,p2,p3)
  ## Construct the scatterplot with superimposed model
  old.par <- graphics::par(mfrow=c(1,2), mar=c(3.5,3.5,1.25,1.25), mgp=c(2,0.4,0), tcl=-0.2, pch=19)
  graphics::plot(S,R,xlab="Parental (Spawner) Stock",ylab="Recruits",
       ylim=c(0,max(R,na.rm=TRUE)),xlim=c(0,max(S,na.rm=TRUE)))
  graphics::lines(x,y,lwd=2,lty=1,col="blue")
  graphics::plot(S,R/S,xlab="Parental (Spawner) Stock",ylab="Recruits/Spawner",
       ylim=c(0,max(R/S,na.rm=TRUE)),xlim=c(0,max(S,na.rm=TRUE)))
  graphics::lines(x,y/x,lwd=2,lty=1,col="blue")
  graphics::par(old.par)
}

#=============================================================
# Construct values for R given values of S, a stock-recruit
# model and parameterization, and values of the parameters.
# This is called by iSRDynPlot()
#=============================================================
iSRDynPlot_makeR <- function(x,type,param,p1,p2,p3=NULL){
  if (type=="BevertonHolt") {
    if (param==1) {
      # p1=a,p2=b
      y <- p1*x/(1+p2*x)
    } else if (param==2) {
      # p1=a, p2=Rp
      y <- p1*x/(1+p1*(x/p2))
    } else if (param==3) {
      # p1=atilde, p2=btilde
      y <- x/(p1+p2*x)
    } else if (param==4) {
      # p1=atilde, p2=Rp
      y <- x/(p1+x/p2)
    } # end B-H switch
  } else if (type=="Ricker") {
    if (param==1) {
      # p1=a,p2=b
      y <- p1*x*exp(-p2*x)
    } else if (param==2) {
      # p1=atilde, p2=b
      y <- x*exp(p1-p2*x)
    } else if (param==3) {
      # p1=a, p2=Rp
      y <- p1*x*exp(-p1*x/(p2*exp(1)))
    } # end Ricker switch
  } else if (type=="Shepherd") {
      # p1=a, p2=b, p3=c
      y <- (p1*x)/(1+(p2*x)^p3)
  } else if (type=="SailaLorda") {
      # p1=a, p2=b, p3=c    
      y <- p1*(x^p3)*exp(-p2*x) 
  } else {  # Independence model
      # p1=a
      y <- p1*x
  }
  # return the value of y
  y
}