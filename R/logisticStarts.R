#' @title Find reasonable starting values for a logistic growth function.
#' 
#' @description Finds reasonable starting values for the parameters in a specific parameterization of the logistic growth function.
#' 
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of the logistic growth function. There is no guarantee that these starting values are the \sQuote{best} starting values. One should use them with caution and should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#' 
#' BLAH-BLAH-BLAH
#' 
#' @inheritParams GompertzStarts
#'
#' @return A list that contains reasonable starting values. Note that the parameters will be listed in the same order and with the same names as listed in \code{\link{logisticFuns}}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @section IFAR Chapter: 12-Individual Growth.
#' 
#' @seealso See \code{\link{growthFunShow}} to display the equations for the parameterizations used in \pkg{FSA} and \code{\link{logisticFuns}} for functions that represent the logistic function parameterizations. See \code{\link{nlsTracePlot}} for help troubleshooting nonlinear models that don't converge.
#' 
#' @keywords manip
#'
#' @examples
#' # Make some fake data using the default parameterization
#' logfun <- logisticFuns()
#' # setup ages, sample sizes (general reduction in numbers with
#' # increasing age), and additive SD to model
#' t <- 0:15
#' n <- c(5,10,40,35,25,12,10,10,8,6,5,3,3,3,2,2)
#' sd <- 15
#' # expand ages
#' ages <- rep(t,n)
#' # get lengths from gompertz and a random error for individuals
#' lens <- logfun(ages,Linf=450,gninf=0.3,ti=2)+rnorm(length(ages),0,sd)
#' # put together as a data.frame
#' df <- data.frame(age=ages,tl=round(lens,0))
#' 
#' logisticStarts(tl~age,data=df)
#' logisticStarts(tl~age,data=df,plot=TRUE)
#' 
#' logisticStarts(tl~age,data=df,param="CampanaJones2",plot=TRUE)
#' logisticStarts(tl~age,data=df,param="Karkach",plot=TRUE)
#' 
#' @export logisticStarts

logisticStarts <- function(formula,data=NULL,
                           param=c("CJ1","CJ2","Karkach",
                                   "CampanaJones1","CampanaJones2"),type=param,
                           fixed=NULL,
                           plot=FALSE,col.mdl="gray70",lwd.mdl=3,lty.mdl=1,
                           cex.main=0.9,col.main="red",...) {
  ## some checks of arguments
  type <- match.arg(type,c("CJ1","CJ2","Karkach","CampanaJones1","CampanaJones2"))
  if (!is.null(fixed)) {
    if(!is.list(fixed)) STOP("'fixed' must be a list.")
    if (any(names(fixed)=="")) STOP("Items in 'fixed' must be named.")
  }
  
  ## handle the formula with some checks
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) STOP("'logisticStarts' must have only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer")) 
    STOP("LHS variable must be numeric.")
  if (!tmp$metExpNumE) STOP("'logisticStarts' must have only one RHS variable.")
  if (!tmp$Eclass %in% c("numeric","integer")) 
    STOP("RHS variable must be numeric.")
  
  ## Get initial values from SSgompertz
  ssform <- stats::as.formula(paste0(tmp$Rname[1],"~stats::SSlogis(",
                                     tmp$Enames[1],",Asym,xmid,scal)"))
  sstmp <- stats::getInitial(ssform,data=data)
  Linf <- sstmp[["Asym"]]
  xmid <- sstmp[["xmid"]]
  scal <- sstmp[["scal"]]
  gninf <- 1/scal
  
  ## get starting values depending on type
  switch(type,
         CampanaJones1=,CJ1={  sv <- list(Linf=Linf,gninf=gninf,ti=xmid)  },
         CampanaJones2=,CJ2={  sv <- list(Linf=Linf,gninf=gninf,a=exp(xmid/scal))  },
         Karkach={  sv <- list(Linf=Linf,gninf=gninf,L0=Linf/(1+exp(xmid/scal)))  },
  ) # end 'type' switch

  ## make the static plot if asked for
  if (plot) iPlotGrowStarts(formula,data,"Logistic",type,sv,NULL,
                            col.mdl,lwd.mdl,lty.mdl,cex.main,col.main)
  ## return starting values list
  sv
}  
