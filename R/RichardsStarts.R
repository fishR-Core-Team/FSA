#' @title Find reasonable starting values for a Richards growth function.
#' 
#' @description Finds reasonable starting values for the parameters in a specific parameterization of the Richards growth function.
#' 
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of the Richards growth function. There is no guarantee that these starting values are the \sQuote{best} starting values. One should use them with caution and should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#' 
#' BLAH-BLAH-BLAH
#' 
#' @inheritParams GompertzStarts
#'
#' @return A list that contains reasonable starting values. Note that the parameters will be listed in the same order and with the same names as listed in \code{\link{RichardsFuns}}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @section IFAR Chapter: 12-Individual Growth.
#' 
#' @seealso See \code{\link{growthFunShow}} to display the equations for the parameterizations used in \pkg{FSA} and \code{\link{RichardsFuns}} for functions that represent the logistic function parameterizations. See \code{\link{nlsTracePlot}} for help troubleshooting nonlinear models that don't converge.
#' 
#' @keywords manip
#'
#' @examples
#' # Make some fake data using the default parameterization
#' rfun <- RichardsFuns()
#' # setup ages, sample sizes (general reduction in numbers with
#' # increasing age), and additive SD to model
#' t <- 0:15
#' n <- c(5,10,40,35,25,12,10,10,8,6,5,3,3,3,2,2)
#' sd <- 15
#' # expand ages
#' ages <- rep(t,n)
#' # get lengths from gompertz and a random error for individuals
#' lens <- rfun(ages,Linf=450,ti=2,k=0.5,b1=1.5)+rnorm(length(ages),0,sd)
#' # put together as a data.frame
#' df <- data.frame(age=ages,tl=round(lens,0))
#' 
#' RichardsStarts(tl~age,data=df)
#' RichardsStarts(tl~age,data=df,plot=TRUE)
#' 
#' RichardsStarts(tl~age,data=df,param=1,plot=TRUE)
#' RichardsStarts(tl~age,data=df,param=2,plot=TRUE)
#' RichardsStarts(tl~age,data=df,param=3,plot=TRUE)
#' RichardsStarts(tl~age,data=df,param=4,plot=TRUE)
#' RichardsStarts(tl~age,data=df,param=5,plot=TRUE)
#' 
#' @export RichardsStarts

RichardsStarts <- function(formula,data=NULL,
                           param=1,type=param,
                           fixed=NULL,
                           plot=FALSE,col.mdl="gray70",lwd.mdl=3,lty.mdl=1,
                           cex.main=0.9,col.main="red",...) {
  ## some checks of arguments
  if (!type %in% 1:5) STOP("'param'/'type' must be in 1:5.")
  type <- paste0("Richards",type)
  if (!is.null(fixed)) {
    if(!is.list(fixed)) STOP("'fixed' must be a list.")
    if (any(names(fixed)=="")) STOP("Items in 'fixed' must be named.")
  }
  
  ## handle the formula with some checks
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) STOP("'RichardsStarts' must have only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer")) 
    STOP("LHS variable must be numeric.")
  if (!tmp$metExpNumE) STOP("'RichardsStarts' must have only one RHS variable.")
  if (!tmp$Eclass %in% c("numeric","integer")) 
    STOP("RHS variable must be numeric.")
  
  ## Get initial values from SSposnegRichards
  sstmp <- FlexParamCurve::modpar(data[[tmp$Enames[1]]],data[[tmp$Rname[1]]],
                                  pn.options="sstmp",width.bounds=2,force4par=TRUE,
                                  verbose=FALSE,suppress.text=TRUE)
  Linf <- sstmp[["Asym"]]
  k <- sstmp[["K"]]
  ti <- sstmp[["Infl"]]
  M <- sstmp[["M"]]
  t0 <- -(log(1/M)/k)+ti
  L0 <- Linf/((1+M*exp(k*ti)))^(1/M)

  ## get starting values depending on type
  switch(type,
         Richards1={  sv <- list(Linf=Linf,k=k,ti=ti,b1=M)  },
         Richards2={  sv <- list(Linf=Linf,k=k,t0=t0,b2=-(1/M))  },
         Richards3={  sv <- list(Linf=Linf,k=k,L0=L0,b3=1+M)  },
         Richards4={  sv <- list(Linf=Linf,k=k,ti=ti,b2=-(1/M))  },
         Richards5={  sv <- list(Linf=Linf,k=k,ti=ti,b3=1+M)  }
  ) # end 'type' switch

  ## make the static plot if asked for
  if (plot) iPlotGrowStarts(formula,data,"Richards",param,sv,NULL,
                            col.mdl,lwd.mdl,lty.mdl,cex.main,col.main)
  ## return starting values list
  sv
}  
