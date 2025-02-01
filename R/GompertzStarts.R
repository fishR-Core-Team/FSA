#' @title Find reasonable starting values for a Gompertz growth function.
#' 
#' @description Finds reasonable starting values for the parameters in a specific parameterization of the Gompertz growth function.
#' 
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of the Gompertz growth function. There is no guarantee that these starting values are the \sQuote{best} starting values. One should use them with caution and should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#' 
#' BLAH-BLAH-BLAH
#' 
#' 
#'
#' @param formula A formula of the form \code{len~age}.
#' @param data A data frame that contains the variables in \code{formula}.
#' @param type,param A string that indicates the parameterization of the von Bertalanffy model.
#' @param fixed A named list that contains user-defined rather than automatically generated (i.e., fixed) starting values for one or more parameters. See details.
#' @param plot A logical that indicates whether a plot of the data with the superimposed model fit at the starting values should be created.
#' @param col.mdl A color for the model when \code{plot=TRUE}.
#' @param lwd.mdl A line width for the model when \code{plot=TRUE}.
#' @param lty.mdl A line type for the model when \code{plot=TRUE}.
#' @param cex.main A character expansion value for the main title when \code{plot=TRUE}.
#' @param col.main A color for the main title when \code{plot=TRUE}.
#' @param \dots Further arguments passed to the methods.
#'
#' @return A list that contains reasonable starting values. Note that the parameters will be listed in the same order and with the same names as listed in \code{\link{GompertzFuns}}.
#' 
#' @note The \sQuote{Ricker2} and \sQuote{QuinnDeriso1} are synonymous, as are \sQuote{Ricker3} and \sQuote{QuinnDeriso2}.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @section IFAR Chapter: 12-Individual Growth.
#' 
#' @seealso See \code{\link{growthFunShow}} to display the equations for the parameterizations used in \pkg{FSA} and \code{\link{GompertzFuns}} for functions that represent the von Bertalanffy parameterizations. See \code{\link{nlsTracePlot}} for help troubleshooting nonlinear models that don't converge.
#' 
#' @keywords manip
#'
#' @examples
#' # Make some fake data using the original parameterization
#' gompO <- GompertzFuns("original")
#' # setup ages, sample sizes (general reduction in numbers with
#' # increasing age), and additive SD to model
#' t <- 0:15
#' n <- c(5,10,40,35,25,12,10,10,8,6,5,3,3,3,2,2)
#' sd <- 15
#' # expand ages
#' ages <- rep(t,n)
#' # get lengths from gompertz and a random error for individuals
#' lens <- gompO(ages,Linf=450,a=1,gi=0.3)+rnorm(length(ages),0,sd)
#' # put together as a data.frame
#' df <- data.frame(age=ages,tl=round(lens,0))
#' 
#' GompertzStarts(tl~age,data=df)
#' GompertzStarts(tl~age,data=df,plot=TRUE)
#' 
#' GompertzStarts(tl~age,data=df,param="Ricker2",plot=TRUE)
#' GompertzStarts(tl~age,data=df,param="Ricker3",plot=TRUE)
#' GompertzStarts(tl~age,data=df,param="QuinnDeriso3",plot=TRUE)
#' GompertzStarts(tl~age,data=df,param="Original",plot=TRUE)
#' 
#' @export GompertzStarts

GompertzStarts <- function(formula,data=NULL,
                           param=c("Ricker1","Ricker2","Ricker3",
                                   "QuinnDeriso1","QuinnDeriso2","QuinnDeriso3",
                                   "QD1","QD2","QD3",
                                   "Original","original"),type=param,
                           fixed=NULL,
                           plot=FALSE,col.mdl="gray70",lwd.mdl=3,lty.mdl=1,
                           cex.main=0.9,col.main="red",...) {
  ## some checks of arguments
  type <- match.arg(type,c("Ricker1","Ricker2","Ricker3",
                           "QuinnDeriso1","QuinnDeriso2","QuinnDeriso3",
                           "QD1","QD2","QD3",
                           "Original","original"))
  if (!is.null(fixed)) {
    if(!is.list(fixed)) STOP("'fixed' must be a list.")
    if (any(names(fixed)=="")) STOP("Items in 'fixed' must be named.")
  }
  
  ## handle the formula with some checks
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) STOP("'GompertzStarts' must have only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer")) 
    STOP("LHS variable must be numeric.")
  if (!tmp$metExpNumE) STOP("'GompertzStarts' must have only one RHS variable.")
  if (!tmp$Eclass %in% c("numeric","integer")) 
    STOP("RHS variable must be numeric.")
  
  ## Get initial values from SSgompertz
  ssform <- stats::as.formula(paste0(tmp$Rname[1],"~stats::SSgompertz(",
                                     tmp$Enames[1],",Asym,b2,b3)"))
  sstmp <- stats::getInitial(ssform,data=data)
  Linf <- sstmp[["Asym"]]
  gi <- -log(sstmp[["b3"]])
  b2 <- sstmp[["b2"]]
  
  ## get starting values depending on type
  switch(type,
         Ricker1={  sv <- list(Linf=Linf,gi=gi,ti=log(b2)/gi)  },
         Ricker2=,QuinnDeriso1=,QD1={  sv <- list(L0=Linf/exp(b2),b=b2,gi=gi)  },
         Ricker3=,QuinnDeriso2=,QD2={  sv <- list(Linf=Linf,c=b2,gi=gi)  },
         QuinnDeriso3=,QD3={  sv <- list(Linf=Linf,gi=gi,t0=log(b2*gi)/gi)  },
         Original=,original={  sv <- list(Linf=Linf,a=log(b2),gi=gi)  },
  ) # end 'type' switch

  ## make the static plot if asked for
  if (plot) iPlotGrowStarts(formula,data,"Gompertz",type,sv,NULL,
                            col.mdl,lwd.mdl,lty.mdl,cex.main,col.main)
  ## return starting values list
  sv
}  
