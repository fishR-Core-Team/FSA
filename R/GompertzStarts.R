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
                                   "Original","original"),
                           type=param,
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
  
  ## get the length and age vectors
  len <- tmp$mf[,tmp$Rname[1]]
  age <- tmp$mf[,tmp$Enames[1]]
  
  ## Find mean lengths at age for the polynomial model
  meanL <- tapply(len,age,mean)
  ages <- as.numeric(names(meanL))

  ## fit 3rd degree polynomial, create data.frame of modeled values
  respoly <- stats::lm(meanL~stats::poly(ages,3,raw=TRUE))
  mdldf <- data.frame(age=seq(min(ages),max(ages),length.out=299))
  mdldf$len <- stats::predict(respoly,data.frame(ages=mdldf$age))
  tmp <- stats::coef(respoly)
  mdldf$deriv <- tmp[2]+tmp[3]*mdldf$age+tmp[4]*mdldf$age^2
  
  ## get the starting values
  Linf <- ifelse ("Linf" %in% names(fixed),fixed[["Linf"]],max(mdldf$len))
  L0 <- ifelse ("L0" %in% names(fixed),fixed[["L0"]],stats::predict(respoly,data.frame(ages=0)))
  Linfpt <- Linf/exp(1)
  infpt.pos <- which(mdldf$len>Linfpt)[[1]]
  ti <- ifelse ("ti" %in% names(fixed),fixed[["ti"]],mdldf$age[infpt.pos])
  infpt.pos <- c(infpt.pos,infpt.pos+1)
  gi <- ifelse ("gi" %in% names(fixed),fixed[["gi"]],
                as.numeric(diff(log(mdldf$len[infpt.pos]))/diff(mdldf$age[infpt.pos])))
  if ("t0" %in% names(fixed)) t0 <- fixed[["t0"]]
  else {
    # get real component of roots to polynomial equation
    resroots <- Re(polyroot(stats::coef(respoly)))
    # find starting value for t0 as polynomial root closest to zero
    t0 <- resroots[which(abs(resroots)==min(abs(resroots)))]
    # removes the attributes and will return only the first
    #  root if a "double root" was found
    t0 <- t0[[1]]
  }
  # starting value for a depends on param/type ... so handled differently
  if (!type %in% c("Ricker1","QuinnDeriso3","QD3")) {
    if ("a" %in% names(fixed)) a <- fixed[["a"]]
    else {
      switch(type,
             Ricker2=,QuinnDeriso1=,QD1={ a <- exp(gi*t0)/gi },
             Ricker3=,QuinnDeriso2=,QD2={ a <- exp(gi*ti) },
             Original=,original={ a <- gi*ti }
             ) # end switch
    }
  }

  ## return values appropriate to the param chosen
  switch(type,
         Ricker1={ sv <- list(Linf=Linf,gi=gi,ti=ti)},
         Ricker2=,QuinnDeriso1=,QD1={ sv <- list(L0=L0,a=a,gi=gi)},
         Ricker3=,QuinnDeriso2=,QD2=,Original=,original={ sv <- list(Linf=Linf,a=a,gi=gi)},
         QuinnDeriso3=,QD3={ sv <- list(Linf=Linf,gi=gi,t0=t0)}
  ) # end 'type' switch
  
  ## make the static plot if asked for
  if (plot) iGompStartsPlot(age,len,type,sv,
                            col.mdl,lwd.mdl,lty.mdl,cex.main,col.main)
  ## return starting values list
  sv
}


#===============================================================================
# Static plot of starting values
#===============================================================================
iGompStartsPlot <- function(age,len,type,sv,
                            col.mdl,lwd.mdl,lty.mdl,cex.main,col.main) { # nocov start
  ## attempting to get by bindings warning in RCMD CHECK
  x <- NULL
  ## Plot the data
  # create a transparency value that attempts to not be too transparent
  tmp <- max(table(age,len))
  clr <- grDevices::rgb(0,0,0,ifelse(tmp>2 & tmp<20,2/tmp,0.1))
  # Make the base plot
  graphics::plot(age,len,pch=19,col=clr,xlab="Age",ylab="Length",
                 main=paste0("Gompertz (",type,") STARTING VALUES"),
                 cex.main=cex.main,col.main=col.main)
  ## Plot the model
  mdl <- GompertzFuns(type)
  min.age <- min(age,na.rm=TRUE)
  max.age <- max(age,na.rm=TRUE)
  graphics::curve(mdl(x,unlist(sv)),from=min.age,to=max.age,
                  col=col.mdl,lwd=lwd.mdl,lty=lty.mdl,add=TRUE)
  ## Put the starting values to put on the plot
  graphics::legend("bottomright",
                   paste(names(sv),formatC(unlist(sv),format="f",digits=2),
                         sep="="),bty="n")
} # nocov end
