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
#' @aliases srStarts srStarts.default srStarts.formula
#'
#' @param S Either a vector of observed stock levels or a formula of the form \code{R~S}.
#' @param R A vector of observed recruitment levels.
#' @param data A data frame from which the vectors of observed stock and recruitment levels can be found.  Used only if a formula is used in \code{s}.
#' @param type A string that indicates the type of the stock-recruitment model.  Must be one of \code{"BevertonHolt"}, \code{"Ricker"}, \code{"Shepherd"}, or \code{"SailaLorda"}.
#' @param param A numeric that indicates the parameterization of the stock-recruitment model type.  This is ignored if \code{type="Shepherd"} or \code{type="SailaLorda"}
#' @param plot A logical that indicates whether or not a plot of the data with the model fit at the starting values superimposed is created.
#' @param col.mdl A color for the model when \code{plot=TRUE}.
#' @param lwd.mdl A line width for the model when \code{plot=TRUE}.
#' @param lty.mdl A line type for the model when \code{plot=TRUE}.
#' @param \dots Further arguments passed to the methods.
#'
#' @return A list that contains reasonable starting values.  Note that the parameters will be listed in the same order and with the same names as listed in \code{\link{srFuns}}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{srModels}}, \code{\link{srFuns}}, and \code{\link{srSim}}.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/StockRecruit.pdf}
#'
#' @references 
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
#'
#' ## Simple Examples with a Plot
#' srStarts(recruits~stock,data=CodNorwegian,type="Ricker",plot=TRUE)
#' srStarts(recruits~stock,data=CodNorwegian,type="BevertonHolt",plot=TRUE)
#' srStarts(recruits~stock,data=CodNorwegian,type="Shepherd",plot=TRUE)
#' srStarts(recruits~stock,data=CodNorwegian,type="SailaLorda",plot=TRUE)
#' 
#' ## See examples in srFuns() for use of srStarts() when fitting stock-recruit models
#'
#' @rdname srStarts
#' @export
srStarts <- function(S,...) {
  UseMethod("srStarts") 
}

#' @rdname srStarts
#' @export
srStarts.formula <- function(S,data=NULL,...) {
  mf <- model.frame(S,data)
  x <- mf[,2]
  y <- mf[,1]
  srStarts.default(x,y,,...)
}

#' @rdname srStarts
#' @export
srStarts.default <- function(S,R,type=c("BevertonHolt","Ricker","Shepherd","SailaLorda"),
                             param=1,plot=FALSE,col.mdl="gray70",lwd.mdl=3,lty.mdl=1,...) {
  ## attempting to get by bindings warning in RCMD CHECK
  x <- NULL
  ## some checks
  type <- match.arg(type)
  if (length(param)!=1) stop("Only one 'param' is allowed.",call.=FALSE)
  ## handle each type separately
  switch(type,
    BevertonHolt={ sv <- iSRStartsBH(S,R,param) },
    Ricker=      { sv <- iSRStartsR(S,R,param) },
    Shepherd =   { sv <- iSRStartsS(S,R) },
    SailaLorda = { sv <- iSRStartsSL(S,R) }
  ) # end type switch
  if (plot) {
    ttl1 <- ifelse(type %in% c("BevertonHolt","Ricker"),paste0(type," #",param),type)
    ttl2 <- paste(paste(names(sv),formatC(unlist(sv),format="f",digits=5),sep="="),collapse=", ")
    ttl <- paste(ttl1,"at",ttl2)
    plot(R~S,pch=19,xlim=c(0,max(S)),xlab="Stock Level",ylab="Recruitment Level",main=ttl)
    mdl <- srFuns(type,param)
    curve(mdl(x,unlist(sv)),from=0,to=max(S),col=col.mdl,lwd=lwd.mdl,lty=lty.mdl,add=TRUE)
  }
  sv
}

##############################################################
# INTERNAL FUNCTIONS
##############################################################
iSRStartsBH <- function(S,R,param) {
  ## some checks
  if (!param %in% 1:4) stop("'param' must be in 1:4 when type='BevertonHolt'.",call.=FALSE)
  ## Linearized B-H #1 model without lognormal errors for starting values
  cfs <- coef(lm(I(1/R)~I(1/S)))
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
}

iSRStartsR <- function(S,R,param) {
  ## some checks
  if (!param %in% 1:3) stop("'param' must be in 1:3 when type='Ricker'.",call.=FALSE)
  ## Linearized Ricker #1 model with lognormal errors for starting values
  cfs <- coef(lm(log(R/S)~S))
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
}

iSRStartsS <- function(S,R) {
  c(iSRStartsBH(S,R,param=1),c=1)
}

iSRStartsSL <- function(S,R) {
  c(iSRStartsR(S,R,param=1),c=1)
}
