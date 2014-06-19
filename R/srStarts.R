#' @title Finds reasonable starting values for the parameters in a specific stock-recruitment model parameterization.
#'
#' @description Finds reasonable starting values for the parameters in a specific stock-recruitment model parameterization.
#'
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of stock-recruitment models.  There is no guarantee that these starting values are the \sQuote{best} starting values.  One should use them with caution and should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#'
#' Starting values for the first parameterization of the Beverton-Holt model were derived by linearizing the first parameterization (inverting both sides and simplifying), fitting the linear model to the observed data, and extracting parameter values that corresponding to the linear model parameters.  Starting values for the other parameterizations of the Beverton-Holt model were derived from known relationships between the parameters of each parameterization and the first parameterization.  If the computed starting value for the Rp parameter was larger then the largest observed recruitment level then the Rp starting value was set to the largest observed recruitment value.
#'
#' Starting values for the Ricker parameterizations followed the same general algorithm as described for the Beverton-Holt parameterizations.  If the computed starting value for atilde was less than zero then the starting value was set to 0.00001.
#'
#' @aliases srStarts srStarts.default srStarts.formula
#'
#' @param S Either a vector of observed stock levels or a formula of the form \code{R~S}.
#' @param R A vector of observed recruitment levels.
#' @param data A data frame from which the vectors of observed stock and recruitment levels can be found if a formula is used.
#' @param type A string that indicates the type of the stock-recruitment model.
#' @param param A numeric that indicates the parameterization of the stock-recruitment model type.
#' @param plot A logical that indicates whether a plot of the data with the model fit at the starting values superimposed should be created.
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
#' Quinn II, T.J. and R.B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press.
#'
#' Ricker, W.E. 1954. Stock and recruitment. Journal of the Fisheries Research Board of Canada 11:559-623.
#'
#' Ricker, W.E. 1975. \href{http://www.dfo-mpo.gc.ca/Library/1485.pdf}{Computation and interpretation of biological statistics of fish populations}. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.
#'
#' @keywords manip
#'
#' @examples
#' ## Simple Examples
#' data(CodNorwegian)
#' srStarts(recruits~stock,data=CodNorwegian)
#' srStarts(recruits~stock,data=CodNorwegian,type="Ricker")
#'
#' ## Simple Example with a Plot
#' srStarts(recruits~stock,data=CodNorwegian,type="Ricker",plot=TRUE)
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
srStarts.default <- function(S,R,type=c("BevertonHolt","Ricker"),param=1,plot=FALSE,...) {
  x <- NULL  # attempting to get by bindings warning in RCMD CHECK
  type <- match.arg(type)
  switch(type,
    BevertonHolt={
      if (is.na(match(param,1:4))) stop("'param' argument must be in 1:4 when type='BevertonHolt'.",call.=FALSE)
      # Linearized B-H #1 model without lognormal errors for starting values
      lms <- lm(I(1/R)~I(1/S))
      a <- 1/coef(lms)[2]
      b <- coef(lms)[1]/coef(lms)[2]
      attributes(a) <- attributes(b) <- NULL
      Rp <- a/b
      if (Rp>max(R)) Rp <- max(R)
      atilde <- 1/a
      btilde <- b/a
      if (param==1) sv <- list(a=a,b=b)
      else if (param==2) sv <- list(a=a,Rp=Rp)
      else if (param==3) sv <- list(a=atilde,b=btilde)
      else sv <- list(a=atilde,Rp=Rp)
    },
    Ricker={
      if (is.na(match(param,1:3))) stop("'param' argument must be in 1:3 when type='Ricker'.",call.=FALSE)
      # Linearized Ricker #1 model with lognormal errors for starting values
      lms <- lm(log(R/S)~S)
      a <- exp(coef(lms)[1])
      b <- -coef(lms)[2]
      attributes(a) <- attributes(b) <- NULL
      atilde <- log(a)
      # don't allow negative atilde
      if (atilde<0) atilde <- 0.00001
      Rp <- a/(b*exp(1))
      if (param==1) sv <- list(a=a,b=b)
      else if (param==2) sv <- list(a=atilde,b=b)
      else sv <- list(a=a,Rp=Rp)
    }
  ) # end type switch
  if (plot) {
    plot(R~S,pch=19,xlab="Stock Level",ylab="Recruitment Level",main=paste(type," #",param," paramaterization fit at intial values",sep=""),xlim=c(0,max(S)))
    mdl <- srFuns(type,param)
    curve(mdl(x,sv[[1]],sv[[2]]),from=0,to=max(S),col="red",lwd=3,add=TRUE)
    legend("topleft",legend=paste(names(sv),formatC(unlist(sv),format="f",digits=3),sep=" = "))
  }
  sv
}

#' @rdname srStarts
#' @export
srStarts.formula <- function(S,data=NULL,...) {
  mf <- model.frame(S,data)
  x <- mf[,2]
  y <- mf[,1]
  srStarts.default(x,y,,...)
}
