#' @title Finds reasonable starting values for parameters in specific parameterizations of common stock-recruitment models.
#'
#' @description Finds reasonable starting values for parameters in specific parameterizations of the \dQuote{Beverton-Holt}, \dQuote{Ricker},  \dQuote{Shepherd}, or \dQuote{Saila-Lorda} stock-recruitment models. Use \code{srFunShow()} to see the equations of each model.
#'
#' @details This function attempts to find reasonable starting values for a variety of parameterizations of the \dQuote{Beverton-Holt}, \dQuote{Ricker},  \dQuote{Shepherd}, or \dQuote{Saila-Lorda} stock-recruitment models. There is no guarantee that these starting values are the \sQuote{best} starting values. One should use them with caution and should perform sensitivity analyses to determine the impact of different starting values on the final model results.
#'
#' Starting values for the first parameterization of the Beverton-Holt model were derived by linearizing the function (inverting both sides and simplifying), fitting a linear model to the observed data, and extracting parameter values from the corresponding linear model parameters. Starting values for the other parameterizations of the Beverton-Holt model were derived from known relationships between the parameters of each parameterization and the first parameterization. If the computed starting value for the \code{Rp} parameter was larger than the largest observed recruitment value, then the starting value for \code{Rp} was set to the largest observed recruitment value.
#' 
#' Starting values for the Shepherd function were the same as those for the first parameterization of the Beverton-Holt function with the addition that \code{c=1}.
#'
#' Starting values for the Ricker parameterizations followed the same general procedure as described for the Beverton-Holt parameterizations. If the computed starting value for \code{atilde} was less than zero then the starting value was set to 0.00001.
#' 
#' Starting values for the Saila-Lorda function were the same as those for the first parameterization of the Ricker function with the addition that \code{c=1}.
#' 
#' @param formula A formula of the form \code{Recruits~Stock}.
#' @param data A data frame in which \code{Recruits} and \code{Stock} are found.
#' @param type A string that indicates the type of the stock-recruitment model. Must be one of \code{"BevertonHolt"}, \code{"Ricker"}, \code{"Shepherd"}, or \code{"SailaLorda"}.
#' @param param A numeric that indicates the parameterization of the stock-recruitment model type. This is ignored if \code{type="Shepherd"} or \code{type="SailaLorda"}
#' @param fixed A named list that contains user-defined rather than automatically generated (i.e., fixed) starting values for one or more parameters. See details.
#' @param plot A logical that indicates whether or not a plot of the data with the model fit at the starting values superimposed is created.
#' @param col.mdl A color for the model when \code{plot=TRUE}.
#' @param lwd.mdl A line width for the model when \code{plot=TRUE}.
#' @param lty.mdl A line type for the model when \code{plot=TRUE}.
#' @param cex.main A character expansion value for the main title when \code{plot=TRUE}.
#' @param col.main A color for the main title when \code{plot=TRUE}.
#' @param dynamicPlot DEPRECATED.
#' @param \dots Further arguments passed to the methods.
#'
#' @return A list that contains reasonable starting values. Note that the parameters will be listed in the same order and with the same names as listed in \code{\link{srFuns}}.
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: 13-Recruitment.
#'
#' @seealso See \code{\link{srFunShow}} and \code{\link{srFuns}} for related functionality. See \code{\link{nlsTracePlot}} for help troubleshooting nonlinear models that don't converge.
#'
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Beverton, R.J.H. and S.J. Holt. 1957. On the dynamics of exploited fish populations, Fisheries Investigations (Series 2), volume 19. United Kingdom Ministry of Agriculture and Fisheries, 533 pp.
#' 
#' Iles, T.C. 1994. A review of stock-recruitment relationships with reference to flatfish populations. Netherlands Journal of Sea Research 32:399-420.
#'
#' Quinn II, T.J. and R.B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press.
#'
#' Ricker, W.E. 1954. Stock and recruitment. Journal of the Fisheries Research Board of Canada 11:559-623.
#'
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada. [Was (is?) from http://www.dfo-mpo.gc.ca/Library/1485.pdf.]
#' 
#' Shepherd, J. 1982. A versatile new stock-recruitment relationship for fisheries and construction of sustainable yield curves. Journal du Conseil International pour l'Exploration de la Mar 40:67-75.
#'
#' @keywords manip
#'
#' @examples
#' ## Simple Examples
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
#' ## See examples in srFuns() for use of srStarts() when fitting stock-recruit models
#'
#' @rdname srStarts
#' @export
srStarts <- function(formula,data=NULL,
                     type=c("BevertonHolt","Ricker","Shepherd","SailaLorda","independence"),
                     param=1,fixed=NULL,
                     plot=FALSE,col.mdl="gray70",lwd.mdl=3,lty.mdl=1,
                     cex.main=0.9,col.main="red",dynamicPlot=FALSE,...) {
  ## some checks
  type <- match.arg(type)
  if (length(param)!=1) STOP("Only one 'param' is allowed.")
  if (!is.null(fixed)) {
    if(!is.list(fixed)) STOP("'fixed' must be a list.")
    if (any(names(fixed)=="")) STOP("Items in 'fixed' must be named.")
  }
  ## some checks on the formula
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (tmp$vnum!=2) {
    if (tmp$vnum==1) STOP("'srstarts' must have a formula with both LHS and RHS.")
    if (tmp$vnum>2) STOP("'srstarts' must have only one LHS and only one RHS variable.")
  }
  if (!tmp$metExpNumR) STOP("'srStarts' must have one and only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer")) STOP("LHS variable must be numeric.")
  if (!tmp$metExpNumE) STOP("'srStarts' must have only one RHS variable.")
  if (!tmp$Eclass %in% c("numeric","integer")) STOP("RHS variable must be numeric.")
  ## get the R and S vectors
  R <- tmp$mf[,tmp$Rname[1]]
  S <- tmp$mf[,tmp$Enames[1]]
  ## find starting values for the type and param
  switch(type,
    BevertonHolt=  { sv <- iSRStartsBH(S,R,param,fixed) },
    Ricker=        { sv <- iSRStartsR(S,R,param,fixed) },
    Shepherd =     { sv <- iSRStartsS(S,R,fixed) },
    SailaLorda =   { sv <- iSRStartsSL(S,R,fixed) },
    independence = { sv <- iSRStartsI(S,R,fixed) }
  ) # end type switch
  ## make the static plot if asked for
  if (plot) iSRStartsPlot(S,R,type,param,sv,col.mdl,lwd.mdl,lty.mdl,cex.main,col.main)
  ## Check if user wants to choose starting values from an interactive plot
  if (dynamicPlot) WARN("The 'dynamicPlot' functionality has been moved to 'srStartsDP' in the 'FSAsim' package.")
  ## return the list of starting values
  sv
}

##############################################################
# INTERNAL FUNCTIONS
##############################################################
#=============================================================
# Find starting values for Beverton-Holt models
#=============================================================
iSRStartsBH <- function(S,R,param,fixed) {
  ## some checks
  if (!param %in% 1:4) STOP("'param' must be in 1:4 when type='BevertonHolt'.")
  if (any(!names(fixed) %in% c("a","b","Rp")))
    STOP("'fixed' parameters not named 'a', 'b', or 'Rp'.")
  ## Linearized B-H #1 model without lognormal errors for starting values
  cfs <- stats::coef(stats::lm(I(1/R)~I(1/S)))
  ## Extract parameter values
  if ("a" %in% names(fixed)) a <- fixed[["a"]]
  else a <- 1/cfs[[2]]
  if (a<=0) WARN("'a' parameter not positive; likely a poor starting value.")
  if ("b" %in% names(fixed)) {
    if (!param %in% c(1,3)) STOP("'b' was fixed, but is not a parameter in the chosen 'param'")
    b <- fixed[["b"]]
  } else b <- cfs[[1]]*a
  if (b<=0) WARN("'b' parameter not positive; likely a poor starting value.")
  if ("Rp" %in% names(fixed)) {
    Rp <- fixed[["Rp"]]
    if (!param %in% c(2,4)) STOP("'Rp' was fixed, but is not a parameter in the chosen 'param'")
  } else {
    Rp <- a/b
    # don't let Rp be more than max of R
    if (Rp>max(R)) Rp <- max(R,na.rm=TRUE)
  }
  if (Rp<=0) WARN("'Rp' parameter not positive; likely a poor starting value.")
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
iSRStartsR <- function(S,R,param,fixed) {
  ## some checks
  if (!param %in% 1:3) STOP("'param' must be in 1:3 when type='Ricker'.")
  if (any(!names(fixed) %in% c("a","b","Rp"))) STOP("'fixed' parameters not named 'a', 'b', or 'Rp'.")
  ## Linearized Ricker #1 model with lognormal errors for starting values
  cfs <- stats::coef(stats::lm(log(R/S)~S))
  ## Extract parameter values
  if ("a" %in% names(fixed)) a <- fixed[["a"]]
  else a <- exp(cfs[[1]])
  if (a<=0) WARN("'a' parameter not positive; likely a poor starting value.")
  if ("b" %in% names(fixed)) {
    if (!param %in% c(1,2)) STOP("'b' was fixed, but is not a parameter in the chosen 'param'")
    b <- fixed[["b"]]
  } else b <- -cfs[[2]]
  if (b<=0) WARN("'b' parameter not positive; likely a poor starting value.")
  # don't allow negative atilde
  if (a>0) atilde <- log(a)
  else atilde <- 0.00001
  if (atilde<0) atilde <- 0.00001
  if ("Rp" %in% names(fixed)) {
    Rp <- fixed[["Rp"]]
    if (param!=3) STOP("'Rp' was fixed, but is not a parameter in the chosen 'param'")
  } else {
    Rp <- a/(b*exp(1))
    # don't let Rp be more than max of R
    if (Rp>max(R)) Rp <- max(R,na.rm=TRUE)
  }
  if (Rp<=0 & param==3) WARN("'Rp' parameter not positive; likely a poor starting value.")
  ## Return a list of starting values
  if (param==1) sv <- list(a=a,b=b)
  else if (param==2) sv <- list(a=atilde,b=b)
  else sv <- list(a=a,Rp=Rp)
  sv
} # end internal iSRStartsR

#=============================================================
# Find starting values for Shepherd model
#=============================================================
iSRStartsS <- function(S,R,fixed) {
  if (any(!names(fixed) %in% c("a","b","c")))
    STOP("'fixed' parameters not named 'a', 'b', or 'c'.")
  if ("c" %in% names(fixed)) {
    c <- fixed[["c"]]
    # must remove fixed 'c' to send to startsBH
    fixed <- fixed[-which(names(fixed)=="c")]
    if (length(fixed)==0) fixed <- NULL
  } else c <- 1
  if (c<=0) WARN("'c' parameter not positive; likely a poor starting value.")
  tmp <- iSRStartsBH(S,R,param=1,fixed)
  c(tmp,c=c)
}

#=============================================================
# Find starting values for Saila-Lorda models
#=============================================================
iSRStartsSL <- function(S,R,fixed) {
  if (any(!names(fixed) %in% c("a","b","c")))
    STOP("'fixed' parameters not named 'a', 'b', or 'c'.")
   if ("c" %in% names(fixed)) {
    c <- fixed[["c"]]
    # must remove fixed 'c' to send to startsBH
    fixed <- fixed[-which(names(fixed)=="c")]
    if (length(fixed)==0) fixed <- NULL
  }
  else c <- 1
  if (c<=0) WARN("'c' parameter not positive; likely a poor starting value.")
  tmp <- iSRStartsR(S,R,param=1,fixed)
  c(tmp,c=c)
}

#=============================================================
# Find starting values for independence model
#=============================================================
iSRStartsI <- function(S,R,fixed) {
  if (any(!names(fixed) %in% c("a")))
    STOP("'fixed' parameters not named 'a'.")
  if ("a" %in% names(fixed)) a <- fixed[["a"]]
  else a <- stats::coef(stats::lm(R~0+S))[[1]]
  if (a<=0) WARN("'a' parameter not positive; likely a poor starting value.")
  list(a=a)
}

#=============================================================
# Static plot of starting values
#=============================================================
iSRStartsPlot <- function(S,R,type,param,sv,col.mdl,lwd.mdl,lty.mdl,
                          cex.main,col.main) { # nocov start
  ## attempting to get by bindings warning in RCMD CHECK
  x <- NULL
  ## Make a title
  ttl1 <- ifelse(type %in% c("BevertonHolt","Ricker"),paste0(type," #",param),type)
  ttl <- paste(ttl1,"STARTING VALUES")
  ## Plot the data
  max.S <- max(S,na.rm=TRUE)
  graphics::plot(S,R,pch=19,xlim=c(0,max.S),xlab="Stock Level",ylab="Recruitment Level",
                 main=ttl,cex.main=cex.main,col.main="red")
  ## Plot the model
  mdl <- srFuns(type,param)
  graphics::curve(mdl(x,unlist(sv)),from=0,to=max.S,col=col.mdl,lwd=lwd.mdl,lty=lty.mdl,add=TRUE)
  graphics::legend("topleft",paste(names(sv),formatC(unlist(sv),format="f",digits=3),sep="="),
         bty="n",cex=cex.main)
} # nocov end
