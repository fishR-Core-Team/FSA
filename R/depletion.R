#' @title Computes the Leslie or DeLury population estimate from catch and effort data.
#'
#' @description Computes the Leslie or DeLury estimates of population size and catchability coefficient from paired catch and effort data. The Ricker modification may also be used.
#'
#' @details For the Leslie method, a linear regression model of catch-per-unit-effort on cumulative catch prior to the sample is fit. The catchability coefficient (q) is estimated from the negative of the slope and the initial population size (No) is estimated by dividing the intercept by the catchability coefficient. If \code{Ricker.mod=TRUE} then the cumulative catch is modified to be the cumulative catch prior to the sample plus half of the catch of the current sample.
#' 
#' For the DeLury method, a linear regression model of log (catch-per-unit-effort) on cumulative effort is fit. The catchability coefficient (q) is estimated from the negative of the slope and the initial population size (No) is estimated by dividing the intercept as an exponent of e by the catchability coefficient. If \code{Ricker.mod=TRUE} then the cumulative effort is modified to be the cumulative effort prior to the sample plus half of the effort of the current sample.
#'
#' Standard errors for the catchability and population size estimates are computed from formulas on page 298 (for Leslie) and 303 (for DeLury) from Seber (2002). Confidence intervals are computed using standard large-sample normal distribution theory with the regression error df.
#' 
#' @param catch A numeric vector of catches of fish at each time, or a formula of the form \code{catch~effort}.
#' @param data A data.frame from which the variables in the \code{catch} formula can be found. Not used if \code{catch} is not a formula.
#' @param effort A numeric vector of efforts expended at each time.
#' @param method A single string that indicates which depletion method to use
#' @param Ricker.mod A single logical that indicates whether to use the modification proposed by Ricker (=TRUE) or not (=FALSE, default).
#' @param object An object saved from the \code{removal} call (i.e., of class \code{depletion}).
#' @param x An object saved from the \code{depletion} call (i.e., of class \code{depletion}).
#' @param as.df A logical that indicates whether the results of \code{coef}, \code{confint}, or \code{summary} should be returned as a data.frame. Ignored in \code{summary} if \code{parm="lm"}.
#' @param verbose A logical that indicates whether a reminder of the method used should be printed with the summary results.
#' @param parm A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param conf.level A single number that represents the level of confidence to use for constructing confidence intervals.
#' @param level Same as \code{conf.level} but used for compatibility with generic \code{confint} function.
#' @param digits The number of digits to round the \code{rSquared} result to.
#' @param percent A logical that indicates if the \code{rSquared} result should be returned as a percentage (\code{=TRUE}) or as a proportion (\code{=FALSE}; default).
#' @param ylab A label for the y-axis.
#' @param xlab A label for the x-axis.
#' @param pch A numeric that indicates the type of plotting character.
#' @param col.pt A string that indicates the color of the plotted points.
#' @param col.mdl A string that indicates the color of the fitted line.
#' @param lwd A numeric that indicates the line width of the fitted line.
#' @param lty A numeric that indicates the type of line used for the fitted line.
#' @param pos.est A single string to identify where to place the estimated population estimate and catchability on the plot. Can be set to one of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} or \code{"center"} for positioning the estimated mortality rates on the plot. Typically \code{"bottomleft"} (DEFAULT) and \code{"topright"} will be \dQuote{out-of-the-way} placements. Set \code{pos.est} to \code{NULL} to remove the estimated population size and catchability coefficient from the plot.
#' @param cex.est A single numeric that identifies the character expansion value for the estimated population estimate and catchability placed on the plot.
#' @param \dots Additional arguments for methods.
#'
#' @return A list with the following items:
#'  \itemize{
#'    \item method A string that indicates whether the \code{"Leslie"} or \code{"DeLury"} model was used.
#'    \item catch The original vector of catches.
#'    \item effort The original vector of efforts.
#'    \item cpe A computed vector of catch-per-unit-effort for each time.
#'    \item KorE A computed vector of cumulative catch (K; Leslie method) or effort (E; DeLury method).
#'    \item lm The \code{lm} object from the fit of CPE on K (Leslie method) or log(CPE) on E (DeLury method).
#'    \item est A 2x2 matrix that contains the estimates and standard errors for No and q.
#'  }
#'
#' @section testing:  The Leslie method without the Ricker modification and the DeLury method with the Ricker modification matches the results from \code{\link[fishmethods]{deplet}} in \pkg{fishmethods} for the \code{\link[fishmethods]{darter}} (from \pkg{fishmethods}), \code{\link[FSAdata]{LobsterPEI}} and \code{\link[FSAdata]{BlueCrab}} from \pkg{FSAdata}, and \code{\link{SMBassLS}} for N0 to whole numbers, the SE for No to one decimal, q to seven decimals, and the SE of q to at least five decimals.
#' 
#' The Leslie method matches the results of Seber (2002) for N0, q, and the CI for Q but not the CI for N (which was so far off that it might be that Seber's result is incorrect) for the lobster data and the q and CI for q but the NO or its CI (likely due to lots of rounding in Seber 2002) for the Blue Crab data.
#' 
#' The Leslie and DeLury methods match the results of Ricker (1975) for No and Q but not for the CI of No (Ricker used a very different method to compute CIs).
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: 10-Abundance from Depletion Data.
#'
#' @seealso See \code{\link{removal}} for related functionality and \code{\link[fishmethods]{deplet}} in \pkg{fishmethods} for similar functionality.
#'
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada. [Was (is?) from http://www.dfo-mpo.gc.ca/Library/1485.pdf.]
#'
#' Seber, G.A.F. 2002. The Estimation of Animal Abundance. Edward Arnold, Second edition (reprinted).
#'
#' @keywords hplot manip
#'
#' @aliases depletion plot.depletion summary.depletion coef.depletion anova.depletion confint.depletion
#'
#' @examples
#' ## Leslie model examples
#' # no Ricker modification
#' l1 <- depletion(SMBassLS$catch,SMBassLS$effort,method="Leslie")
#' summary(l1)
#' summary(l1,verbose=TRUE)
#' summary(l1,parm="No")
#' rSquared(l1)
#' rSquared(l1,digits=1,percent=TRUE)
#' cbind(Est=coef(l1),confint(l1))
#' cbind(Est=coef(l1,parm="No"),confint(l1,parm="No"))
#' cbind(Est=coef(l1,parm="q"),confint(l1,parm="q"))
#' summary(l1,parm="lm")
#' plot(l1)
#' 
#' # with Ricker modification
#' l2 <- depletion(SMBassLS$catch,SMBassLS$effort,method="Leslie",Ricker.mod=TRUE)
#' summary(l2)
#' cbind(Est=coef(l2),confint(l1))
#' plot(l2)
#'
#' ## DeLury model examples
#' # no Ricker modification
#' d1 <- depletion(SMBassLS$catch,SMBassLS$effort,method="DeLury")
#' summary(d1)
#' summary(d1,parm="q")
#' summary(d1,verbose=TRUE)
#' rSquared(d1)
#' cbind(Est=coef(d1),confint(d1))
#' summary(d1,parm="lm")
#' plot(d1)
#' 
#' # with Ricker modification
#' d2 <- depletion(SMBassLS$catch,SMBassLS$effort,method="DeLury",Ricker.mod=TRUE)
#' summary(d2)
#' cbind(Est=coef(d2),confint(d2))
#' cbind(Est=coef(d2,parm="q"),confint(d2,parm="q"))
#' plot(d2)
#'
#' # with formula notation
#' l3 <- depletion(catch~effort,data=SMBassLS)
#' summary(l3)
#' 
#' # summarizing by group (requires dplyr package)
#' # Dummy example data (grp="A" is SMBassLS example ... just FYI)
#' tmpdf <- data.frame(ct=c(131,69,99,78,56,76,49,42,63,47,
#'                          117,75,87,67,58,67,42),
#'                     ft=c(7,7,7,7,7,7,7,7,7,7,
#'                          5,7,5,5,4,6,5),
#'                    grp=as.factor(c("A","A","A","A","A","A","A","A","A","A",
#'                                    "B","B","B","B","B","B","B")))
#'                                    
#' if (require(dplyr)) {
#'   res1 <- tmpdf %>%
#'     dplyr::group_by(grp) %>%
#'       dplyr::group_modify(~summary(depletion(ct~ft,data=.x),as.df=TRUE)) %>%
#'       as.data.frame() # removes tibble and grouping structure
#'   res1
#' 
#'   res2 <- tmpdf %>%
#'     dplyr::group_by(grp) %>%
#'       dplyr::group_modify(~confint(depletion(ct~ft,data=.x),as.df=TRUE)) %>%
#'       as.data.frame() # removes tibble and grouping structure
#'   res2
#' 
#'   res <- dplyr::left_join(res1,res2,by="grp")
#'   res
#' }
#' 
#' @rdname depletion
#' @export
depletion <- function(catch,...) {
  UseMethod("depletion") 
}

#' @rdname depletion
#' @export
depletion.formula <- function(catch,data,method=c("Leslie","DeLury","Delury"),
                              Ricker.mod=FALSE,...) {
  ## Handle the formula and perform some checks
  tmp <- iHndlFormula(catch,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR)
    STOP("'depletion' must have only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer"))
    STOP("LHS variable (catch) must be numeric.")
  if (!tmp$metExpNumE)
    STOP("'depletion' must have only one RHS variable.")
  if (!tmp$Eclass %in% c("numeric","integer"))
    STOP("RHS variable (effort) must be numeric.")
  ## Get variables from model frame
  catch <- tmp$mf[,tmp$Rname]
  effort <- tmp$mf[,tmp$Enames]
  ## Call the default function
  depletion.default(catch,effort,method=method,Ricker.mod=Ricker.mod)
}

#' @rdname depletion
#' @export
depletion.default <- function(catch,effort,method=c("Leslie","DeLury","Delury"),
                              Ricker.mod=FALSE,...) {
  # check method, change bad spelling of DeLury if necessary
  method <- match.arg(method)
  if (method=="Delury") method <- "DeLury"
  # some initial checks
  iCheckCatchEffort(catch,effort)
  # goto internal function depending on method
  switch(method,
    Leslie= { d <- iLeslie(catch,effort,Ricker.mod) },
    DeLury= { d <- iDeLury(catch,effort,Ricker.mod) }
  )
  class(d) <- "depletion"
  d  
}

##############################################################
# INTERNAL function to check Catch and Effort vectors
##############################################################
iCheckCatchEffort <- function(catch,effort) {
  if (!is.numeric(catch)) STOP("'catch' must be a numeric vector.")
  if (!is.vector(catch)) STOP("'catch' must be a vector.")
  if (any(catch<0)) STOP("All 'catch' must be non-negative.")
  if (!is.numeric(effort)) STOP("'effort' must be a numeric vector.")
  if (!is.vector(effort)) STOP("'effort' must be a vector.")
  if (any(effort<=0)) STOP("All 'effort' must be positive.")
  if (length(catch)!=length(effort)) STOP("'catch' and 'effort' must be same length.")
  if (length(catch)<3) STOP("Must have at least 3 values in 'catch'.")
}

##############################################################
# INTERNAL function for compute the Leslie estimates
##############################################################
iLeslie <- function(catch,effort,Ricker.mod) {
  cpe <- catch/effort
  ifelse(!Ricker.mod,K <- cumsum(catch)-catch,K <- cumsum(catch)-(catch/2))
  n <- length(catch)
  # main regression
  lm1 <- stats::lm(cpe~K)
  tmp <- summary(lm1)
  # extract results for calculations
  s <- tmp$sigma
  q <- -tmp$coef[2,"Estimate"]
  q.SE <- tmp$coef[2,"Std. Error"]
  N0 <- tmp$coef[1,"Estimate"]/q
  # This is Seber (2002) variance equation on page 298 where
  #    (Seber's symbol listed first) sigma=s, K=q, s=n, N=N0, and x=K
  ss.K <- stats::var(K)*(n-1)
  N0.SE <- s/q*sqrt(1/n + ((N0-mean(K))^2)/ss.K)    
  # Create matrix of parameter estimates
  mres <- cbind(c(N0,q),c(N0.SE,q.SE))
  rownames(mres) <- c("No","q")
  colnames(mres) <- c("Estimate","Std. Err.")
  # check significance of slopes before leaving
  iCheckRegSig(lm1)
  # put together a return list
  list(method="Leslie",catch=catch,effort=effort,cpe=cpe,K=K,lm=lm1,est=mres)
}

##############################################################
# INTERNAL function for compute the DeLury estimates
##############################################################
iDeLury <- function(catch,effort,Ricker.mod) {
  if (any(catch==0)) STOP("Can't have zero catches with 'DeLury' method.")
  cpe <- catch/effort
  ifelse(!Ricker.mod,E <- cumsum(effort)-effort,E <- cumsum(effort)-(effort/2))
  n <- length(effort)
  # main regression
  lm1 <- stats::lm(log(cpe)~E)
  tmp <- summary(lm1)
  # extract results for caculations
  s <- tmp$sigma
  q <- -tmp$coef[2,"Estimate"]
  q.SE <- tmp$coef[2,"Std. Error"]
  N0 <- exp(tmp$coef[1,"Estimate"])/q
  # This is Seber (2002) variance equation on page 303 where
  #    (Seber's symbol listed first) sigma=s, k=q, s=n, N=N0, and x=E
  ss.E <- stats::var(E)*(n-1)
  N0.SE <- s*N0*sqrt(1/n + (((q*mean(E)-1)/q)^2)*(1/ss.E))  
  # Create matrix of parameter estimates
  mres <- cbind(c(N0,q),c(N0.SE,q.SE))
  rownames(mres) <- c("No","q")
  colnames(mres) <- c("Estimate","Std. Err.")
  # check significance of slopes before leaving
  iCheckRegSig(lm1)
  # put together a return list
  list(method="DeLury",catch=catch,effort=effort,cpe=cpe,E=E,lm=lm1,est=mres)
}

##############################################################
# INTERNAL function for compute the Leslie estimates
##############################################################
iCheckRegSig <- function(tmp) {
  tmp.slope <- stats::coef(tmp)[2]
  if (tmp.slope>0) WARN("Estimates are suspect as model did not exhibit a negative slope.")
  tmp.slope.p <- stats::anova(tmp)[1,"Pr(>F)"]
  if (tmp.slope.p>0.05 & tmp.slope<0) WARN("Estimates are suspect as model did not exhibit a significantly (p>0.05) negative slope.")
}

#' @rdname depletion
#' @export
summary.depletion <- function(object,parm=c("all","both","No","q","lm"),
                              verbose=FALSE,as.df=FALSE,...) {
  if (verbose) message("The ",object$method," method was used.")
  parm <- match.arg(parm)
  if(parm=="lm") {
    res <- summary(object$lm,...)
  } else {
    res <- object$est
    if (parm %in% c("all","both")) {
      if (as.df) {
        res <- data.frame(No_Est=res[["No","Estimate"]],
                          No_SE=res[["No","Std. Err."]],
                          q_Est=res[["q","Estimate"]],
                          q_SE=res[["q","Std. Err."]])
      }
    } else {
      res <- res[parm,,drop=FALSE]
      if (as.df) {
        res <- data.frame(Est=res[[parm,"Estimate"]],
                          SE=res[[parm,"Std. Err."]])
        names(res) <- paste(parm,names(res),sep="_")
      }
    }
  }
  res
}

#' @rdname depletion
#' @export
coef.depletion <- function(object,parm=c("all","both","No","q","lm"),as.df=FALSE,...) {
  parm <- match.arg(parm)
  if(parm=="lm") {
    tmp <- stats::coef(object$lm,...)
    if(as.df) tmp <- data.frame(Intercept=tmp[["(Intercept)"]],K=tmp[["K"]])
  } else {
    tmp <- object$est[,"Estimate"]
    if(parm %in% c("all","both")) {
      if (as.df) tmp <- data.frame(No=tmp[["No"]],q=tmp[["q"]])
    } else {
      tmp <- tmp[parm]
      if (as.df) tmp <- data.frame(No=tmp[[parm]])
    }
  }
  tmp
}

#' @rdname depletion
#' @export
confint.depletion <- function(object,parm=c("all","both","No","q","lm"),
                              level=conf.level,conf.level=0.95,as.df=FALSE,...) {
  parm <- match.arg(parm)
  
  ## Check on conf.level
  iCheckConfLevel(conf.level) 
  
  if (parm=="lm") {
    res <- stats::confint(object$lm,level=conf.level)
    colnames(res) <- iCILabel(conf.level)
    if (as.df) {
      res <- data.frame(Intercept_LCI=res[["(Intercept)","95% LCI"]],
                        Intercept_UCI=res[["(Intercept)","95% UCI"]],
                        K_LCI=res[["K","95% LCI"]],
                        K_UCI=res[["K","95% UCI"]])
    }
  } else {
    t <- stats::qt(1-(1-conf.level)/2,summary(object$lm)$df[2])
    tmp <- summary(object)
    res <- cbind(tmp[,"Estimate"]-t*tmp[,"Std. Err."],
                 tmp[,"Estimate"]+t*tmp[,"Std. Err."])
    colnames(res) <- iCILabel(conf.level)
    if (parm %in% c("all","both")) {
      if (as.df) {
        res <- data.frame(No_LCI=res[["No","95% LCI"]],
                          No_UCI=res[["No","95% UCI"]],
                          q_LCI=res[["q","95% LCI"]],
                          q_UCI=res[["q","95% UCI"]])
      }
    } else {
      res <- res[parm,,drop=FALSE]
      if (as.df) {
        res <- data.frame(LCI=res[[parm,"95% LCI"]],
                          UCI=res[[parm,"95% UCI"]])
        names(res) <- paste(parm,names(res),sep="_")
      }
    }
  }
  res
}

#' @rdname depletion
#' @export
anova.depletion <- function(object,...) {
  stats::anova(object$lm,...)
}

#' @rdname depletion
#' @export
rSquared.depletion <- function(object,digits=getOption("digits"),
                               percent=FALSE,...) {
  rSquared(object$lm,digits=digits,percent=percent,...)
}

#' @rdname depletion
#' @export
plot.depletion <- function(x,xlab=NULL,ylab=NULL,
                           pch=19,col.pt="black",
                           col.mdl="gray70",lwd=1,lty=1,
                           pos.est="topright",cex.est=0.95,...) { # nocov start
  # make base plot
  if (x$method=="Leslie") {
    if (is.null(xlab)) xlab <- "Cumulative Catch"
    if (is.null(ylab)) ylab <- "CPE"
    graphics::plot(x$K,x$cpe,pch=pch,col=col.pt,xlab=xlab,ylab=ylab,...)
  } else {
    if (is.null(xlab)) xlab <- "Cumulative Effort"
    if (is.null(ylab)) ylab <- "log(CPE)"
    graphics::plot(x$E,log(x$cpe),pch=pch,col=col.pt,xlab=xlab,ylab=ylab,...)
  }
  # add best-fit line
  graphics::abline(x$lm,col=col.mdl,lwd=lwd,lty=lty)
  # add values to plot
  if (!is.null(pos.est)) {
    graphics::legend(pos.est,legend=paste0("No=",round(x$est["No","Estimate"],0),
                                           "\nq=",round(x$est["q","Estimate"],4)),
                     cex=cex.est,bty="n")
  }
} # nocov end
