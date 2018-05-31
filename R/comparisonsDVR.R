#' @title Tests for significant differences among all pairs of slopes in a dummy variable regression (DVR).
#'
#' @description Tests for significant differences among all pairs of slopes in a dummy variable regression where the dummy variables all stem from one factor.
#'
#' @details In a dummy variable regression the coefficient for the interaction between the covariate (x) and a dummy variable tests for a difference in slopes between the level of the dummy variable and the reference level. Thus, all dummy variables from a particular linear model fit only compare slopes with the reference level. Other slope comparisons may be found by changing the reference level, which requires refitting the model. This function automates this sequential process and produces a data.frame that shows the estimated difference, an unadjusted confidence interval for the difference, and the unadjusted and adjusted (for multiple comparisons) p-values for testing that the difference in slopes is equal to zero for each pair of levels. The adjusted p-values may be computed with any of the methods coded in \code{\link[stats]{p.adjust}} (see \code{p.adjust.methods} there).
#' 
#' @param mdl A \code{lm} object.
#' @param method A string indicating the method of p-value adjustment to use. See details and \code{p.adjust.methods} (documented in \code{\link[stats]{p.adjust}}.
#' @param conf.level A single number that represents the level of confidence to use for constructing confidence intervals.
#' @param x A \code{compSlopes} object (i.e., returned from \code{compSlopes}).
#' @param order.slopes A logical indicating whether the slopes should be ordered from smallest to largest upon output.
#' @param digits A numeric that controls the number of digits to print.
#' @param \dots Other arguments sent to \code{print}.
#' 
#' @return A list with three components. The first component contains the p-value adjustment method in \code{method}. The second component, called \code{comparisons}, is a data.frame that contains the following:
#' \tabular{ll}{
#' \code{comparison} \tab Description of how the difference in levels was computed.\cr
#' \code{diff} \tab The estimated difference in slope values.\cr
#' \code{lwr} \tab Lower confidence bound for difference in slope values.\cr 
#' \code{upr} \tab Upper confidence bound for difference in slope values.\cr 
#' \code{p.unadj} \tab Unadjusted p-value for testing the difference in slopes is zero.\cr 
#' \code{p.adj} \tab Adjusted p-value for testing the difference in slopes is zero.\cr
#' }
#'
#' The third component, called \code{slopes}, is a data.frame that contains the following:
#' \tabular{ll}{
#' \code{level} \tab A level name.\cr
#' \code{slope} \tab The estimated slope value for the given level.\cr
#' \code{XX LCI} \tab Lower confidence bound for difference in slope values.\cr
#' \code{XX UCI} \tab Upper confidence bound for difference in slope values.\cr
#' \code{p.unadj} \tab Unadjusted p-value for testing the slope is zero.\cr 
#' \code{p.adj} \tab Adjusted p-value for testing the slope is zero.\cr
#'}
#'
#' The \code{print} function prints the results nicely.
#' 
#' @note This function only works for linear models with one factor variable.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @seealso \code{\link{compIntercepts}} in \pkg{FSA}.
#'
#' @aliases compSlopes print.compSlopes
#' 
#' @keywords htest
#' 
#' @examples
#' Mirex$year <- factor(Mirex$year)
#' # fit a dummy variable regression, see that slopes differ
#' lm1 <- lm(mirex~weight*year,data=Mirex)
#' anova(lm1)
#' # compare all pairs of slopes using default Holm control
#' compSlopes(lm1)
#' # compare all pairs of slopes using the false discovery rate control
#' compSlopes(lm1,method="fdr")
#' # visualize the results
#' fitPlot(lm1)
#'
#' @rdname compSlopes
#' @export
compSlopes <- function(mdl,method=stats::p.adjust.methods,
                       conf.level=0.95,order.slopes=TRUE,digits=getOption("digits")) {
  ## Perform some checks
  method <- match.arg(method)
  lmtype <- iTypeoflm(mdl)
  if (lmtype$type!="IVR")
    STOP("Function only works for dummy variable regressions.")
  if (lmtype$Enum>2)
    STOP("Function only works for dummy variable regressions\n with one factor and one covariate variable.")
  ## isolate model information
  y <- lmtype$mf[,lmtype$Rpos]
  x <- lmtype$mf[,lmtype$ENumPos]
  g <- lmtype$mf[,lmtype$EFactPos]
  ## Handle level names and number and total number of comparisons
  lev.names <- levels(g)
  num.lvls <- length(levels(g))
  if (num.lvls<3) WARN("Function not needed with fewer than three levels.")
  num.comps <- choose(num.lvls,2)
  ## initiate results data.frames -- sdf=slopes, cdf=slope comparisons
  tmp <- rep(0,num.lvls)  # zeroes to initiate
  sdf <- data.frame(level=lev.names,slopes=tmp,lwr=tmp,upr=tmp,p.unadj=tmp)
  tmp <- rep(0,num.comps)  # zeroes to initiate
  cdf <- data.frame(comparison=rep("j",num.comps),diff=tmp,lwr=tmp,upr=tmp,p.unadj=tmp)
  cdf$comparison <- toString(cdf$comparison)  # must convert to strings
  ## cycle through multiple models extracting information about
  ## each slope and each comparison of slopes
  for (i in seq_len(num.lvls)) {
    # fit model and get results
    lm1 <- stats::lm(y~x*g)
    sum <- summary(lm1)
    conf <- stats::confint(lm1,level=conf.level)           
    # get individual slopes information
    sdf <- iExtractSlopes(sum,conf,sdf,i,conf.level)
    if (i < num.lvls) {
      # get slopes comparison information
      cdf <- iExtractComparisons(sum,conf,cdf,i,lev.names,num.lvls,conf.level)
      # prepare to fit model w/ new reference group
      g <- stats::relevel(g,lev.names[i+1])
    } # for if
  } # for i loop
  ## Control for multiple comparisons
  cdf <- iPerformControl(method,cdf)
  sdf <- iPerformControl(method,sdf)
  ## Reorder the slopes if asked to do so
  if (order.slopes) sdf <- sdf[order(sdf$slopes),]
  ## Rename the confidence interval columns
  names(cdf)[3:4] <- names(sdf)[3:4] <- iCILabel(conf.level)
  ## Create and return the results list
  res <- list(method=method,comparisons=cdf,slope=sdf,digits=digits)
  class(res) <- "compSlopes"
  res
}

#' @rdname compSlopes
#' @export
print.compSlopes <- function(x,...) { # nocov start
  message("Multiple Slope Comparisons (using the '",x$method,"' adjustment)")
  print(x$comparisons,digits=x$digits)
  message("\nSlope Information (using the '",x$method,"' adjustment)")
  print(x$slope,digits=x$digits)
} # nocov end


#' @title Tests for significant differences among all pairs of intercepts in a dummy variable regression.
#'
#' @description Tests for significant differences among all pairs of intercepts in a dummy variable regression where the dummy variables all stem from one factor.
#'
#' @details In a dummy variable regression without the interaction(s) between the covariate (x) and the dummy variable(s) (i.e., parallel lines) the coefficient for the dummy variables tests for a difference in intercepts between the level of the dummy variable and the reference level. Thus, all dummy variables from a particular linear model fit only compare intercepts with the reference level. Other intercept comparisons may be found by changing the reference level, which requires refitting the model.
#' 
#' Alternatively, Tukey's HSD method of multiple comparisons may be used, but this requires adjusting the original observations as if the original observations were all collected at the exact same value of the covariate (x). Because of this required adjustment, the  \code{\link[stats]{TukeyHSD}} function is inappropriate for testing for difference in intercepts in a dummy variable regression.
#'
#'This function provides a statistical comparison of all pairs of intercepts by first adjusting the observed data to a common value of the covariate (\code{common.cov}), computing a one-way ANOVA to determine if the mean adjusted values differ by level of the group factor in the original dummy variable regression, and then submitting the one-way ANOVA results to the \code{\link[stats]{TukeyHSD}} function to determine for which levels the mean adjusted values differ. The levels for which the mean adjusted values differ are also the levels for which the intercepts differ.
#'
#'The default is to compute the adjusted values at the mean value of the covariate (i.e., \code{common.cov=mean(x)}. However, if interest is in the intercepts (i.e., at X=0) then \code{common.cov=0} should be used instead.
#'
#' @param mdl A \code{lm} object.
#' @param common.cov A value to be used as the common value of the covariate in the adjustment process. See details.
#' @param conf.level A single number that represents the level of confidence to use for constructing confidence intervals.
#' @param digits A numeric that controls the number of digits to print.
#' @param x A \code{compIntercepts} object (i.e., returned from \code{compIntercepts}).
#' @param \dots Other arguments to be passed to the \code{TukeyHSD} or \code{print} functions.
#' 
#' @return A list with the following four components:
#' \tabular{ll}{
#' \code{comparison} \tab The comparison results as returned from \code{\link[stats]{TukeyHSD}}.\cr
#' \code{common.cov} \tab The value of the common covariate sent in \code{common.cov}.\cr
#' \code{adjvals} \tab A vector of values of the response variable adjusted to the \code{common.cov} value of the covariate. This vector can be appended to the original data frame to construct summary statistics for the adjusted values (e.g., mean adjusted value for each group).\cr 
#' \code{means} \tab A vector of mean adjusted values at the value of the common covariate.\cr 
#' \code{digits} \tab The value sent in \code{digits}.\cr 
#' \code{rnm} \tab The name of the response (LHS) variable.\cr
#' \code{cnm} \tab The name of the covariate variable.\cr
#' }
#'
#' The \code{print} function prints the comparison and adjusted means in a nice format.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @seealso \code{\link[stats]{TukeyHSD}} and \code{compSlopes} from \pkg{FSA}.
#' 
#' @aliases compIntercepts print.compIntercepts
#'
#' @keywords htest
#' 
#' @examples
#' ## Reduce Mirex data to years where slopes don't differ to illustrate this
#' ##   function ... see compSlopes() for analysis of full data set. 
#' Mirex <- Mirex[Mirex$year!="1996" & Mirex$year!="1999",]
#' Mirex$year <- factor(Mirex$year)
#' ## Fit DVR, see that slopes don't differ, 
#' ##   compare intercepts, visualize results
#' lm1 <- lm(mirex~weight*year,data=Mirex)
#' anova(lm1)
#' compIntercepts(lm1)
#' ## Fit model without interaction to avoid warning, but
#' ##   note that the compIntercepts() results are the same
#' lm2 <- lm(mirex~weight+year,data=Mirex)
#' compIntercepts(lm2)
#' fitPlot(lm1,legend="topleft")
#'
#' @rdname compIntercepts
#' @export
compIntercepts <- function(mdl,common.cov=mean(x),
                           conf.level=0.95,digits=getOption("digits"),...) {
  ## Perform some checks
  lmtype <- iTypeoflm(mdl)
  if (lmtype$type!="IVR") STOP("Function only works for dummy variable regressions.")
  if (lmtype$Enum>2) STOP("Function only works for dummy variable regressions\n with one factor and one covariate variable.")
  if (length(attr(stats::terms(mdl),"term.labels"))>2) WARN("Removed an interaction term from 'mdl' (i.e., assumed\n parallel lines) to test intercepts.\n")
  ## isolate model information
  y <- lmtype$mf[,lmtype$Rpos]
  x <- lmtype$mf[,lmtype$ENumPos]
  g <- lmtype$mf[,lmtype$EFactPos]
  if (length(levels(g))<3) WARN("Function not needed with fewer than three levels.")
  ## Fit model without an interaction term
  lm1 <- stats::lm(y~x+g)
  ## Construct the adjusted values
  adjvals <- stats::predict(lm1,data.frame(x=rep(common.cov,nrow(lmtype$mf)),g=g))+lm1$residuals
  ## Compure Tukey's adjustment on the adjusted values
  thsd <- stats::TukeyHSD(stats::aov(adjvals~g),...)
  ## Put results into a better data.frame
  cdf <- data.frame(comparison=rownames(thsd[[1]]),thsd[[1]])
  rownames(cdf) <- seq_len(nrow(cdf))
  # Rename the confidence interval columns
  names(cdf)[3:4] <- iCILabel(conf.level)
  ## Prepare results list to return
  res <- list(comparisons=cdf,common.cov=common.cov,adjvals=adjvals,
              means=tapply(adjvals,g,mean),digits=digits,
              rnm=lmtype$Rname,cnm=names(lmtype$mf)[lmtype$ENumPos])
  class(res) <- "compIntercepts"
  res
}

#' @rdname compIntercepts
#' @export
print.compIntercepts <- function(x,...) { # nocov start
  message("Tukey HSD on means adjusted assuming parallel lines")
  print(x$comparisons,digits=x$digits)
  message("\nMean ",x$rnm," when ",x$cnm,"=",formatC(x$common.cov,format="fg",digits=x$digits))
  print(x$means,digits=x$digits)
} # nocov end


# ============================================================
# Internal functions
# ============================================================
iPerformControl <- function(method,idf) {
  res <- stats::p.adjust(idf[,"p.unadj"],method)
  idf <- data.frame(idf,p.adj=round(res,5))
  invisible(idf)
}

iExtractSlopes <- function(sum,conf,isdf,i,conf.level) {
  isdf$slopes[i] <- sum$coefficients[2,1]
  isdf$p.unadj[i] <- sum$coefficients[2,4]
  isdf$lwr[i] <- conf[2,1]
  isdf$upr[i] <- conf[2,2]
  isdf[2:5] <- round(isdf[2:5],5)
  invisible(isdf)
}

iExtractComparisons <- function(sum,conf,icdf,i,lev.names,num.lvls,conf.level) {
  coeffs <- sum$coefficients[,1]
  pvals <- sum$coefficients[,4]
  comps <- (num.lvls-1):1
  # find position to start replacements
  pos <- ifelse(i==1,1,1+sum(comps[seq_len(i-1)]))
  for (j in (i+1):num.lvls) {
    # info on comparison of slopes w/ ref group
    icdf$comparison[pos] <- paste0(lev.names[j],"-",lev.names[i])
    icdf$diff[pos] <- coeffs[(length(pvals)-num.lvls+j)]
    icdf$p.unadj[pos] <- pvals[(length(coeffs)-num.lvls+j)]
    icdf$lwr[pos] <- conf[(length(pvals)-num.lvls+j),1]
    icdf$upr[pos] <- conf[(length(pvals)-num.lvls+j),2]
    icdf[2:5] <- round(icdf[2:5],5)
    pos <- pos+1
  } # for j    
  invisible(icdf)
}
