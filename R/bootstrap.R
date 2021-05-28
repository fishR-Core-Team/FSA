#' @name bootstrap
#' 
#' @title Associated S3 methods for bootstrap results from car::Boot.
#'
#' @description S3 methods are provided to construct non-parametric bootstrap confidence intervals, predictions with non-parametric confidence intervals, hypothesis tests, and plots of the parameter estimates for objects returned from \code{\link[car]{Boot}} from \pkg{car}.
#'
#' @details \code{confint} is largely a wrapper for \code{\link[car]{Confint}} from \pkg{car} (see its manual page).
#' 
#' \code{predict} applies a user-supplied function to each row of \code{object} and then finds the median and the two quantiles that have the proportion (1-\code{conf.level})/2 of the bootstrapped predictions below and above. The median is returned as the predicted value and the quantiles are returned as an approximate 100\code{conf.level}\% confidence interval for that prediction. Values for the independent variable in \code{FUN} must be a named argument sent in the \dots argument (see examples). Note that if other arguments are needed in \code{FUN} besides values for the independent variable, then these are included in the \dots argument AFTER the values for the independent variable.
#'
#' In \code{htest} the \dQuote{direction} of the alternative hypothesis is identified by a string in the \code{alt=} argument. The strings may be \code{"less"} for a \dQuote{less than} alternative, \code{"greater"} for a \dQuote{greater than} alternative, or \code{"two.sided"} for a \dQuote{not equals} alternative (the DEFAULT). In the one-tailed alternatives the p-value is the proportion of bootstrapped parameter estimates in \code{object$coefboot} that are extreme of the null hypothesized parameter value in \code{bo}. In the two-tailed alternative the p-value is twice the smallest of the proportion of bootstrapped parameter estimates above or below the null hypothesized parameter value in \code{bo}.
#'
#' @aliases boot confint.boot htest.boot hist.boot plot.boot predict.boot
#'
#' @param object,x An object of class \code{boot} from \code{\link[car]{Boot}}.
#' @param type Confidence interval type; types implemented are the "percentile" method, which uses the function quantile to return the appropriate quantiles for the confidence limit specified, the default bca which uses the bias-corrected and accelerated method presented by Efron and Tibshirani (1993, Chapter 14). For the other types, see the documentation for \code{\link[boot]{boot}}.
#' @param parm A number or string that indicates which column of \code{object} contains the parameter estimates to use for the confidence interval or hypothesis test.
#' @param conf.level A level of confidence as a proportion.
#' @param level Same as \code{conf.level}.
#' @param plot A logical that indicates whether a plot should be constructed. If \code{confint} then a histogram of the \code{parm} parameters from the bootstrap samples with error bars that illustrate the bootstrapped confidence intervals will be constructed. If code{htest} then a histogram of the \code{parm} parameters with a vertical line illustrating the \code{bo} value will be constructed.
#' @param err.col A single numeric or character that identifies the color for the error bars on the plot.
#' @param err.lwd A single numeric that identifies the line width for the error bars on the plot.
#' @param rows A single numeric that contains the number of rows to use on the graphic.
#' @param cols A single numeric that contains the number of columns to use on the graphic.
#' @param FUN The function to be applied for the prediction. See the examples.
#' @param digits A single numeric that indicates the number of digits for the result.
#' @param bo The null hypothesized parameter value.
#' @param alt A string that indicates the \dQuote{direction} of the alternative hypothesis. See details.
#' @param same.ylim A logical that indicates whether the same limits for the y-axis should be used on each histogram. Defaults to \code{TRUE}. Ignored if \code{ylmts} is non-null.
#' @param ymax A single value that sets the maximum y-axis limit for each histogram or a vector of length equal to the number of groups that sets the maximum y-axis limit for each histogram separately.
#' @param col A named color for the histogram bars.
#' @param \dots Additional items to send to functions. See details.
#'
#' @return If \code{object} is a matrix, then \code{confint} returns a matrix with as many rows as columns (i.e., parameter estimates) in \code{object} and two columns of the quantiles that correspond to the approximate confidence interval. If \code{object} is a vector, then \code{confint} returns a vector with the two quantiles that correspond to the approximate confidence interval.
#'
#' \code{htest} returns a two-column matrix with the first column containing the hypothesized value sent to this function and the second column containing the corresponding p-value.
#'
#' \code{hist} constructs histograms of the bootstrapped parameter estimates.
#'
#' \code{plot} constructs scatterplots of all pairs of bootstrapped parameter estimates.
#'
#' \code{predict} returns a matrix with one row and three columns, with the first column holding the predicted value (i.e., the median prediction) and the last two columns holding the approximate confidence interval.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso \code{\link[car]{Boot}} in \pkg{car}.
#'
#' @references S. Weisberg (2005). \emph{Applied Linear Regression}, third edition. New York: Wiley, Chapters 4 and 11.
#' 
#' @keywords htest
#' 
#' @examples
#' fnx <- function(days,B1,B2,B3) {
#'   if (length(B1) > 1) {
#'     B2 <- B1[2]
#'     B3 <- B1[3]
#'     B1 <- B1[1]
#'   }
#'   B1/(1+exp(B2+B3*days))
#' }
#' nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,
#'            start=list(B1=6,B2=7.2,B3=-1.45))
#' 
#' if (require(car)) {
#'   nl1.bootc <- car::Boot(nl1,f=coef,R=99)  # R=99 too few to be useful
#'   confint(nl1.bootc,"B1")
#'   confint(nl1.bootc,c(2,3))
#'   confint(nl1.bootc,conf.level=0.90)
#'   confint(nl1.bootc,"B1",plot=TRUE)
#'   htest(nl1.bootc,1,bo=6,alt="less")
#'   htest(nl1.bootc,1,bo=6,alt="less",plot=TRUE)
#'   predict(nl1.bootc,fnx,days=1:3)
#'   predict(nl1.bootc,fnx,days=3)
#'   hist(nl1.bootc)
#'   plot(nl1.bootc)
#'   cor(nl1.bootc$t,use="pairwise.complete.obs")
#' }
#' 
#' @rdname boot
#' @export
confint.boot <- function(object,parm=NULL,level=conf.level,conf.level=0.95,
                         type=c("bca","norm","basic","perc"),
                         plot=FALSE,err.col="black",err.lwd=2,
                         rows=NULL,cols=NULL,...) {
  type <- match.arg(type)
  iCheckConfLevel(conf.level)
  if (is.null(parm)) parm <- colnames(object$t)
  else {
    if (is.numeric(parm)) {
      # check numeric parm
      if (any(parm<0) & any(parm>0))
        STOP("Numbers in 'parm' cannot be both positive and negative.")
      if (max(abs(parm))>ncol(object$t))
        STOP("Number in 'parm' exceeds number of columns.")
    } else {
      # check named parm
      if (!all(parm %in% colnames(object$t)))
        STOP("Name in 'parm' does not exist in 'object'.")
    }
  }
  res <- car::Confint(object,parm=parm,level=conf.level,type=type)
  colnames(res)[2:3] <- iCILabel(conf.level)
  if (plot) {
    tmp <- object$t
    np <- ncol(tmp)
    if (is.null(rows)) rows <- round(sqrt(np))
    if (is.null(cols)) cols <- ceiling(sqrt(np))
    withr::local_par(list(mfrow=c(rows,cols)))
    for (i in seq_len(np)) {
      h <- hist.formula(~tmp[,i],xlab=colnames(tmp)[i],...)
      plotrix::plotCI(res[i,1],y=0.95*max(h$counts),
                      li=res[i,2],ui=res[i,3],err="x",
                      pch=19,col=err.col,lwd=err.lwd,add=TRUE)
    }
  }
  res
}

#' @rdname boot
#' @export
htest.boot <- function(object,parm=NULL,bo=0,
                       alt=c("two.sided","less","greater"),
                       plot=FALSE,...) {
  iHTestBoot(object$t,parm=parm,bo=bo,alt=alt,plot=plot)
}

#' @rdname boot
#' @export
predict.boot <- function(object,FUN,conf.level=0.95,digits=NULL,...) {
  iPredictBoot(object$t,FUN=FUN,MARGIN=1,conf.level=conf.level,
               digits=digits,...)
}

#' @rdname boot
#' @export
hist.boot <- function(x,same.ylim=TRUE,ymax=NULL,
                      rows=round(sqrt(ncol(x$t))),
                      cols=ceiling(sqrt(ncol(x$t))),...){ # nocov start
  ## Set graphing parameters
  withr::local_par(list(mfrow=c(rows,cols)))
	## If not given ymax, then find highest count on all histograms
  if (is.null(ymax)) {
    for (i in seq_len(ncol(x$t)))
      ymax[i] <- max(hist.formula(~x$t[,i],plot=FALSE,
                                  warn.unused=FALSE,...)$counts)
  }
  if (same.ylim) ymax <- rep(max(ymax),length(ymax))
	## Make the plots
	for(i in seq_len(ncol(x$t)))
	  hist.formula(~x$t[,i],xlab=colnames(x$t)[i],ylim=c(0,ymax[i]),...)
} # nocov end


#' @rdname boot
#' @export
plot.boot <- function(x,...){ #nocov start
	np <- ncol(x$t)
	lay <- lower.tri(matrix(0,(np-1),(np-1)), TRUE)
	lay[which(lay, TRUE)] <- seq_len(choose(np,2))
	graphics::layout(lay)
	for(i in seq_len((np-1)))
		for(j in (i+1):np)
		  graphics::plot(x$t[,i],x$t[,j],xlab=colnames(x$t)[i],
		                 ylab=colnames(x$t)[j],pch=20)
} #nocov end




#' @name nlsBoot
#' 
#' @title Associated S3 methods for nlsBoot from nlstools.
#'
#' @description Provides S3 methods to construct non-parametric bootstrap confidence intervals and hypothesis tests for parameter values and predicted values of the response variable for a \code{\link[nlstools]{nlsBoot}} object from the \pkg{nlstools} package.
#'
#' @details \code{confint} finds the two quantiles that have the proportion (1-\code{conf.level})/2 of the bootstrapped parameter estimates below and above. This is an approximate 100\code{conf.level}\% confidence interval.
#' 
#' In \code{htest} the \dQuote{direction} of the alternative hypothesis is identified by a string in the \code{alt=} argument. The strings may be \code{"less"} for a \dQuote{less than} alternative, \code{"greater"} for a \dQuote{greater than} alternative, or \code{"two.sided"} for a \dQuote{not equals} alternative (the DEFAULT). In the one-tailed alternatives the p-value is the proportion of bootstrapped parameter estimates in \code{object$coefboot} that are extreme of the null hypothesized parameter value in \code{bo}. In the two-tailed alternative the p-value is twice the smallest of the proportion of bootstrapped parameter estimates above or below the null hypothesized parameter value in \code{bo}.
#' 
#' In \code{predict}, a user-supplied function is applied to each row of the \code{coefBoot} object in a \code{nlsBoot} object and then finds the median and the two quantiles that have the proportion (1-\code{conf.level})/2 of the bootstrapped predictions below and above. The median is returned as the predicted value and the quantiles are returned as an approximate 100\code{conf.level}\% confidence interval for that prediction.
#'
#' @param object An object saved from \code{nlsBoot()}.
#' @param parm An integer that indicates which parameter to compute the confidence interval or hypothesis test for. The confidence interval Will be computed for all parameters if \code{NULL}.
#' @param conf.level A level of confidence as a proportion. 
#' @param level Same as \code{conf.level}. Used for compatibility with the main \code{confint}.
#' @param plot A logical that indicates whether a plot should be constructed. If \code{confint}, then a histogram of the \code{parm} parameters from the bootstrap samples with error bars that illustrate the bootstrapped confidence intervals will be constructed. If code{htest}, then a histogram of the \code{parm} parameters with a vertical lines illustrating the \code{bo}value will be constructed.
#' @param err.col A single numeric or character that identifies the color for the error bars on the plot.
#' @param err.lwd A single numeric that identifies the line width for the error bars on the plot.
#' @param rows A numeric that contains the number of rows to use on the graphic.
#' @param cols A numeric that contains the number of columns to use on the graphic.
#' @param FUN The function to be applied for the prediction. See the examples.
#' @param digits A single numeric that indicates the number of digits for the result.
#' @param bo The null hypothesized parameter value.
#' @param alt A string that identifies the \dQuote{direction} of the alternative hypothesis. See details.
#' @param \dots Additional arguments to functions.
#'
#' @return
#' \code{confint} returns a matrix with as many rows as columns (i.e., parameter estimates) in the \code{object$coefboot} data frame and two columns of the quantiles that correspond to the approximate confidence interval.
#' 
#' \code{htest} returns a matrix with two columns. The first column contains the hypothesized value sent to this function and the second column is the corresponding p-value.
#' 
#' \code{predict} returns a matrix with one row and three columns, with the first column holding the predicted value (i.e., the median prediction) and the last two columns holding the approximate confidence interval.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso \code{\link[car]{Boot}} and related methods in \pkg{car} and \code{summary.\link[nlstools]{nlsBoot}} in \pkg{nlstools}.
#'
#' @aliases confint.nlsboot htest.nlsboot predict.nlsbooot
#'
#' @keywords htest
#'
#' @examples
#' fnx <- function(days,B1,B2,B3) {
#'   if (length(B1) > 1) {
#'     B2 <- B1[2]
#'     B3 <- B1[3]
#'     B1 <- B1[1]
#'   }
#'   B1/(1+exp(B2+B3*days))
#' }
#' nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,
#'            start=list(B1=6,B2=7.2,B3=-1.45))
#' if (require(nlstools)) {
#'   nl1.bootn <-  nlstools::nlsBoot(nl1,niter=99) # too few to be useful
#'   confint(nl1.bootn,"B1")
#'   confint(nl1.bootn,c(2,3))
#'   confint(nl1.bootn,conf.level=0.90)
#'   confint(nl1.bootn,plot=TRUE)
#'   predict(nl1.bootn,fnx,days=3)
#'   predict(nl1.bootn,fnx,days=1:3)
#'   htest(nl1.bootn,1,bo=6,alt="less")
#' }
#' 
#' @rdname nlsBoot
#' @export
confint.nlsBoot <- function(object,parm=NULL,
                            level=conf.level,conf.level=0.95,
                            plot=FALSE,err.col="black",err.lwd=2,
                            rows=NULL,cols=NULL,...) {
  iCIBoot(object$coefboot,parm,conf.level,plot,err.col,err.lwd,rows,cols,...)
}

#' @rdname nlsBoot
#' @export
predict.nlsBoot <- function(object,FUN,conf.level=0.95,digits=NULL,...) {
  iPredictBoot(object$coefboot,FUN=FUN,MARGIN=1,
               conf.level=conf.level,digits=digits,...)
}

#' @rdname nlsBoot
#' @export
htest <- function(object, ...) {
  UseMethod("htest") 
}

#' @rdname nlsBoot
#' @export
htest.nlsBoot <- function(object,parm=NULL,bo=0,
                          alt=c("two.sided","less","greater"),
                          plot=FALSE,...) {
  iHTestBoot(object$coefboot,parm=parm,bo=bo,alt=alt,plot=plot)
}





##############################################################
## INTERNAL FUNCTIONS
##############################################################
## ===========================================================
## Confindence intervals from bootstrapped results
##   should work for bootCase and nlsboot results
## ===========================================================
iCIBoot <- function(object,parm,conf.level,plot,err.col,err.lwd,rows,cols,...) {
  #### internal function to find CIs
  cl <- function(x) stats::quantile(x,c((1-conf.level)/2,1-(1-conf.level)/2))
  #### end internal function
  #### Main function
  ## Perform some checks on parm
  # if parm=NULL then set to all paramaters
  if (is.null(parm)) parm <- colnames(object)
  else {
    if (is.numeric(parm)) {
      # check numeric parm
      if (any(parm<0) & any(parm>0))
        STOP("Numbers in 'parm' cannot be both positive and negative.")
      if (max(abs(parm))>ncol(object))
        STOP("Number in 'parm' exceeds number of columns.")
    } else {
      # check named parm
      if (!all(parm %in% colnames(object)))
        STOP("Name in 'parm' does not exist in 'object'.")
    }
  }
  
  ## Check on conf.level
  iCheckConfLevel(conf.level) 
  
  object <- object[,parm,drop=FALSE]
  ## Compute CIs for each column, but handle differently if vector or matrix
  res <- t(apply(object,2,cl))
  colnames(res) <- iCILabel(conf.level)
  rownames(res) <- colnames(object)
  ## Make plot if asked for
  # nocov start
  if (plot) {
    np <- ncol(object)
    if (is.null(rows)) rows <- round(sqrt(np))
    if (is.null(cols)) cols <- ceiling(sqrt(np))
    withr::local_par(list(mfrow=c(rows,cols)))
    for (i in seq_len(np)) {
      h <- hist.formula(~object[,i],xlab=colnames(object)[i],...)
      plotrix::plotCI(mean(object[,i]),y=0.95*max(h$counts),
                      li=res[i,1],ui=res[i,2],err="x",
                      pch=19,col=err.col,lwd=err.lwd,add=TRUE)
    }
  } # nocov end
  ## Return CI result
  res
}

## ===========================================================
## Predictions, with intervals, from bootstrapped results
##   should work for bootCase, Boot, and nlsboot results
## ===========================================================
iPredictBoot <- function(object,FUN,MARGIN,conf.level,digits,...) {
  ## Some checks
  if (!inherits(FUN,"function"))
    STOP("'FUN' is not a function.")
  
  ## Check on conf.level
  iCheckConfLevel(conf.level) 
  
  ## Get items in the dots
  tmp <- list(...)
  ## Prep the results matrix
  n <- length(tmp[[1]])
  res <- matrix(NA,nrow=n,ncol=4)
  ## Loop through the items in the dots variable
  for (i in seq_len(n)) {
    # set arguments for apply
    tmp1 <- c(tmp[[1]][i],unlist(tmp[-1]))
    names(tmp1) <- names(tmp)
    args <- c(list(X=object,MARGIN=MARGIN,FUN=FUN),tmp1)
    # get the bootstrap results for one set of values in the dots variable
    tmpres <- do.call(apply,args)
    # get median, LCI, and UCI and put in results matrix (with dots variable value)
    res[i,] <- c(tmp1[[1]],stats::quantile(tmpres,c(0.5,0.5-conf.level/2,
                                                    0.5+conf.level/2),
                                           na.rm=TRUE))
  }
  ## Potentially round the median and CI results
  if (!is.null(digits)) {
    if (digits<=0) STOP("'digits' must be positive.")
    res[,2:4] <- round(res[,2:4],digits)
  }
  colnames(res) <- c(names(tmp1)[1],"Median",iCILabel(conf.level))
  ## Return the matrix
  res
}

## ===========================================================
## Hypothesis testing from bootstrapped results
##   should work for bootCase, Boot, and nlsboot results
## ===========================================================
iHTestBoot <- function(object,parm,bo=0,
                       alt=c("two.sided","less","greater"),
                       plot=FALSE) {
  ## Some checks
  alt <- match.arg(alt)
  ## Multiple parm values in object, make sure a parm was selected
  ## if it was then reduce object to vector of that parm
  if (!is.null(dim(object))) {
    if (is.null(parm)) STOP("You must select a parameter to test with `parm`.")
    else {
      # check parm
      if (length(parm)>1) STOP("'parm' must be of length 1.")
      else {
        if (is.numeric(parm)) {
          # the column number was too small or too big
          if (parm>ncol(object))
            STOP("Number in 'parm' exceeds number of columns.")
          if (parm<=0) STOP("Number in 'parm' must be positive.")
        } else {
          # column name does not exist in the matrix
          if (!parm %in% colnames(object))
            STOP("Name in 'parm' does not exist in 'object'.")
        }
      }
    }
  }
  ## Calculate one-sided p-values
  tmp <- object[,parm]
  p.lt <- length(tmp[tmp>bo])/length(tmp)
  p.gt <- length(tmp[tmp<bo])/length(tmp)
  ## Calculate p-value based on choice in alt
  switch(alt,
         less=p.value <- p.lt,
         greater=p.value <- p.gt,
         two.sided=p.value <- 2*min(p.lt,p.gt)
  )
  ## Put together a result to return
  res <- cbind(bo,p.value)
  colnames(res) <- c("Ho Value","p value")
  rownames(res) <- ifelse(is.character(parm),parm,colnames(object)[parm])
  ## Make a plot if asked for
  # nocov start
  if (plot) {
    hist.formula(~object[,parm],xlab=rownames(res),main="")
    graphics::abline(v=bo,col="red",lwd=2,lty=2)
  } # nocov end
  ## Return the result
  res
}
