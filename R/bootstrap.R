#' @name bootstrap
#' 
#' @title Associated S3 methods for bootCase from car.
#'
#' @description Provides S3 methods to construct non-parametric bootstrap confidence intervals, hypothesis tests, and plots of the parameter estimates for a \code{\link[car]{bootCase}} object (from the \pkg{car} package).
#'
#' @details \code{confint} finds the two quantiles that have the (1-\code{conf.level})/2 proportion of bootstrapped parameter estimates below and above.  This is an approximate 100\code{conf.level}\% confidence interval.
#'
#' In the one-tailed alternatives, the p-value is the proportion of bootstrapped parameter estimates in that are extreme of the null hypothesized parameter value (\code{bo}).  In the two-tailed alternative, the p-value is twice the smallest of the proportion of bootstrapped parameter \code{bo}.
#'
#' @aliases confint.bootCase htest.bootCase hist.bootCase plot.bootCase predict.bootCase
#'
#' @param object A \code{bootCase} matrix object for \code{confint} or \code{htest}.
#' @param x A \code{bootCase} object for \code{hist}, \code{plot} or \code{boxplot}.
#' @param parm A number that indicates which column of \code{object} contains the parameter estimates to use in the hypothesis test.
#' @param conf.level A level of confidence as a proportion.
#' @param level Same as \code{conf.level}.
#' @param bo The null hypothesized parameter value.
#' @param alt A string that indicates the \dQuote{direction} of the alternative hypothesis.  See details.
#' @param same.ylim A logicial that indicates whether the same limits for the y-axis should be used on each histogram.  Defaults to \code{TRUE}.  Ignored if \code{ylmts} is non-null.
#' @param ymax A single value that sets the maximum y-axis limit for each histogram or a vector of length equal to the number of groups that sets the maximum y-axis limit for each histogram separately.
#' @param col A named color for the histogram bars.
#' @param nr A numeric that contains the number of rows to use on the graphic.
#' @param nc A numeric that contains the number of columns to use on the graphic.
#' @param right A logical that indicates if the histogram bins are right-closed (left open) intervals (\code{=TRUE}) or not (\code{=FALSE}; default).
#' @param FUN The function to be applied.
#' @param MARGIN A single numeric that indicates the margin over which to apply the function.  \code{MARGIN=1} will apply to each row and is the default.
#' @param digits A single numeric that indicates the number of digits for the result.
#' @param \dots Additional items to send to functions.
#'
#' @return \code{confint} will return a matrix with as many rows as columns (i.e., parameter estimates) in \code{object} and two columns of the quantiles that correspond to the approximate confidence interval if \code{object} is a matrix.  If \code{object} is a vector then \code{confint} will return a vector with the two quantiles that correspond to the approximate confidence interval.
#'
#' \code{htest} returns a two-column matrix with the first column containing the hypothesized value sent to this function and the second column containing the corresponding p-value.
#'
#' \code{hist} constructs histograms of the bootstrapped parameter estimates.
#'
#' \code{plot} constructs scatterplots of all pairs of bootstrapped parameter estimates.
#'
#' \code{predict} applies a user-supplied function to each row of \code{object} and then finds the median and the two quantiles that have the proportion (1-\code{conf.level})/2 of the bootstrapped predictions below and above.  The median is returned as the predicted value and the quantiles are returned as an approximate 100\code{conf.level}\% confidence interval for that prediction.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link[car]{bootCase}} in \pkg{car}.
#'
#' @references S. Weisberg (2005). \emph{Applied Linear Regression}, third edition.  New York: Wiley, Chapters 4 and 11.
#' 
#' @keywords htest
#' 
#' @examples
#' data(Ecoli)
#' fnx <- function(days,B1,B2,B3) {
#'   if (length(B1) > 1) {
#'     B2 <- B1[2]
#'     B3 <- B1[3]
#'     B1 <- B1[1]
#'   }
#'   B1/(1+exp(B2+B3*days))
#' }
#' nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,start=list(B1=6,B2=7.2,B3=-1.45))
#' if (require(car)) {    # for bootCase()
#'   nl1.boot <- bootCase(nl1,B=99)  # B=99 too small to be useful
#'   confint(nl1.boot,"B1")
#'   confint(nl1.boot,c(2,3))
#'   confint(nl1.boot,conf.level=0.90)
#'   htest(nl1.boot,1,bo=6,alt="less")
#'   hist(nl1.boot)
#'   plot(nl1.boot)
#'   cor(nl1.boot)
#' }
#'
#' @rdname bootCase
#' @export
confint.bootCase <- function(object,parm=NULL,level=conf.level,conf.level=0.95,...) {
  cl <- function(x) quantile(x,c((1-conf.level)/2,1-(1-conf.level)/2))
  if (is.null(dim(object))) {             # if a vector, then only one parameter
    res <- cl(object)
    names(res) <- iCILabel(conf.level)
  } else {
    if (!is.null(parm)) object <- object[,parm]   # reduce to only parameters asked for
    if (is.null(dim(object))) {                   # if a vector, then only one parameter
      res <- cl(object)
      names(res) <- iCILabel(conf.level)
    } else {
      res <- t(apply(object,2,cl))
      colnames(res) <- iCILabel(conf.level)
      rownames(res) <- colnames(object)
    }
  }
  res
}

#' @rdname bootCase
#' @export
htest.bootCase <- function(object,parm=NULL,bo=0,alt=c("two.sided","less","greater"),...) {
  alt <- match.arg(alt)
  if (is.null(parm)) {
    if (!is.null(dim(object))) { stop("You must select a parameter number to test.\n",call.=FALSE) }
      else { obj.var <- object }
  } else {
    if (is.null(dim(object))) {
        warning("You entered a parameter number to select but only a vector was sent.\n  The selected parameter number will be ignored.\n",call.=FALSE)
        obj.var <- object
    } else { obj.var <- object[,parm] }
  }
  p.lt <- length(obj.var[obj.var>bo])/length(obj.var)
  p.gt <- length(obj.var[obj.var<bo])/length(obj.var)
  switch(alt,
    less=p.value <- p.lt,
    greater=p.value <- p.gt,
    two.sided=p.value <- 2*min(p.lt,p.gt)
  )
  res <- cbind(bo,p.value)
  colnames(res) <- c("Ho Value","p value")
  rownames(res) <- ""
  res
}

#' @rdname bootCase
#' @export
hist.bootCase <- function(x,same.ylim=TRUE,ymax=NULL,col="gray90",nr=round(sqrt(ncol(x))),nc=ceiling(sqrt(ncol(x))),right=FALSE,...){
	opar <- par(mfrow=c(nr,nc),mar=c(3.5,3.5,2,1),mgp=c(2,0.75,0))
  if (is.null(ymax)) {
    for (i in 1:ncol(x)) {  # used to find highest count on all histograms
      ymax[i] <- max(hist(x[,i],right=right,plot=FALSE,warn.unused=FALSE,...)$counts)
    }
  }
  if (same.ylim) { ymax <- rep(max(ymax),length(ymax)) }
	for(i in 1:ncol(x)){ hist(x[,i],xlab=colnames(x)[i],col=col,main="",right=right,ylim=c(0,ymax[i]),...) }
  par(opar)
}

#' @rdname bootCase
#' @export
plot.bootCase <- function(x,nr=round(sqrt(np)),nc=ceiling(sqrt(np)),...){
	np <- ncol(x)
	lay <- lower.tri(matrix(0,(np-1),(np-1)), TRUE)
	lay[which(lay, TRUE)] <- 1:choose(np,2)
	layout(lay)
	opar <- par(mar=c(3.5,3.5,1,1),mgp=c(2,0.75,0))
	for(i in 1:(np-1))
		for(j in (i+1):np)
			 plot(x[,i],x[,j],xlab=colnames(x)[i],ylab=colnames(x)[j],pch=20)
	par(opar)
}

#' @rdname bootCase
#' @export
predict.bootCase <- function(object,FUN,MARGIN=1,conf.level=0.95,digits=NULL,...) {
  res <- quantile(apply(object,MARGIN=MARGIN,FUN=FUN,...),c(0.5,(1-conf.level)/2,1-(1-conf.level)/2))
  if (!is.null(digits)) res <- round(res,digits)
  names(res) <- c("prediction",iCILabel(conf.level))
  res
}





#' @title Associated S3 methods for nlsBoot from nlstools.
#'
#' @description Provides S3 methods to construct non-parametric bootstrap confidence intervals and hypothesis tests for parameter values and predicted values of the response variable for a \code{\link[nlstools]{nlsBoot}} object (from the \pkg{nlstools} package).
#'
#' @details \code{confint} finds the two quantiles that have the proportion (1-\code{conf.level})/2 of the bootstrapped parameter estimates below and above.  This is an approximate 100\code{conf.level}\% confidence interval.
#' 
#' In \code{htest} the \dQuote{direction} of the alternative hypothesis is identified by a string in the \code{alt=} argument.  The strings may be \code{"less"} for a \dQuote{less than} alternative, \code{"greater"} for a \dQuote{greater than} alternative, or \code{"two.sided"} for a \dQuote{not equals} alternative (the DEFAULT).  In the one-tailed alternatives the p-value is the proportion of bootstrapped parameter estimates in \code{object$coefboot} that are extreme of the null hypothesized parameter value in \code{bo}.  In the two-tailed alternative the p-value is twice the smallest of the proportion of bootstrapped parameter estimates above or below the null hypothesized parameter value in \code{bo}.
#' 
#' In \code{predict}, a user-supplied function is applied to each row of the \code{coefBoot} object in a \code{nlsBoot} object and then finds the median and the two quantiles that have the proportion (1-\code{conf.level})/2 of the bootstrapped predictions below and above.  The median is returned as the predicted value and the quantiles are returned as an approximate 100\code{conf.level}\% confidence interval for that prediction.
#'
#' @param object An object saved from \code{nlsBoot()}.
#' @param parm An integer that indicates which parameter to compute the confidence interval for.  Will compute for all parameters if \code{NULL}.
#' @param conf.level A level of confidence as a proportion. 
#' @param level Same as \code{conf.level}.  Used for compatability with the main \code{confint}.
#' @param plot A logical that indicates whether a plot should be consturcted.  If \code{confint} then a histogram of the \code{parm} parameters from the bootstrap samples with error bars that illustrate the bootstrapped confidence intervals should be constructed.  If code{htest} then a histogram of the \code{parm} parameters with a vertical lines illustrating the \code{bo}value should be constructed.
#' @param err.col A single numeric or character that identifies the color for the error bar on the plot.
#' @param err.lwd A single numeric that identifies the line width for the error bar on the plot.
#' @param rows A numeric that contains the number of rows to use on the graphic.
#' @param cols A numeric that contains the number of columns to use on the graphic.
#' @param bo The null hypothesized parameter value.
#' @param alt A string that identifies the \dQuote{direction} of the alternative hypothesis.  See details.
#' @param FUN The function to be applied.
#' @param MARGIN A single numeric that indicates the margin over which to apply the function. \code{MARGIN=1} will apply to each row and is the default.
#' @param digits A single numeric that indicates the number of digits for the result.
#' @param \dots Additional arguments to functions.
#'
#' @return
#' \code{confint} returns a matrix with as many rows as columns (i.e., parameter estimates) in the \code{object$coefboot} data frame and two columns of the quantiles that correspond to the approximate confidence interval.
#' 
#' \code{htest} returns a matrix with two columns.  The first column contains the hypothesized value sent to this function and the second column is the corresponding p-value.
#' 
#' \code{predict} returns a matrix with one row and three columns with the first column holding the predicted value (i.e., the median prediction) and the last two columns holding the approximate confidence interval.
#' 
#' \code{predict}
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link[nlstools]{summary.nlsBoot}} in \pkg{nlstools}
#'
#' @aliases confint.nlsboot
#'
#' @keywords htest
#'
#' @examples
#' data(Ecoli)
#' fnx <- function(days,B1,B2,B3) {
#'   if (length(B1) > 1) {
#'     B2 <- B1[2]
#'     B3 <- B1[3]
#'     B1 <- B1[1]
#'   }
#'   B1/(1+exp(B2+B3*days))
#' }
#' nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,start=list(B1=6,B2=7.2,B3=-1.45))
#' if (require(nlstools)) {
#'   nl1.boot <- nlsBoot(nl1,niter=99)  # way too few
#'   confint(nl1.boot,"B1")
#'   confint(nl1.boot,c(2,3))
#'   confint(nl1.boot,conf.level=0.90)
#'   htest(nl1.boot,1,bo=6,alt="less")
#'   predict(nl1.boot,fnx,days=3)
#' }
#' 
#' @rdname nlsBoot
#' @export
confint.nlsBoot <- function(object,parm=NULL,level=0.95,conf.level=level,
                            plot=FALSE,err.col="black",err.lwd=2,
                            rows=round(sqrt(ncol(object$coefboot))),cols=ceiling(sqrt(ncol(object$coefboot))),...) {
  # internal function to find CIs
  cl <- function(x) quantile(x,c((1-conf.level)/2,1-(1-conf.level)/2))
  # end internal function
  # begin main function
  # determine if just a vector (one variable) was sent
  if (is.null(dim(object$coefboot))) {
    res <- cl(object$coefboot)
    names(res) <- iCILabel(conf.level)
  } else {
    # if a matrix (>1 variable) was sent then find all CIs
    res <- t(apply(object$coefboot,2,cl))
    colnames(res) <- iCILabel(conf.level)
  }
  # perform some checks on parm argument
  if (!is.null(parm)) {
    # was a numeric column name given
    if (is.numeric(parm)) {
      # was that number too big
      if (max(parm)>ncol(object$coefboot)) {
        warning("A 'parm' number was greater than the number of parameters in the model.\n  That number was ignored to continue.",call.=FALSE)
        # ignore what was sent if it is too big
        parm <- parm[which(parm<=ncol(object$coefboot))]
      }
    } else { # a named variable was given
      # does it exist in the matrix
      if (!(parm %in% colnames(object$coefboot))) {
        warning("Parameter name does not exist if nlsBoot results.  Parameter name ignored.",call.=FALSE)
        # ignore what was sent if it does not
        parm <- NULL
      }
    }
    # reduce results to just parm if parm is legitimate
    if (!is.null(parm)) res <- res[parm,]
  }
  
  if (plot) {
    if (is.null(parm)) {
      op <- par(mfrow=c(rows,cols))
      np <- ncol(object$coefboot)
      for (i in 1:np) {
        h <- hist(~object$coefboot[,i],xlab=colnames(object$coefboot)[i],...)
        plotCI(object$bootCI[i,1],y=0.95*max(h$counts),li=res[i,1],ui=res[i,2],err="x",pch=19,col=err.col,lwd=err.lwd,add=TRUE)
      }
    } else {
      h <- hist(~object$coefboot[,parm],xlab=parm,main="")
      plotCI(object$bootCI[parm,1],y=0.95*max(h$counts),li=res[1],ui=res[2],err="x",pch=19,col=err.col,lwd=err.lwd,add=TRUE,...)
    }
  }
  res
}

#' @rdname nlsBoot
#' @export
htest <- function(object, ...) {
  UseMethod("htest") 
}

#' @rdname nlsBoot
#' @export
htest.nlsBoot <- function(object,parm=NULL,bo=0,alt=c("two.sided","less","greater"),plot=FALSE,...) {
  alt <- match.arg(alt)
  # check if result is a vector -- i.e., only one parameter
  if (class(object$coefboot) != "matrix") {
    if (!is.null(parm)) warning("Results have only one dimension, value in 'parm' will be ignored.",call.=FALSE)
    # set dat equal to that vector
    dat <- object$coefboot
  } else {
    # check if more than one parm was sent
    if (length(parm)>1) {
      # only use first
      parm <- parm[1]
      warning("You can only test one paramater at a time.  Only the first will be used.",call.=FALSE)
    }
    # was a numeric column name given
    if (is.numeric(parm)) {
      # the column number was too big
      if (parm>ncol(object$coefboot)) stop("'parm' number greater than number of parameters.",call.=FALSE)
    } else { # a named variable was given
      # column name does not exist in the matrix
      if (!(parm %in% colnames(object$coefboot))) stop("'parm' name does not exist in nlsBoot results.",call.=FALSE)
    }
    # set dat equal to one column of matrix of results
    dat <- object$coefboot[,parm]
  }
  p.lt <- length(dat[dat>bo])/length(dat)
  p.gt <- length(dat[dat<bo])/length(dat)
  switch(alt,
         less=p.value <- p.lt,
         greater=p.value <- p.gt,
         two.sided=p.value <- 2*min(p.lt,p.gt)
  )
  res <- cbind(bo,p.value)
  colnames(res) <- c("Ho Value","p value")
  rownames(res) <- ""
  
  if (plot) {
    op <- par(mar=c(3.5,3.5,1,1),mgp=c(2,0.75,0))
    hist(object$coefboot[,parm],xlab=colnames(object$coefboot)[parm],main="")
    abline(v=bo,col="red",lwd=2,lty=2)
  }
  res
}  

#' @rdname nlsBoot
#' @export
predict.nlsBoot <- function(object,FUN,MARGIN=1,conf.level=0.95,digits=NULL,...) {
  res <- quantile(apply(object$coefboot,MARGIN=MARGIN,FUN=FUN,...),c(0.5,(1-conf.level)/2,1-(1-conf.level)/2))
  if (!is.null(digits)) res <- round(res,digits)
  names(res) <- c("prediction",iCILabel(conf.level))
  res
}
