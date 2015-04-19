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
#' require(car)    # for bootCase()
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
#' nl1.boot <- bootCase(nl1,B=99)  # B=99 too small to be useful
#' confint(nl1.boot,"B1")
#' confint(nl1.boot,c(2,3))
#' confint(nl1.boot,conf.level=0.90)
#' htest(nl1.boot,1,bo=6,alt="less")
#' hist(nl1.boot)
#' plot(nl1.boot)
#' cor(nl1.boot)
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
