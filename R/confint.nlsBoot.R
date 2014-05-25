#'Construct a confidence interval from nlsBoot results.
#'
#'Constructs a non-parametric bootstrap confidence interval from an object of \code{nlsBoot()} (in the \pkg{nlstools} package) results.
#'
#'This function finds the two quantiles that have the proportion (1-\code{conf.level})/2 of the bootstrapped parameter estimates below and above.  This is an approximate 100\code{conf.level}\% confidence interval.
#'
#' @param object An object saved from \code{nlsBoot()}.
#' @param parm An integer that indicates which parameter to compute the confidence interval for.  Will compute for all parameters if \code{NULL}.
#' @param conf.level A level of confidence as a proportion. 
#' @param level Same as \code{conf.level}.  Used for compatability with the main \code{confint}.
#' @param plot A logical that indicates whether a histogram of the \code{parm} parameters from the bootstrap samples with error bars that illustrate the bootstrapped confidence intervals should be constructed.
#' @param rows A numeric that contains the number of rows to use on the graphic.
#' @param cols A numeric that contains the number of columns to use on the graphic.
#' @param \dots Additional arguments for methods.
#'
#' @return A matrix with as many rows as columns (i.e., parameter estimates) in the \code{object$coefboot} data frame and two columns of the quantiles that correspond to the approximate confidence interval.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{predict.nlsBoot}}, \code{summary.nlsBoot} in \pkg{nlstools}
#'
#' @aliases confint.nlsboot
#'
#' @keywords htest
#'
#' @examples
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#'
#' require(nlstools)
#' example(nlsBoot)
#' confint(boo,conf.level=0.90)
#' confint(boo,conf.level=0.90,plot=TRUE)
#'
#' } ## END IF INTERACTIVE MODE
#'
#' @rdname confint.nlsBoot
#' @export
confint.nlsBoot <- function(object,parm=NULL,level=0.95,conf.level=level,plot=FALSE,rows=round(sqrt(ncol(object$coefboot))),cols=ceiling(sqrt(ncol(object$coefboot))),...) {
  # internal function to find CIs
  cl <- function(x) quantile(x,c((1-conf.level)/2,1-(1-conf.level)/2))
  # end internal function
  # begin main function
  # determine if just a vector (one variable) was sent
  if (is.null(dim(object$coefboot))) {
    res <- cl(object$coefboot)
    names(res) <- ciLabel(conf.level)
  } else {
    # if a matrix (>1 variable) was sent then find all CIs
    res <- t(apply(object$coefboot,2,cl))
    colnames(res) <- ciLabel(conf.level)
  }
  # perform some checks on parm argument
  if (!is.null(parm)) {
    # was a numeric column name given
    if (is.numeric(parm)) {
      # was that number too big
      if (parm>ncol(object$coefboot)) {
        warning("Parameter number greater than number of parameters.  Parameter number ignored.",call.=FALSE)
        # ignore what was sent if it is too big
        parm <- NULL
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
      op <- par(mfrow=c(rows,cols),mar=c(3.5,3.5,1,1),mgp=c(2,0.75,0))
      np <- ncol(object$coefboot)
      for (i in 1:np) {
        h <- hist(object$coefboot[,i],xlab=colnames(object$coefboot)[i],main="")
        plotCI(object$bootCI[i,1],y=0.95*max(h$counts),li=res[i,1],ui=res[i,2],err="x",lwd=2,pch=19,col="red",add=TRUE)
      }
    } else {
      op <- par(mar=c(3.5,3.5,1,1),mgp=c(2,0.75,0))
      h <- hist(object$coefboot[,parm],xlab=parm,main="")
      plotCI(object$bootCI[parm,1],y=0.95*max(h$counts),li=res[1],ui=res[2],err="x",lwd=2,pch=19,col="red",add=TRUE)
    }
  }
  res
}
