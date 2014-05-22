#'A permutation test to compute a p-value for the D test statistic for the one-sample two-dimensional Kolmogrov-Smirnov test.
#'
#'A permutation test to compute a p-value for the D test statistic for the one-sample two-dimensional Kolmogrov-Smirnov test.
#'
#'NEED DETAIL HERE.
#'
#' @aliases ks2d1p print.ks2d1p plot.ks2d1p
#'
#' @param object An object returned from \code{ks2d2}.
#' @param x An object returned from \code{ks2d2p}.
#' @param B A numeric representing the number of resamples.
#' @param xlab A string to label the x-axis.
#' @param main A string to label the main title on the plot.
#' @param \dots Additional arguments sent to the plot function.
#'
#' @return The main function returns a list with the following items:
#'\itemize{
#'\item D The D test statistic from the \code{ks2d2} object.
#'\item pval The p-value from the permutation test.  See details.
#'\item Ds The D test statistics from each of the B \sQuote{resamples}.
#'\item B The \code{B} value supplied by the user.
#'}
#'The \code{plot} function returns a density plot of the D test statistics from each of the B \sQuote{resamples} with the observed D test statistic shown with a vertical line.  The \code{print} function prints the results in a nice format).
#'
#' @note This function is experimental at best at this point.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, with significant help from Ben Bolker.
#'
#' @seealso \code{\link{ks2d1}}
#'
#' @references Garvey, J.E., E.A. Marschall, and R.A. Wright.  1998.  From star charts to stoneflies: detecting relationships in continuous bivariate data. Ecology 79:442 447.  \url{http://opensiuc.lib.siu.edu/fiaq_pubs/17/}
#'
#'Press, W.H., S.A. Teukolsky, W.T. Vetterling, B.P. Flannery.  2007. Numerical Recipes: The Art of Scientific Computing, 3rd Edition.  Cambridge University Press.  1286 pages. \url{http://www.nr.com/}
#'
#' @keywords htest
#'
#' @examples
#'data(Garvey4a)
#'
#'# Results from the main 1-sample 2-D KS test from ks2d1
#'( res1 <- with(Garvey4a,ks2d1(shad,sunfish)) )
#'plot(res1,xlab="Shad Densitiy",ylab="Sunfish Density")
#'
#'# permutation test using resampling
#'( res1p <- ks2d1p(res1,B=10) )  # B should be >1000, used 10 here to save time
#'plot(res1p)
#'
#' @rdname ks2d1p
#' @export
ks2d1p <- function(object,B=100) {
  ## INTERNAL
  resampD <- function(object) {
    # randomize the x's
    x1 <- object[["x1"]][sample(object[["n1"]],replace=FALSE)]
    # randomize the y's ... not sure this is necessary
    y1 <- object[["y1"]][sample(object[["n1"]],replace=FALSE)]
    # send selected points to ks2d1 to compute D
    ks2d1(x1,y1,justD=TRUE,divbylen=object[["divbylen"]])
  } # end internal resampD
  
  # resample D B times
  Dstat <- replicate(B,resampD(object))
  # p-value is proportion of resamples with larger D
  p <- sum(Dstat>object[["D"]])/B
  res <- list(D=object[["D"]],pval=p,Ds=Dstat,B=B)
  class(res) <- "ks2d1p"
  res
}

#' @rdname ks2d1p
#' @method print ks2d1p
#' @export
print.ks2d1p <- function(x,...) {
  cat("One-Sample Two-Dimensional Kolmogorov-Smirnov Test p-value - THESE RESULTS ARE EXPERIMENTAL AT THIS POINT!!!\n")
  cat("  Used 'resample' method. B=",x[["B"]],"times\n")
  cat("D=",formatC(x[["D"]],format="f",digits=4),", p-value =",formatC(x[["pval"]],format="g"),"\n")
}

#' @rdname ks2d1p
#' @export
plot.ks2d1p <- function(x,xlab="D Test Statistic",main="",...) {
  plot(density(x$Ds),xlab=xlab,main=main,xlim=range(c(x$D,x$Ds)))
  abline(v=x$D,col="red",lty=3)
  axis(3,at=x$D,labels="D",col="red")
}
