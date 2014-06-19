#' @title Computes the D test statistic for the two-sample two-dimensional Kolmogrov-Smirnov test.
#'
#' @description Computes the D test statistic for the two-sample two-dimensional Kolmogrov-Smirnov test.
#'
#' @details NEED DETAIL HERE.
#'
#' @note This function is experimental at best at this point.
#'
#' @aliases ks2d2 print.ks2d2 plot.ks2d2
#'
#' @param x1 Vector of X-coordinates for the first set of coordinates.
#' @param y1 Vector of Y-coordinates for the first set of coordinates.
#' @param x2 Vector of X-coordinates for the second set of coordinates.
#' @param y2 Vector of Y-coordinates for the second set of coordinates.
#' @param justD Logical that indicates whether just the D test statistic (\code{=TRUE}) or more information should be returned (default; see value section below).
#' @param KSp Logical that indicates whether the approximate p-value from the Kolmogorov-Smirnov distribution should be returned.  NOT YET IMPLEMENTED.
#' @param divbylen Logical that indicates whether the proportion of values in each quadrant is computed by dividing by the sample size (\code{=TRUE}, default) or by the number of points that could be assigned to quadrants.
#' @param x An object returned from \code{d2ks}.
#' @param pchs A vector of length two plotting characters to be used when plotting the two sets of coordinates.
#' @param cexs A vector of length two character expansion values to be used when plotting the two sets of coordinates.
#' @param xlab A string to label the x-axis.
#' @param ylab A string to label the y-axis.
#' @param xlim A vector of length two that indicates the limits over which to plot the x-axis.
#' @param ylim A vector of length two that indicates the limits over which to plot the y-axis.
#' @param \dots Additiona arguments sent to the plot function.
#'
#' @return The main function returns a single numeric of the D test statistic if \code{justD=TRUE} or a list with the following items if \code{justD=FALSE}:
#'  \itemize{
#'    \item D The D test statistic.  See details.
#'    \item pval The approximate p-value from the Kolmogorov-Smirnov distribution.  Returned only if \code{KSp=TRUE}.  NOT YET IMPLEMENTED.
#'    \item n1 Sample size of first set of coordinates.
#'    \item n2 Sample size of second set of coordinates.
#'    \item max1 Maximum D for first set of coordinates.
#'    \item max2 Maximum D for second set of coordinates.
#'    \item where1 Observation(s) where maximum D occurred for first set of coordinates.
#'    \item where2 Observation(s) where maximum D occurred for second set of coordinates.
#'    \item KSp Same logical supplied by user.
#'    \item divbylen Same logical supplied by user.
#'    \item x1 Same vector of X-coordinates for the first set of coordinates supplied by user.
#'    \item y1 Same vector of Y-coordinates for the first set of coordinates supplied by user.
#'    \item x2 Same vector of X-coordinates for the second set of coordinates supplied by user.
#'    \item y2 Same vector of Y-coordinates for the second set of coordinates supplied by user.
#'  }
#'
#'The \code{plot} function returns side-by-side plots showing the two sets of coordinates and the point or points that resulted in the maximum D value for each set of coordinates.  The \code{print} function prints results in a nice format if \code{justD=FALSE} (only D will be printed if \code{justD=TRUE}).
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, with significant help from Ben Bolker.
#'
#' @references
#' Garvey, J.E., E.A. Marschall, and R.A. Wright.  1998.  \href{http://opensiuc.lib.siu.edu/fiaq_pubs/17/}{From star charts to stoneflies: detecting relationships in continuous bivariate data.} Ecology 79:442 447.
#'
#' Press, W.H., S.A. Teukolsky, W.T. Vetterling, B.P. Flannery.  2007.  \href{http://www.nr.com/}{Numerical Recipes: The Art of Scientific Computing, 3rd Edition.}  Cambridge University Press.  1286 pages. 
#'
#' @seealso \code{\link{ks2d1}} and \code{\link{ks2d2p}}
#'
#' @keywords htest
#'
#' @examples
#' data(KS2D_NR)
#'
#' # separate into the two sets of coordinates
#' d1 <- subset(KS2D_NR,group=="triangles")
#' d2 <- subset(KS2D_NR,group=="squares")
#'
#' # perform analysis
#' ( res1 <- ks2d2(d1$x,d1$y,d2$x,d2$y) )
#' plot(res1,xlim=c(-3,3),ylim=c(-3,3))
#'
#' # perform same analysis with modified computation of proportions
#' ( res2 <- ks2d2(d1$x,d1$y,d2$x,d2$y,divbylen=FALSE) )
#'
#' @rdname ks2d2
#' @export
ks2d2 <- function(x1,y1,x2,y2,justD=FALSE,KSp=FALSE,divbylen=TRUE) {
    ## INTERNAL -- compute differences
    dffrnc <- function(d,x1,y1,x2,y2,divbylen) {
      q1 <- iquad_dens(d,x1,y1,divbylen)
      q2 <- iquad_dens(d,x2,y2,divbylen)
      max(abs(q1 - q2))
    }  ## end internal dffrnc

  # find differences using points in first, then second sample, as origins
  diff1 <- apply(cbind(x1,y1),1,dffrnc,x1,y1,x2,y2,divbylen)
  diff2 <- apply(cbind(x2,y2),1,dffrnc,x1,y1,x2,y2,divbylen)
  # find the maximum absolute difference in each sample
  max1 <- max(diff1)
  max2 <- max(diff2)
  # compute test statistic and p-value  
  D <- mean(c(max1,max2))
  # if only D is desired then return it, otherwise compute some more
  if (justD) { D }    
  else {
    # find where maximums first occurred  
    where1 <- unique(which(round(diff1,6)==round(max1,6),arr.ind=TRUE))
    where2 <- unique(which(round(diff2,6)==round(max2,6),arr.ind=TRUE))
    # get two sample sizes
    n1 <- length(x1)
    n2 <- length(x2)
    # if p-value is not desired then return all from above, otherwise find it & return everything
    if (!KSp) { res <- list(D=D,n1=n1,n2=n2,max1=max1,max2=max2,where1=where1,where2=where2,divbylen=divbylen,KSp=KSp,x1=x1,y1=y1,x2=x2,y2=y2) }
    else {
      pval <- 1-ipkolmogorov1x(D,n1*n2/(n1+n2))
      res <- list(D=D,pval=pval,n1=n1,n2=n2,max1=max1,max2=max2,where1=where1,where2=where2,divbylen=divbylen,KSp=KSp,x1=x1,y1=y1,x2=x2,y2=y2)
    }
    class(res) <- "ks2d2"
    res
  }
}

#' @rdname ks2d2
#' @method print ks2d2
#' @export
print.ks2d2 <- function(x,...) {
  cat("Two Dimensional Kolmogorov-Smirnov Test - THESE RESULTS ARE EXPERIMENTAL AT THIS POINT!!!\n\n")
  cat("Maximum for first coordinate set as origins was",formatC(x[["max1"]],format="f",digits=4),"for observation #",x[["where1"]],"\n")
  cat("Maximum for second coordinate set as origins was",formatC(x[["max2"]],format="f",digits=4),"for observation #",x[["where2"]],"\n")
  txt <- paste("\nResulting in a test statistic (D) of",formatC(x[["D"]],format="f",digits=4))
  if (x[["KSp"]]) txt <- paste(txt,",with a K-S p-value of ",formatC(x[["pval"]],format="f",digits=4),sep="")
  cat(paste(txt,"\n"))
}

#' @rdname ks2d2
#' @export
plot.ks2d2 <- function(x,pchs=c(2,19),cexs=c(1.25,1),xlab=NULL,ylab=NULL,xlim=range(c(x$x1,x$x2)),ylim=range(c(x$y1,x$y2)),...) {
  if (is.null(xlab)) xlab <- deparse(substitute(x$x1))
  if (is.null(ylab)) ylab <- deparse(substitute(x$y1))
  op <- par(mfrow=c(1,2))
  plot(x$y1~x$x1,pch=pchs[1],cex=cexs[1],xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,main="First Set as Origins",...)
  points(x$y2~x$x2,pch=pchs[2],cex=cexs[2])
  abline(v=x$x1[x[["where1"]]],lty=3,col=rainbow(length(x[["where1"]])))
  abline(h=x$y1[x[["where1"]]],lty=3,col=rainbow(length(x[["where1"]])))
  plot(x$y1~x$x1,pch=pchs[1],cex=cexs[1],xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,main="Second Set as Origins",...)
  points(x$y2~x$x2,pch=pchs[2],cex=cexs[2])
  abline(v=x$x2[x[["where2"]]],lty=3,col=rainbow(length(x[["where2"]])))
  abline(h=x$y2[x[["where2"]]],lty=3,col=rainbow(length(x[["where2"]])))  
  par(op)
}
