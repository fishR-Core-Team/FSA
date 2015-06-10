#' @title Computes the D test statistic for the one-sample two-dimensional Kolmogrov-Smirnov test.
#'
#' @description Computes the D test statistic for the one-sample two-dimensional Kolmogrov-Smirnov test.
#'
#' @details NEED DETAIL HERE.
#'
#' @note This function is experimental at best at this point.
#'
#' @aliases ks2d1 print.ks2d1 plot.ks2d1
#' 
#' @param x1 Vector of X values.
#' @param y1 Vector of Y values.
#' @param justD Logical that indicates whether just the D test statistic (\code{=TRUE}) or more information should be returned (default; see value section below).
#' @param KSp Logical that indicates whether the approximate p-value from the Kolmogorov-Smirnov distribution should be returned.  HIGHLY EXPERIMENTAL (NOT YET TESTED).
#' @param divbylen Logical that indicates whether the proportion of values in each quadrant is computed by dividing by the sample size (\code{=TRUE}, default) or by the number of points that could be assigned to quadrants.
#' @param x An object returned from \code{d2ks1}.
#' @param pch A numeric that indicates the character to be used when plotting the results.
#' @param xlab A string to label the x-axis.
#' @param ylab A string to label the y-axis.
#' @param xlim A vector of length two that indicates the limits over which to plot the x-axis.
#' @param ylim A vector of length two that indicates the limits over which to plot the y-axis.
#' @param \dots Additional arguments sent to the plot function.
#' 
#' @return The main function returns a single numeric of the D test statistic if \code{justD=TRUE} or a list with the following items if \code{justD=FALSE}:
#'  \itemize{
#'    \item D The D test statistic.  See details.
#'    \item pval The approximate p-value from the Kolmogorov-Smirnov distribution.  Returned only if \code{KSp=TRUE}.  HIGHLY EXPERIMENTAL (NOT YET TESTED).
#'    \item n1 Sample size.
#'    \item max1 Maximum D .
#'    \item where1 Observation(s) where maximum D occurred.
#'    \item KSp Same logical supplied by user.
#'    \item divbylen Same logical supplied by user.
#'    \item x1 Same vector of X data supplied by user.
#'    \item y1 Same vector of Y data supplied by user.
#'  }
#'
#'The \code{plot} function return a scatterplot of the data with the point or points that resulted in the maximum D value.  The \code{print} function prints results in a nice format if \code{justD=FALSE} (only D will be printed if \code{justD=TRUE}).
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, with significant help from Ben Bolker.
#'
#' @references
#' Garvey, J.E., E.A. Marschall, and R.A. Wright.  1998.  \href{http://opensiuc.lib.siu.edu/fiaq_pubs/17/}{From star charts to stoneflies: detecting relationships in continuous bivariate data.} Ecology 79:442 447.
#'
#' Press, W.H., S.A. Teukolsky, W.T. Vetterling, B.P. Flannery.  2007.  \href{http://www.nr.com/}{Numerical Recipes: The Art of Scientific Computing, 3rd Edition.}  Cambridge University Press.  1286 pages. 
#' 
#' @seealso \code{\link{ks2d1p}}
#' 
#' @keywords htest
#'
#' @examples
#' # Example from Figure 4a in Garvey
#' data(Garvey4a)
#'
#' # with the defaults
#' ( res1 <- with(Garvey4a,ks2d1(shad,sunfish)) )
#' plot(res1,xlab="Shad Densitiy",ylab="Sunfish Density")
#'
#' # same, but with modified computation of proportions
#' ( res2 <- with(Garvey4a,ks2d1(shad,sunfish,divbylen=FALSE)) )
#'
#' # same, but compute large-sample p-value -- HIGHLY EXPERIMENTAL (NOT TESTED)
#' ( res3 <- with(Garvey4a,ks2d1(shad,sunfish,KSp=TRUE)) )
#'
#' # Example from Figure 1 in Garvey
#' data(Garvey1)
#' ( res4 <- with(Garvey1,ks2d1(Ameletus,Leuctra)) )
#' plot(res4,xlab="Ameletus Numbers",ylab="Leuctra Numbers",col=rgb(0,0,0,0.1))
#' 
#' @rdname ks2d1
#' @export ks2d1
ks2d1 <- function(x1,y1,justD=FALSE,KSp=FALSE,divbylen=TRUE) {
  ## compute differences between observed quadrant proportions and expected as
  ##   if the two distributions were independent.  Return the max absolute diff.
  dffrnc <- function(d,x,y,divbylen) {
    # observed proportions in each quadrant
    obs <- iquad_dens(d,x,y,divbylen)
    # expected proportions in each quadrant ... e.g., the expected proportion
    #   that are less than x0, y0 is the proportion less than x0 times the 
    #   the proportion less than y0.
    exp <- matrix(rowSums(obs),nrow=2) %*% matrix(colSums(obs),ncol=2)
    # find and return the maximum absolute difference
    max(abs(obs-exp))
  }  ## end internal dffrnc
  
  # find maximum observed difference among obs and exp within quadrants around each point
  diff <- apply(cbind(x1,y1),1,dffrnc,x1,y1,divbylen)
  # find the maximum difference ... this is the D test statistic
  D <- max(diff)
  # if only D is desired then return it, otherwise compute some more
  if (justD) D
  else {
    # find where maximum first occurred  
    where1 <- unique(which(round(diff,6)==round(D,6),arr.ind=TRUE))
    # get sample size
    n1 <- length(x1)
    # if p-value is not desired then return all from above, otherwise find it & return everything
    if (!KSp) res <- list(D=D,n1=n1,where1=where1,divbylen=divbylen,KSp=KSp,x1=x1,y1=y1)
    else {
      pval <- 1-ipkolmogorov1x(D,n1)  ## Not sure of this
      res <- list(D=D,pval=pval,n1=n1,where1=where1,divbylen=divbylen,KSp=KSp,x1=x1,y1=y1)
    }
    class(res) <- "ks2d1"
    res
  }
}

#' @rdname ks2d1
#' @export
print.ks2d1 <- function(x,...) {
  message("1-Sample Two-Dimensional Kolmogorov-Smirnov Test - THESE RESULTS ARE EXPERIMENTAL!!!")
  vars <- c("where1","D")
  tmp <- unlist(x[vars])
  tmp <- matrix(c(x$x1[x$where1],tmp,ifelse(x[["KSp"]],x[["pval"]],NA)),nrow=1)
  colnames(tmp) <- c("max1",vars,"pvalue")
  printCoefmat(tmp,cs.ind=1,tst.ind=2,has.Pvalue=x[["KSp"]],...)
}

#' @rdname ks2d1
#' @export
plot.ks2d1 <- function(x,xlab=NULL,ylab=NULL,xlim=range(x$x1),ylim=range(x$y1),pch=16,...) {
  if (is.null(xlab)) xlab <- deparse(substitute(x$x1))
  if (is.null(ylab)) ylab <- deparse(substitute(x$y1))
  plot(x$y1~x$x1,pch=pch,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
  abline(v=x$x1[x[["where1"]]],lty=3,col=rainbow(length(x[["where1"]])))
  abline(h=x$y1[x[["where1"]]],lty=3,col=rainbow(length(x[["where1"]])))
}




#' @title A permutation test to compute a p-value for the D test statistic for the one-sample two-dimensional Kolmogrov-Smirnov test.
#'
#' @description A permutation test to compute a p-value for the D test statistic for the one-sample two-dimensional Kolmogrov-Smirnov test.
#'
#' @details NEED DETAIL HERE.
#'
#' @note This function is experimental at best at this point.
#'
#' @aliases ks2d1p print.ks2d1p plot.ks2d1p
#'
#' @param object An object returned from \code{ks2d1}.
#' @param x An object returned from \code{ks2d1p}.
#' @param B A numeric representing the number of resamples.
#' @param xlab A string to label the x-axis.
#' @param main A string to label the main title on the plot.
#' @param \dots Additional arguments sent to the plot function.
#'
#' @return The main function returns a list with the following items:
#'  \itemize{
#'    \item D The D test statistic from the \code{ks2d2} object.
#'    \item pval The p-value from the permutation test.  See details.
#'    \item Ds The D test statistics from each of the B \sQuote{resamples}.
#'    \item B The \code{B} value supplied by the user.
#'  }
#'The \code{plot} function returns a density plot of the D test statistics from each of the B \sQuote{resamples} with the observed D test statistic shown with a vertical line.  The \code{print} function prints the results in a nice format).
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, with significant help from Ben Bolker.
#'
#' @seealso \code{\link{ks2d1}}
#'
#' @references
#' Garvey, J.E., E.A. Marschall, and R.A. Wright.  1998.  \href{http://opensiuc.lib.siu.edu/fiaq_pubs/17/}{From star charts to stoneflies: detecting relationships in continuous bivariate data.} Ecology 79:442 447.
#'
#' Press, W.H., S.A. Teukolsky, W.T. Vetterling, B.P. Flannery.  2007.  \href{http://www.nr.com/}{Numerical Recipes: The Art of Scientific Computing, 3rd Edition.}  Cambridge University Press.  1286 pages. 
#'
#' @keywords htest
#'
#' @examples
#' data(Garvey4a)
#'
#' # Results from the main 1-sample 2-D KS test from ks2d1
#' ( res1 <- with(Garvey4a,ks2d1(shad,sunfish)) )
#' plot(res1,xlab="Shad Densitiy",ylab="Sunfish Density")
#'
#' # permutation test using resampling
#' ( res1p <- ks2d1p(res1,B=10) )  # B should be >1000, used 10 here to save time
#' plot(res1p)
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
#' @export
print.ks2d1p <- function(x,...) {
  message("One-Sample Two-Dimensional Kolmogorov-Smirnov Test p-value - THESE RESULTS ARE EXPERIMENTAL!!!\n",
          "  Used 'resample' method B=",x[["B"]]," times.")
  tmp <- matrix(unlist(x[c("D","pval")]),nrow=1)
  colnames(tmp) <- c("D","pvalue")
  printCoefmat(tmp,tst.ind=1,has.Pvalue=TRUE,...)
}

#' @rdname ks2d1p
#' @export
plot.ks2d1p <- function(x,xlab="D Test Statistic",main="",...) {
  plot(density(x$Ds),xlab=xlab,main=main,xlim=range(c(x$D,x$Ds)))
  abline(v=x$D,col="red",lty=3)
  axis(3,at=x$D,labels="D",col="red")
}




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
#' @param KSp Logical that indicates whether the approximate p-value from the Kolmogorov-Smirnov distribution should be returned.  HIGHLY EXPERIMENTAL (NOT YET TESTED).
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
#'    \item pval The approximate p-value from the Kolmogorov-Smirnov distribution.  Returned only if \code{KSp=TRUE}.  HIGHLY EXPERIMENTAL (NOT YET TESTED)
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
#' # same but with large sample p-values --  HIGHLY EXPERIMENTAL (NOT YET TESTED)
#' ( res1 <- ks2d2(d1$x,d1$y,d2$x,d2$y,KSp=TRUE) )
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
#' @export
print.ks2d2 <- function(x,...) {
  message("Two-Sample Two-Dimensional Kolmogorov-Smirnov Test - THESE RESULTS ARE EXPERIMENTAL!!!")
  vars <- c("max1","where1","max2","where2","D")
  tmp <- unlist(x[vars])
  tmp <- matrix(c(tmp,ifelse(x[["KSp"]],x[["pval"]],NA)),nrow=1)
  colnames(tmp) <- c(vars,"pvalue")
  printCoefmat(tmp,cs.ind=c(1,3),tst.ind=5,has.Pvalue=x[["KSp"]],...)
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




#' @title A permutation test to compute a p-value for the D test statistic for the two-sample two-dimensional Kolmogrov-Smirnov test.
#'
#' @description A permutation test to compute a p-value for the D test statistic for the two-sample two-dimensional Kolmogrov-Smirnov test.
#'
#' @details NEED DETAIL HERE.
#'
#' @note This function is experimental at best at this point.
#'
#' @aliases ks2d2p print.ks2d2p plot.ks2d2p
#'
#' @param object An object returned from \code{ks2d2}.
#' @param x An object returned from \code{ks2d2p}.
#' @param B A numeric representing the number of resamples.
#' @param type The type of \sQuote{resampling} to be conducted.  See details.
#' @param randtype The type of randomization to use if \code{type="randomize"}; ignored if \code{type="resample"}.
#' @param coordX A vector of length two giving the minimum and maximum values of the X coordinates when \code{randtype="discrete"}; ignored if \code{type="resample"} or if \code{type="randomize"} and \code{randtype="continuous"}.
#' @param coordY Same as \code{coordX} but for Y coordinates.
#' @param xlab A string to label the x-axis.
#' @param main A string to label the main title on the plot.
#' @param \dots Additional arguments sent to the plot function.
#'
#' @return The main function returns a list with the following items:
#'  \itemize{
#'    \item D The D test statistic from the \code{ks2d2} object.
#'    \item pval The p-value from the permutation test.  See details.
#'    \item Ds The D test statistics from each of the B \sQuote{resamples}.
#'    \item type The \code{type} supplied by the user.
#'    \item randtype The \code{randtype} supplied by the user.
#'    \item B The \code{B} value supplied by the user.
#'  }
#'The \code{plot} function returns a density plot of the D test statistics from each of the B \sQuote{resamples} with the observed D test statistic shown with a vertical line.  The \code{print} function prints the results in a nice format).
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, with significant help from Ben Bolker.
#'
#' @seealso \code{\link{ks2d2}}
#'
#' @references
#' Garvey, J.E., E.A. Marschall, and R.A. Wright.  1998.  \href{http://opensiuc.lib.siu.edu/fiaq_pubs/17/}{From star charts to stoneflies: detecting relationships in continuous bivariate data.} Ecology 79:442 447.
#'
#' Press, W.H., S.A. Teukolsky, W.T. Vetterling, B.P. Flannery.  2007.  \href{http://www.nr.com/}{Numerical Recipes: The Art of Scientific Computing, 3rd Edition.}  Cambridge University Press.  1286 pages. 
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
#' # perform D2KS analysis
#' ( res1 <- ks2d2(d1$x,d1$y,d2$x,d2$y) )
#'
#' # perform permutation test using resampling
#' ( res1p <- ks2d2p(res1,B=10) )  # B should be >1000, used 10 here to save time
#' plot(res1p)
#'
#' @rdname ks2d2p
#' @export
ks2d2p <- function(object,B=100,type=c("resample","randomize"),randtype=c("discrete","continuous"),coordX=NULL,coordY=NULL) {
  resampD <- function(object) {
    # combine all x coords into one vector
    xt <- c(object[["x1"]],object[["x2"]]) 
    # combine all y coords into one vector
    yt <- c(object[["y1"]],object[["y2"]])
    # sample n1 positions in combined vectors
    svals <- sample(object[["n1"]]+object[["n2"]],size=object[["n1"]],replace=FALSE)
    # send selected points to ks2d to compute D
    ks2d2(xt[svals],yt[svals],xt[-svals],yt[-svals],justD=TRUE,divbylen=object$divbylen)
  } # end internal resamp.D
  
  DrandD <- function(object,coordX,coordY) {
    minx <- coordX[1]; maxx <- coordX[2]; rngx <- maxx-minx+1
    miny <- coordY[1]; maxy <- coordY[2]; rngy <- maxy-miny+1
    # sample "linear" points in first sample
    D1 <- sample(1:(rngx*rngy),object[["n1"]])
    temp <- floor(D1/maxx)
    temp[temp==(D1/maxx)] <- temp[temp==(D1/maxx)]-1
    # convert to coordinates
    Y1 <- miny + temp
    X1 <- minx + D1-(Y1-1)*maxx -1
    # sample "linear" points in second sample
    D2 <- sample(1:(rngx*rngy),object[["n2"]])
    temp <- floor(D2/maxx)
    temp[temp==(D2/maxx)] <- temp[temp==(D2/maxx)]-1
    # convert to coordinates
    Y2 <- miny + temp
    X2 <- minx + D2-(Y2-1)*maxx -1
    # compute and store D statistic
    ks2d2(X1,Y1,X2,Y2,justD=TRUE,divbylen=object$divbylen)
  } # end internal DrandD
  
  CrandD <- function(object,coordX,coordY) {
    minx <- coordX[1]; maxx <- coordX[2]
    miny <- coordY[1]; maxy <- coordY[2]
    X1 <- runif(object[["n1"]],minx,maxx)
    Y1 <- runif(object[["n1"]],miny,maxy)
    X2 <- runif(object[["n2"]],minx,maxx)
    Y2 <- runif(object[["n2"]],miny,maxy)
    ks2d2(X1,Y1,X2,Y2,justD=TRUE,divbylen=object$divbylen)    # compute and store D statistic
  } # end internal CrandD
  
  type <- match.arg(type)
  if (type=="resample") {
    randtype <- NULL
    Dstat <- replicate(B,resampD(object))                     # resample D B times
  } else {
    randtype <- match.arg(randtype)
    if (is.null(coordX) | is.null(coordY)) stop("Must supply coordinate ranges.",call.=FALSE)
    if (randtype=="discrete") Dstat <- replicate(B,DrandD(object,coordX,coordY))
    else Dstat <- replicate(B,CrandD(object,coordX,coordY))
  }
  p <- sum(Dstat>object[["D"]])/B                             # p-value is proportion of resamples with larger D
  res <- list(D=object[["D"]],pval=p,Ds=Dstat,type=type,randtype=randtype,B=B)        # return result
  class(res) <- "ks2d2p"
  res
}

#' @rdname ks2d2p
#' @export
print.ks2d2p <- function(x,...) {
  message("Two-Sample Two-Dimensional Kolmogorov-Smirnov Test p-value - THESE RESULTS ARE EXPERIMENTAL!!!")
  if (x[["type"]]=="resample") message("  Used 'resample' method")
    else message("  Used ",x[["randtype"]]," 'randomization' method")
  tmp <- matrix(unlist(x[c("D","pval")]),nrow=1)
  colnames(tmp) <- c("D","pvalue")
  printCoefmat(tmp,tst.ind=1,has.Pvalue=TRUE,...)
}

#' @rdname ks2d2p
#' @export
plot.ks2d2p <- function(x,xlab="D Test Statistic",main="",...) {
  plot(density(x$Ds),xlab=xlab,main=main,xlim=range(c(x$D,x$Ds)))
  abline(v=x$D,col="red",lty=3)
  axis(3,at=x$D,labels="D",col="red")
}




# ============================================================
# Internal functions used in ksd1
# ============================================================
ipkolmogorov1x <- function(x,n) {
  ## taken from internal function in ks.test()
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  j <- seq.int(from=0,to=floor(n*(1-x)))
  1-x*sum(exp(lchoose(n,j)+(n-j)*log(1-x-j/n)+(j-1)*log(x+j/n)))
}  ## end internal ipkolmogorov1x

iquad_dens <- function(d,x,y,divbylen) {
  ## compute densities in each quadrat around (x0,y0), which comes from d
  # get values of the point being tests
  x0 <- d[1]; y0 <- d[2]
  # compute the number of points in each quadrant off of that point
  res <- table(factor(sign(x-x0),levels=-1:1),
               factor(sign(y-y0),levels=-1:1))[c(1,3),c(1,3)]
  # calculate proportions in each quadrant off of that point ...
  if (divbylen) res/length(x)   # ... from entire data set
  else res/sum(res)             # ... adjusting for removal of density=0
}  ## end internal iquad_dens



