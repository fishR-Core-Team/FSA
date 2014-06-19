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
#' @param KSp Logical that indicates whether the approximate p-value from the Kolmogorov-Smirnov distribution should be returned.  NOT YET IMPLEMENTED.
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
#'    \item pval The approximate p-value from the Kolmogorov-Smirnov distribution.  Returned only if \code{KSp=TRUE}.  NOT YET IMPLEMENTED.
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
#' # same, but compute large-sample p-value
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
#' @method print ks2d1
#' @export
print.ks2d1 <- function(x,...) {
  cat("1-Sample Two-Dimensional Kolmogorov-Smirnov Test -- THESE RESULTS ARE EXPERIMENTAL AT THIS POINT!!!\n\n")
  cat("Maximum occurred for observation #",x[["where1"]],"\n",sep="")
  txt <- paste("\nTest statistic (D) of",formatC(x[["D"]],format="f",digits=4))
  if (x[["KSp"]]) txt <- paste(txt,", with a K-S p-value of ",formatC(x[["pval"]],format="f",digits=4),sep="")
  cat(paste(txt,"\n"))
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
