#'A permutation test to compute a p-value for the D test statistic for the
#'two-sample two-dimensional Kolmogrov-Smirnov test.
#'
#'A permutation test to compute a p-value for the D test statistic for the
#'two-sample two-dimensional Kolmogrov-Smirnov test.
#'
#'NEED DETAIL HERE.
#'
#'@aliases ks2d2p print.ks2d2p plot.ks2d2p
#'@param object An object returned from \code{ks2d2}.
#'@param x An object returned from \code{ks2d2p}.
#'@param B A numeric representing the number of resamples.
#'@param type The type of \sQuote{resampling} to be conducted.  See details.
#'@param randtype The type of randomization to use if \code{type="randomize"};
#'ignored if \code{type="resample"}.
#'@param coordX A vector of length two giving the minimum and maximum values of
#'the X coordinates when \code{randtype="discrete"}; ignored if
#'\code{type="resample"} or if \code{type="randomize"} and \code{randtype="continuous"}.
#'@param coordY Same as \code{coordX} but for Y coordinates.
#'@param xlab A string to label the x-axis.
#'@param main A string to label the main title on the plot.
#'@param \dots Additional arguments sent to the plot function.
#'@return The main function returns a list with the following items:
#'\itemize{
#'\item D The D test statistic from the \code{ks2d2} object.
#'\item pval The p-value from the permutation test.  See details.
#'\item Ds The D test statistics from each of the B \sQuote{resamples}.
#'\item type The \code{type} supplied by the user.
#'\item randtype The \code{randtype} supplied by the user.
#'\item B The \code{B} value supplied by the user.
#'}
#'The \code{plot} function returns a density plot of the D test statistics from
#'each of the B \sQuote{resamples} with the observed D test statistic shown
#'with a vertical line.  The \code{print} function prints the results in a nice
#'format).
#'@note This function is experimental at best at this point.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}, with significant help from Ben Bolker.
#'@seealso \code{\link{ks2d2}}
#'@references Garvey, J.E., E.A. Marschall, and R.A. Wright.  1998.  From star
#'charts to stoneflies: detecting relationships in continuous bivariate data.
#'Ecology 79:442 447.
#'
#'Press, W.H., S.A. Teukolsky, W.T. Vetterling, B.P. Flannery.  2007.
#'Numerical Recipes: The Art of Scientific Computing, 3rd Edition.  Cambridge
#'University Press.  1286 pages.
#'@keywords htest
#'@examples
#'data(KS2D_NR)
#'
#'# separate into the two sets of coordinates
#'d1 <- subset(KS2D_NR,group=="triangles")
#'d2 <- subset(KS2D_NR,group=="squares")
#'
#'# perform D2KS analysis
#'( res1 <- ks2d2(d1$x,d1$y,d2$x,d2$y) )
#'
#'# perform permutation test using resampling
#'( res1p <- ks2d2p(res1,B=10) )  # B should be >1000, used 10 here to save time
#'plot(res1p)
#'
#'@rdname ks2d2p
#'@export ks2d2p
ks2d2p <- function(object,B=100,type=c("resample","randomize"),randtype=c("discrete","continuous"),coordX=NULL,coordY=NULL) {
  resampD <- function(object) {
    xt <- c(object[["x1"]],object[["x2"]])                                            # combine all x coords into one vector 
    yt <- c(object[["y1"]],object[["y2"]])                                            # combine all y coords into one vector
    svals <- sample(object[["n1"]]+object[["n2"]],size=object[["n1"]],replace=FALSE)  # sample n1 positions in combined vectors
    ks2d2(xt[svals],yt[svals],xt[-svals],yt[-svals],justD=TRUE,divbylen=object$divbylen)      # send selected points to ks2d to compute D
  } # end internal resamp.D
  
  DrandD <- function(object,coordX,coordY) {
    minx <- coordX[1]; maxx <- coordX[2]; rngx <- maxx-minx+1
    miny <- coordY[1]; maxy <- coordY[2]; rngy <- maxy-miny+1
    D1 <- sample(1:(rngx*rngy),object[["n1"]])                # sample "linear" points in first sample
    temp <- floor(D1/maxx)
    temp[temp==(D1/maxx)] <- temp[temp==(D1/maxx)]-1
    Y1 <- miny + temp                                         # convert to coordinates
    X1 <- minx + D1-(Y1-1)*maxx -1
    D2 <- sample(1:(rngx*rngy),object[["n2"]])                # sample "linear" points in second sample
    temp <- floor(D2/maxx)
    temp[temp==(D2/maxx)] <- temp[temp==(D2/maxx)]-1
    Y2 <- miny + temp                                         # convert to coordinates
    X2 <- minx + D2-(Y2-1)*maxx -1
    ks2d2(X1,Y1,X2,Y2,justD=TRUE,divbylen=object$divbylen)    # compute and store D statistic
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

#'@rdname ks2d2p
#'@method print ks2d2p
#'@S3method print ks2d2p
print.ks2d2p <- function(x,...) {
  cat("Two-Sample Two-Dimensional Kolmogorov-Smirnov Test p-value - THESE RESULTS ARE EXPERIMENTAL AT THIS POINT!!!\n")
  if (x[["type"]]=="resample") txt <- "  Used 'resample' method"
  else txt <- paste("  Used",x[["randtype"]],"'randomization' method")
  cat(txt,"B=",x[["B"]],"times\n")
  cat("D=",formatC(x[["D"]],format="f",digits=4),", p-value =",formatC(x[["pval"]],format="g"),"\n")
}

#'@rdname ks2d2p
#'@method plot ks2d2p
#'@S3method plot ks2d2p
plot.ks2d2p <- function(x,xlab="D Test Statistic",main="",...) {
  plot(density(x$Ds),xlab=xlab,main=main,xlim=range(c(x$D,x$Ds)))
  abline(v=x$D,col="red",lty=3)
  axis(3,at=x$D,labels="D",col="red")
}
