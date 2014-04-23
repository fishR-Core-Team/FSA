#'Computes the standard weight equation using the methods described in Froese (2006).
#'
#'Computes the standard weight equation using the geometric mean of a and the
#'mean of b from weight-length regression equations as described in Froese (2006).
#'
#'The main function computes the mean of the \eqn{log_{10}(a)} and b values for
#'the standard weight equation as detailed in Froese (2006).  Note that log(a)
#'and b must be from the regression of \eqn{log_{10}(W)} on \eqn{log_{10}(L)}
#'where W is measured in grams and L is the total length measured in mm.
#'
#'The \code{plot} and \code{coef} methods are used to construct a plot (see
#'below) and extract the coefficients of the standard weight equation.  The
#'\code{what} argument in the \code{plot} method can be set to \code{"both"},
#'\code{"log"}, or \code{"raw"}.  The \code{"raw"} plot plots lines on the
#'length-weight scale for each population represented in the \code{log.a} and
#'\code{b} vectors with the resultant standard weight equation superimposed in
#'red.  The \code{"log"} plot constructs a similar plot but on the
#'\eqn{log_{10}(weight)}-\eqn{log_{10}(length)} scale.  The \code{"both"}
#'option produces both plots side-by-side.
#'
#'If the \code{col.pop} argument is set equal to one of these palettes --
#'\dQuote{rich}, \dQuote{cm}, \dQuote{default}, \dQuote{grey}, \dQuote{gray},
#'\dQuote{heat}, \dQuote{jet}, \dQuote{rainbow}, \dQuote{topo}, or
#'\dQuote{terrain} -- and the \code{order.pop=TRUE} then the populations
#'plotted should form a general color gradient from smallest to largest weight
#'in the initial length category.  This will make it easier to identify
#'populations that \dQuote{cross over} other populations.
#'
#'@aliases FroeseWs plot.FroeseWs coef.FroeseWs
#'@param log.a A numeric vector that contains the \eqn{log_{10}(a)} values for the
#'population of length-weight regression equations.
#'@param b A numeric vector that contains the b values for the population of
#'length-weight regression equations
#'@param x An object saved from the \code{FrowseWs()} call (i.e., of class
#'\code{Froese}).
#'@param min A number that indicates the smallest X-mm length to model.
#'@param max A number that indicates the midpoint value of the largest X-mm length
#'category.
#'@param object An object saved from \code{FroeseWs()} (i.e., of class
#'\code{FroeseWs}).
#'@param what A string that indicates the type of plot to produce.  See details.
#'@param col.pop A string that indicates the type of color or palette to use for
#'the population of length-weight regression lines.  See details.
#'@param order.pop A logical that indicates whether the populations should be
#'plotted from the smallest to largest weight in the initial length category.
#'See details.
#'@param lwd.pop A numeric that indicates the width of the line to use for the
#'population of length-weight regression lines.
#'@param lty.pop A numeric that indicates the type of line to use for the
#'population of length-weight regression lines.
#'@param col.Ws A string that indicates the type of color to use for the standard
#'length-weight regression line.
#'@param lwd.Ws A numeric that indicates the width of the line to use for the
#'standard length-weight regression line.
#'@param lty.Ws A numeric that indicates the type of line to use for the standard
#'length-weight regression line.
#'@param \dots Additional arguments for methods.
#'@return A list is returned with five items.  The first (\code{log.a}) is a
#'numeric vector of the observed \eqn{log_{10}(a)} values sent in the
#'\code{log.a} argument.  The second (\code{b}) is a numeric vector of the
#'observed \eqn{b} values sent in the \code{b} argument.  The third
#'(\code{gm.a}) is a numeric that contains the geometric mean of the \eqn{a}
#'parameter.  This is simply the back-transformed mean \eqn{log_{10}(a)} value
#'-- i.e., \eqn{10^{log_{10}(a)}}.  The fourth (\code{mn.b}) is the arithmetic
#'mean of the \eqn{b} parameter.  The fifth item (\code{mn.log.a}) is the
#'arithmetic mean of \eqn{log_{10}(a)}.
#'@seealso \code{\link{rlp}}, \code{\link{emp}}, \code{\link{wsValidate}},
#'\code{quantile} in \pkg{stats}
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}
#'@references Froese, R.  2006.  Cube law, condition factor and weight-length
#'relationships: history, meta-analysis and recommendations.  Journal of
#'Applied Ichthyology 22:241-253.
#'@export
#'@keywords manip hplot
#'@examples
#'#See examples in RuffeWs in the FSAdata package.
#'@rdname FroeseWs
#'@export FroeseWs
FroeseWs <- function(log.a,b) {
  mn.log.a <- mean(log.a)
  gm.a <- 10^mn.log.a
  mn.b <- mean(b)
  res <- list(log.a=log.a,b=b,gm.a=gm.a,mn.b=mn.b,mn.log.a=mn.log.a)
  class(res) <- "FroeseWs"
  res
}

#'@rdname FroeseWs
#'@method coef FroeseWs
#'@S3method coef FroeseWs
coef.FroeseWs <- function(object,...) {
  res <- c(object$gm.a,object$mn.b)
  names(res) <- c("gm.a","mn.b")
  res
}

#'@rdname FroeseWs
#'@method plot FroeseWs
#'@S3method plot FroeseWs
plot.FroeseWs <- function(x,min,max,what=c("both","raw","log"),col.pop="rich",lwd.pop=1,lty.pop=1,order.pop=TRUE,col.Ws="black",lwd.Ws=3,lty.Ws=1,...) {
  object <- x
  what <- match.arg(what)
  if (col.pop %in% paletteChoices()) col.pop <- chooseColors(col.pop,length(object$b))
  len <- seq(min,max,length.out=200)
  for (i in 1:length(object$log.a)) {                                     # predict weight at each 1-cm midpoint for each population
    w <- 10^(object$log.a[i]+object$b[i]*log10(len))
    if (i==1) pred.w <- w                                          # store in a matrix called pred.w
    else pred.w <- cbind(pred.w,w)
  }
  colnames(pred.w) <- seq(1:dim(pred.w)[2])                        # rename columns to numbers that correspond to populations
  if (order.pop) pred.w <- pred.w[,order(pred.w[1,])]
  if (what=="both") old.par <- par(mar=c(3.5,3.5,1,1), mfcol=c(1,2), mgp=c(2,0.75,0))
    else old.par <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.75,0))
  on.exit(par(old.par))
  if (what=="raw" | what=="both") {
    matplot(len,pred.w,type="l",lty=lty.pop,lwd=lwd.pop,col=col.pop,xlab="Length (mm)",ylab="Weight (g)")
    curve(object$gm.a*x^object$mn.b,min(len),max(len),lty=lty.Ws,lwd=lwd.Ws,col=col.Ws,add=TRUE)
  }
  if (what=="log" | what=="both") {
    matplot(log10(len),log10(pred.w),type="l",lty=lty.pop,lwd=lwd.pop,col=col.pop,xlab="log10(Length (mm))",ylab="log10(Weight (g))")
    curve(log10(object$gm.a)+object$mn.b*x,min(log10(len)),max(log10(len)),lty=lty.Ws,lwd=lwd.Ws,col=col.Ws,add=TRUE)
  }
}
