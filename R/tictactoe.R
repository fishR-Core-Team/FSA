#' @title Construct a base tic-tac-toe plot for presenting predator-prey PSD values.
#'
#' @description Construct a base tic-tac-toe plot for presenting predator-prey PSD values.  Predator-prey PSD values are added with \code{plotCI} from \pkg{plotrix}.
#'
#' @details This function simply creates a base tic-tac-toe plot.  Observed values, with confidence intervals, are added to this plot with \code{plotCI} from \pkg{plotrix}.
#'
#' @param predbal A vector of length 2 that contains the range of values to be considered \dQuote{in balance} for the predator.
#' @param preybal A vector of length 2 that contains the range of values to be considered \dQuote{in balance} for the prey.
#' @param xlab A string representing a label for the x-axis.
#' @param ylab A string representing a label for the y-axis.
#' @param bal.col A string designating a color to which the \dQuote{in balance} regions should be shaded.
#' @param bal.trans A numeric (decimal) that indicates the level of transparency for marking the \dQuote{in balance} regions.
#' @param bnd.col A string that indicates a color for the boundaries of the \dQuote{in balance} regions.
#' @param bnd.lwd A numeric that indicates the line width for the boundaries of the \dQuote{in balance} region.
#' @param bnd.lty A numeric that indicates the line type for the boundaries of the \dQuote{in balance} regions.
#'
#' @return None.  However, a graphic is produced.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{psdVal}}, \code{\link{psdCalc}}.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/SizeStructure.pdf}
#'
#' @keywords hplot
#'
#' @examples
#' ## Create hypothetical data for plotting one point .. similar to what might come from psdCalc()
#' prey <- c(45.4,30.2,56.8)
#' pred <- c(24.5,10.2,36.7)
#' names(prey) <- names(pred) <- c("Estimate","95% LCI","95% UCI")
#' prey
#' pred
#' 
#' tictactoe()
#' if (require(plotrix)) {
#'   plotCI(prey[1],pred[1],li=prey[2],ui=prey[3],err="x",pch=16,add=TRUE)
#'   plotCI(prey[1],pred[1],li=pred[2],ui=pred[3],err="y",pch=16,add=TRUE) 
#' }
#' 
#' ## Create hypothetical data for plotting three points .. similar to what might come from psdCalc()
#' prey <- rbind(c(45.4,30.2,56.8),
#'               c(68.2,56.7,79.4),
#'               c(17.1, 9.5,26.3))
#' pred <- rbind(c(24.5,10.2,36.7),
#'               c(14.2, 7.1,21.3),
#'               c(16.3, 8.2,24.4))
#' colnames(prey) <- colnames(pred) <- c("Estimate","95% LCI","95% UCI")
#' prey
#' pred
#' 
#' tictactoe()
#' if (require(plotrix)) {
#'   plotCI(prey[,1],pred[,1],li=prey[,2],ui=prey[,3],err="x",pch=16,add=TRUE)
#'   plotCI(prey[,1],pred[,1],li=pred[,2],ui=pred[,3],err="y",pch=16,add=TRUE)
#' }
#' lines(prey[,1],pred[,1])
#' text(prey[,1],pred[,1],labels=c(2010,2011,2012),adj=c(-0.5,-0.5))
#' 
#' @export
tictactoe <- function(predbal=c(30,70),preybal=c(30,70),xlab="Predator PSD",ylab="Prey PSD",
                      bal.col="wheat",bal.trans=0.3,bnd.col="red",bnd.lwd=2,bnd.lty=2) {
  old.par <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.75,0)); on.exit(par(old.par))
  if (length(predbal)!=2) stop("Values for predator balance must contain two numbers.\n",call.=FALSE)
  if (length(preybal)!=2) stop("Values for prey balance must contain two numbers.\n",call.=FALSE)
  x <- y <- -100
  plot(y~x,xlim=c(0,100),ylim=c(0,100),xlab=xlab,ylab=ylab,xaxt="n",yaxt="n")
  axis(1,seq(0,100,10)); axis(2,seq(0,100,10))
  x1 <- predbal[1]; x2 <- predbal[2]
  y1 <- preybal[1]; y2 <- preybal[2]
  if (!is.null(bal.col)) {
    polygon(c(x1,x1,x2,x2,x1),c(-10,110,110,-10,-10),col=iMakeColor(bal.col,1/bal.trans),border=NA)
    polygon(c(-10,110,110,-10,-10),c(y1,y1,y2,y2,y1),col=iMakeColor(bal.col,1/bal.trans),border=NA)
    polygon(c(x1,x1,x2,x2,x1),c(y1,y2,y2,y1,y1),col=bal.col,border=NA)
  }
  abline(v=predbal,col=bnd.col,lwd=bnd.lwd,lty=bnd.lty)
  abline(h=preybal,col=bnd.col,lwd=bnd.lwd,lty=bnd.lty)
}
