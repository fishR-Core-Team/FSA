#'Constructs the base tic-tac-toe graph for presenting predator-prey PSD values.
#'
#'Constructs the base tic-tac-toe graph for presenting predator-prey PSD values.
#'Predator prey values are added with \code{\link{tictactoeAdd}}.
#'
#'This function simply creates the base tic-tac-toe graph.  Observed values, with
#'confidence intervals, can be added to this graph with \code{\link{tictactoeAdd}}.
#'
#'@param predbal A vector of length 2 that contains the range of values to be
#'considered \dQuote{in balance} for the predator.
#'@param preybal A vector of length 2 that contains the range of values to be
#'considered \dQuote{in balance} for the prey.
#'@param xlab A string representing a label for the x-axis.
#'@param ylab A string representing a label for the y-axis.
#'@param bal.col A string designating a color to which the \dQuote{in balance}
#'regions should be shaded.
#'@param bal.trans A numeric (decimal) indicating the level of transparency for
#'marking the \dQuote{in balance} regions.
#'@param bnd.col A string designating a color for the boundaries of the
#'\dQuote{in balance} regions.
#'@param bnd.lwd A numeric designating the line width for the boundaries of the
#'\dQuote{in balance} region.
#'@param bnd.lty A numeric designating the line type for the boundaries of the
#'\dQuote{in balance} regions.
#'@return None.  However, a graphic is produced.
#'@seealso \code{\link{tictactoeAdd}}, \code{\link{psdVal}}, \code{\link{psdCalc}},\
#' \code{\link{psdPlot}}, \code{\link{psdDataPrep}}, \code{\link{PSDlit}}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/SizeStructure.pdf}
#'@export
#'@keywords hplot
#'@examples
#'op <- par(mfrow=c(2,2))
#'## Simple plot with one point on it
#'tictactoe()
#'tictactoeAdd(c(50,80),c(25,45))
#'## Changed predator balance lines, color of point
#'tictactoe(predbal=c(20,50))
#'tictactoeAdd(c(50,80),c(25,45),pt.col="blue")
#'## Add two points to plot
#'tictactoe()
#'tictactoeAdd(c(50,80),c(25,45),pt.col="blue")
#'tictactoeAdd(c(30,80),c(35,45),pt.col="green")
#'## Add two points to plot, and label points
#'tictactoe()
#'tictactoeAdd(c(50,80),c(25,45),pt.col="blue",label="2007",lbl.cex=1.5)
#'tictactoeAdd(c(30,80),c(35,45),pt.col="green",label="2009",lbl.pos="SW",lbl.cex=1.5)
#'
#'par(op)
#'
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
    polygon(c(x1,x1,x2,x2,x1),c(-10,110,110,-10,-10),col=makeColor(bal.col,1/bal.trans),border=NA)
    polygon(c(-10,110,110,-10,-10),c(y1,y1,y2,y2,y1),col=makeColor(bal.col,1/bal.trans),border=NA)
    polygon(c(x1,x1,x2,x2,x1),c(y1,y2,y2,y1,y1),col=bal.col,border=NA)
  }
  abline(v=predbal,col=bnd.col,lwd=bnd.lwd,lty=bnd.lty)
  abline(h=preybal,col=bnd.col,lwd=bnd.lwd,lty=bnd.lty)
}
