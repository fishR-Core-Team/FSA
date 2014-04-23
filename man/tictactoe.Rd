\name{tictactoe}
\alias{tictactoe}
\title{Constructs the base tic-tac-toe graph for presenting predator-prey PSD values.}
\usage{
tictactoe(predbal = c(30, 70), preybal = c(30, 70), xlab = "Predator PSD",
  ylab = "Prey PSD", bal.col = "wheat", bal.trans = 0.3,
  bnd.col = "red", bnd.lwd = 2, bnd.lty = 2)
}
\arguments{
  \item{predbal}{A vector of length 2 that contains the
  range of values to be considered \dQuote{in balance} for
  the predator.}

  \item{preybal}{A vector of length 2 that contains the
  range of values to be considered \dQuote{in balance} for
  the prey.}

  \item{xlab}{A string representing a label for the
  x-axis.}

  \item{ylab}{A string representing a label for the
  y-axis.}

  \item{bal.col}{A string designating a color to which the
  \dQuote{in balance} regions should be shaded.}

  \item{bal.trans}{A numeric (decimal) that indicates the
  level of transparency for marking the \dQuote{in balance}
  regions.}

  \item{bnd.col}{A string designating a color for the
  boundaries of the \dQuote{in balance} regions.}

  \item{bnd.lwd}{A numeric designating the line width for
  the boundaries of the \dQuote{in balance} region.}

  \item{bnd.lty}{A numeric designating the line type for
  the boundaries of the \dQuote{in balance} regions.}
}
\value{
None.  However, a graphic is produced.
}
\description{
Constructs the base tic-tac-toe graph for presenting
predator-prey PSD values. Predator prey values are added
with \code{\link{tictactoeAdd}}.
}
\details{
This function simply creates the base tic-tac-toe graph.
Observed values, with confidence intervals, can be added to
this graph with \code{\link{tictactoeAdd}}.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/SizeStructure.pdf}
}
\examples{
op <- par(mfrow=c(2,2))
## Simple plot with one point on it
tictactoe()
tictactoeAdd(c(50,80),c(25,45))
## Changed predator balance lines, color of point
tictactoe(predbal=c(20,50))
tictactoeAdd(c(50,80),c(25,45),pt.col="blue")
## Add two points to plot
tictactoe()
tictactoeAdd(c(50,80),c(25,45),pt.col="blue")
tictactoeAdd(c(30,80),c(35,45),pt.col="green")
## Add two points to plot, and label points
tictactoe()
tictactoeAdd(c(50,80),c(25,45),pt.col="blue",label="2007",lbl.cex=1.5)
tictactoeAdd(c(30,80),c(35,45),pt.col="green",label="2009",lbl.pos="SW",lbl.cex=1.5)

par(op)
}
\seealso{
\code{\link{tictactoeAdd}}, \code{\link{psdVal}},
\code{\link{psdCalc}},\ \code{\link{psdPlot}},
\code{\link{psdDataPrep}}, \code{\link{PSDlit}}.
}
\keyword{hplot}

