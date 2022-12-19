#' @title Construct a base tic-tac-toe plot for presenting predator-prey PSD values.
#'
#' @description Construct a base tic-tac-toe plot for presenting predator-prey PSD values. Predator-prey PSD values are added with \code{\link[plotrix]{plotCI}} from \pkg{plotrix}.
#'
#' @details This function simply creates a base tic-tac-toe plot. Observed values, with confidence intervals, are added to this plot with \code{\link[plotrix]{plotCI}} from \pkg{plotrix}; see examples.
#'
#' @param predobj A vector of length 2 that contains the target objective range for the predator.
#' @param preyobj A vector of length 2 that contains the target objective range for the prey.
#' @param predlab A string representing a label for the x-axis.
#' @param preylab A string representing a label for the y-axis.
#' @param obj.col A string designating a color to which the target objective regions should be shaded.
#' @param obj.trans A numeric (decimal) that indicates the level of transparency for marking the target objective regions.
#' @param bnd.col A string that indicates a color for the boundaries of the target objective regions.
#' @param bnd.lwd A numeric that indicates the line width for the boundaries of the target objective regions.
#' @param bnd.lty A numeric that indicates the line type for the boundaries of the target objective regions.
#'
#' @return None. However, a graphic is produced.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: 6-Size Structure.
#'
#' @seealso See \code{\link{psdVal}} and \code{\link{psdCalc}} for related functionality.
#' 
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
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
#' ## Create hypothetical data for plotting three points ... similar to what might come from psdCalc()
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
tictactoe <- function(predobj=c(30,70),preyobj=c(30,70),
                      predlab="Predator PSD",preylab="Prey PSD",
                      obj.col="black",obj.trans=0.2,
                      bnd.col="black",bnd.lwd=1,bnd.lty=2) {
  ## Test values
  if (!is.numeric(predobj)) STOP("Predator target objectives must be numeric.")
  if (length(predobj)!=2)
    STOP("Predator target objective must contain two numbers.")
  if (any(predobj<0|predobj>100))
    STOP("Predator target objectives must be between 0 and 100.")
  if (!is.numeric(preyobj)) STOP("Prey target objectives must be numeric.")
  if (length(preyobj)!=2)
    STOP("Prey target objective must contain two numbers.")
  if (any(preyobj<0|preyobj>100))
    STOP("Prey target objectives must be between 0 and 100.")  ## Base plot
  # nocov start
  graphics::plot(NULL,xlim=c(0,100),ylim=c(0,100),
                 xlab=predlab,ylab=preylab,xaxt="n",yaxt="n")
  graphics::axis(1,seq(0,100,10))
  graphics::axis(2,seq(0,100,10))
  ## Get axis limits
  xmin <- ifelse(graphics::par("xaxs")=="r",-10,0)
  xmax <- ifelse(graphics::par("xaxs")=="r",110,100)
  ymin <- ifelse(graphics::par("yaxs")=="r",-10,0)
  ymax <- ifelse(graphics::par("yaxs")=="r",110,100)
  ## Add the shaded objective regions if a color is given
  if (!is.null(obj.col)) {
    graphics::polygon(c(rep(predobj,each=2),predobj[1]),
                      c(ymin,ymax,ymax,ymin,ymin),
                      col=col2rgbt(obj.col,1/obj.trans),border=NA)
    graphics::polygon(c(xmin,xmax,xmax,xmin,xmin),
                      c(rep(preyobj,each=2),preyobj[1]),
                      col=col2rgbt(obj.col,1/obj.trans),border=NA)
  }
  ## add borders to objective regions
  for (i in 1:2) {
    graphics::lines(x=c(predobj[i],predobj[i]),y=c(ymin,ymax),
                    col=bnd.col,lwd=bnd.lwd,lty=bnd.lty)
    graphics::lines(x=c(xmin,xmax),y=c(preyobj[i],preyobj[i]),
                    col=bnd.col,lwd=bnd.lwd,lty=bnd.lty)
  }
  # nocov end
}
