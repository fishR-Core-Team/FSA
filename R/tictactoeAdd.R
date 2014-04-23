#'Adds point and confidence intervals for observed preator-prey PSD to
#'tic-tac-toe graph.
#'
#'Adds point and confidence intervals for observed preator-prey PSD to
#'tic-tac-toe graph.  The base tic-tac-toe graph is constructed with the
#'\code{tictactoe} function.
#'
#'This function adds the point and confidence interval for observed
#'predator-prey PSD values to a base tic-tac-toe graph that already exists.
#'Thus, the \code{tictactoe} function must be run before this function and the
#'resultant plot must be active.
#'
#'Confidence intervals are constructed assuming a binomial distribution with
#'the \code{binCI} function.
#'
#'@param predval A vector of length 2 that contains the number of predators in
#'two length categories.  It is assumed that the number in the larger length
#'category and, thus, the smaller number of individuals is given first.
#'@param preyval A vector of length 2 that contains the number of prey in two
#'length categories.  It is assumed that the number in the larger length
#'category and, thus, the smaller number of individuals is given first.
#'@param conf.level A numeric representing the confidence level for the confidence intervals.
#'@param ci.type A string representing the method to use for computing the
#'binomial confidence intervals.  See details of \code{binCI}.
#'@param ci.lwd A numeric designating the line width for the condince interval lines.
#'@param pt A numeric representing the type of symbol to use for the
#'predator/prey PSD point.
#'@param pt.col A string representing the color of the symbol used for the
#'predator/prey PSD point.
#'@param pt.cex A numeric representing the relative size of the symbol used for
#'the predator/prey PSD point.
#'@param label A string used to label the point.
#'@param lbl.pos A string that indicates where to place the label.
#'@param lbl.col A string that indicates the color to use for the label.  Defaults
#'to same color as the color of the point.
#'@param lbl.cex A numberic representing the relative size of the point label.
#'@return None.  However, a base graphic is modified.
#'@seealso \code{\link{tictactoe}}, \code{\link{psdVal}}, \code{\link{binCI}}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/SizeStructure.pdf}
#'@keywords aplot
#'@examples
#'## See example for tictactoe() function.
#'
#'@export
tictactoeAdd <- function(predval,preyval,conf.level=0.95,ci.type=c("wilson","exact","asymptotic"),
                         ci.lwd=2,pt=19,pt.col="red",pt.cex=1,
                         label=NULL,lbl.pos=c("NE","NW","SE","SW"),lbl.col=pt.col,lbl.cex=1) {
  ci.type <- match.arg(ci.type)
  lbl.pos <- match.arg(lbl.pos)
  if (length(predval)!=2) stop("Values for predator must contain two numbers.\n",call.=FALSE)
  if (length(preyval)!=2) stop("Values for prey must contain two numbers.\n",call.=FALSE)
  if (predval[2]<predval[1]) stop("The first value for the predator is larger than the second.\nIt is assumed that the number in the larger length category is given first.",call.=FALSE)
  if (preyval[2]<preyval[1]) stop("The first value for the prey is larger than the second.\nIt is assumed that the number in the larger length category is given first.",call.=FALSE)  
  pred.PSD <- predval[1]/predval[2]*100; prey.PSD <- preyval[1]/preyval[2]*100
  points(pred.PSD,prey.PSD,pch=pt,col=pt.col,cex=pt.cex)
  pred.ci <- binCI(predval[1],predval[2],conf.level=conf.level,type=ci.type)*100
  prey.ci <- binCI(preyval[1],preyval[2],conf.level=conf.level,type=ci.type)*100
  lines(pred.ci,rep(prey.PSD,2),lwd=ci.lwd,col=pt.col)
  lines(rep(pred.PSD,2),prey.ci,lwd=ci.lwd,col=pt.col)
  if (!is.null(label)) text(pred.PSD,prey.PSD,label=label,col=lbl.col,cex=lbl.cex,adj=pos2adj(lbl.pos))
  cat("Predator PSD was ",round(pred.PSD,0)," with a ",100*conf.level,"% CI of (",round(pred.ci[1],1),",",round(pred.ci[2],1),").\n",sep="")
  cat("Prey PSD was ",round(prey.PSD,0)," with a ",100*conf.level,"% CI of (",round(prey.ci[1],1),",",round(prey.ci[2],1),").\n",sep="")
}
