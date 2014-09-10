#' @title Length-frequency histogram with PSD-X categories highlighted.
#'
#' @description Constructs a length-frequency histogram with PSD-X categories highlighted.
#'
#' @details This function creates a length-frequency histogram with the stock-size fish highlighted, the Gabelhouse length values marked by vertical lines, and the PSD-X values superimposed.
#'
#' The length of fish plotted on the x-axis can be controlled with \code{xlim}, however, the minimum value in \code{xlim} must be less than the length of a stock size fish for that species.
#'
#' @aliases psdPlot
#'
#' @param formula A formula of the form \code{~length} where \dQuote{length} generically represents a variable in \code{data} that contains length measurements.  Note that this formula can only contain one variable.
#' @param data A data.frame that minimally contains the length measurements given in the variable in the \code{formula}.
#' @param species A string that contains the species name for which five-cell length categories exist.  See \code{\link{psdVal}} for details.
#' @param units A string that indicates the type of units used for the length measurements.  Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param startcat A number that indicates the beginning of the first length-class.
#' @param w A number that indicates the width of length classes to create.
#' @param justPSDQ A logical that indicates whether just stock and quality (for PSD-Q calculations) categories should be used.  If \code{FALSE} (default) then the five-cell categories of Gabelhouse will be used.
#' @param main A string that serves as the main label for the histogram.
#' @param xlab A string that serves as the label for the x-axis.
#' @param ylab A string that serves as the label for the y-axis.
#' @param xlim A numeric vector of length two that indicates the minimum and maximum values for the x-axis.
#' @param ylim A numeric vector of length two that indicates the minimum and maximum values for the y-axis.
#' @param substock.col A string that indicates the color to use for the bars representing under-stock size fish.
#' @param stock.col A string that indicates the color to use for the bars representing stock size fish.
#' @param psd.col A string that indicates the color to use for the vertical lines at the PSD category values.
#' @param psd.lty A numeric that indicates the line type to use for the vertical lines at the PSD category values.
#' @param psd.lwd A numeric that indicates the line width to use for the vertical lines at the PSD category values.
#' @param legend.pos A string that indicates the position for the legend text.
#' @param show.abbrevs A logical that indicates if the abbreviations for the Gabelhouse length categories should be added to the top of the plot.
#' @param legend.cex A numeric value that indicates the character expansion for the legend text.
#' @param \dots Arguments to be passed to the low-level plotting functions.
#'
#' @return None.  However, a graphic is produced.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{psdVal}}, \code{\link{psdCalc}}, \code{\link{psdDataPrep}}, \code{\link{PSDlit}}, \code{\link{lencat}}, \code{\link{tictactoe}}, \code{\link{tictactoeAdd}}, \code{\link{lencat}}, and \code{\link{rcumsum}}.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/SizeStructure.pdf}
#'
#' @references
#' Guy, C.S., R.M. Neumann, and D.W. Willis.  2006.  \href{http://pubstorage.sdstate.edu/wfs/415-F.pdf}{New terminology for proportional stock density (PSD) and relative stock density (RSD): proportional size structure (PSS).}  Fisheries 31:86-87.  
#'
#' Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson.  2006.  \href{http://www.montana.edu/mtcfru/Guy/Publication\%20pdf/PSD\%20pub.pdf}{Proportional size distribution (PSD): A further refinement of population size structure index terminology.}  Fisheries 32:348.
#'
#' Willis, D.W., B.R. Murphy, and C.S. Guy.  1993.  \href{http://web1.cnre.vt.edu/murphybr/web/Readings/Willis\%20et\%20al.pdf}{Stock density indices: development, use, and limitations.}  Reviews in Fisheries Science 1:203-222. 
#'
#' @keywords hplot
#'
#' @examples
#' ## Random length data
#' # suppose this is yellow perch to the nearest mm
#' yepmm <- c(rnorm(100,mean=125,sd=15),rnorm(50,mean=200,sd=25),rnorm(20,mean=300,sd=40))
#' # same data to the nearest 0.1 cm
#' yepcm <- yepmm/10
#' # same data to the nearest 0.1 in
#' yepin <- yepmm/25.4
#' # put together as data.frame
#' yepdf <- data.frame(yepmm,yepcm,yepin)
#'
#' ## Example graphics
#' op <- par(mfrow=c(2,2),mar=c(3.5,3.5,1,1),mgp=c(2,0.75,0))
#' # mm data using 10-mm increments
#' psdPlot(~yepmm,data=yepdf,species="Yellow perch",w=10)
#' # cm data using 1-cm increments
#' psdPlot(~yepcm,data=yepdf,species="Yellow perch",units="cm",w=1)
#' # inch data using 1-in increments
#' psdPlot(~yepin,data=yepdf,species="Yellow perch",units="in",w=1)
#' # same as first with some color changes
#' psdPlot(~yepmm,data=yepdf,species="Yellow perch",w=10,substock.col="gray90",stock.col="gray30")
#'
#' par(op)
#'
#' @export psdPlot
psdPlot <- function(formula,data,species="List",units=c("mm","cm","in"),
                    startcat=0,w=1,justPSDQ=FALSE,
                    main="",xlab="Length",ylab="Number",
                    xlim=NULL,ylim=c(0,max(h$counts)),
                    substock.col="white",stock.col="gray90",
                    psd.col="black",psd.lty=2,psd.lwd=1,
                    show.abbrevs=TRUE,
                    legend.pos="topleft",legend.cex=0.75,...) {
  ## Get variable name that contains the lengths
  cl <- iGetVarFromFormula(formula,data,expNumVars=1)
  ## get ultimate sample size 
  n <- nrow(data)
  ## get psd values for this species
  if (justPSDQ) psdlens <- psdVal(species,units=units)[1:3]
    else psdlens <- psdVal(species,units=units)  
  ## If xlim is provided then limit temporary df to fish in range of xlim
  if (!is.null(xlim)) {
    if (min(xlim)>psdlens["stock"]) stop("Minimum chosen length value in 'xlim' is greater than 'stock' size.",call.=FALSE)
    dftemp <- data[data[,cl]>=min(xlim) & data[,cl]<=max(xlim),] 
  } else dftemp <- data
  ## make an initial histogram to get the breaks and counts
  min.brk <- min(c(dftemp[,cl],psdlens),na.rm=TRUE)
  max.brk <- max(dftemp[,cl])+w
  h <- hist(dftemp[,cl],right=FALSE,breaks=seq(min.brk,max.brk,w),plot=FALSE)
  ## Create xlim values if none were given
  if (is.null(xlim)) xlim=range(h$breaks)
  ## Create colors for the bars
  clr <- ifelse(h$breaks<psdlens[2],substock.col,stock.col)
  ## Plot the histogram with the new colors
  plot(h,col=clr,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,...)
  box()
  if (show.abbrevs) axis(3,at=psdlens[-1],labels=toupper(substring(names(psdlens)[-1],1,1)))
  ## add psd category lines
  abline(v=psdlens[-1],col=psd.col,lty=psd.lty,lwd=psd.lwd)
  ## add PSD calculations
  # get PSDs
  psds <- psdCalc(formula,data=dftemp,species=species,units=units,what="traditional")
  # reduce to only those that are >0 (drop is needed in case it reduces to only one)
  psds <- psds[psds[,"Estimate"]>0,,drop=FALSE]
  # add stock number
  n.stock <- nrow(Subset(dftemp,dftemp[,cl]>psdlens[2]))
  # put it all together
  psdleg <- paste(c("n","n[stock]",rownames(psds)),"=",formatC(c(n,n.stock,psds[,"Estimate"]),format="f",digits=0))
  legend(legend.pos,psdleg,cex=legend.cex,box.col="white",bg="white",inset=0.002)
  rm(dftemp)
}
