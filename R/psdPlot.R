#' @title Length-frequency histogram with Gabelhouse lengths highlighted.
#'
#' @description Constructs a length-frequency histogram with Gabelhouse lengths highlighted.
#'
#' @details Constructs a length-frequency histogram with the stock-sized fish highlighted, the Gabelhouse lengths marked by vertical lines, and the (traditional) PSD-X values superimposed.
#'
#' The length of fish plotted on the x-axis can be controlled with \code{xlim}, however, the minimum value in \code{xlim} must be less than the stock length for that species.
#'
#' @param formula A formula of the form \code{~length} where \dQuote{length} generically represents a variable in \code{data} that contains length measurements.  Note that this formula can only contain one variable.
#' @param data A data.frame that minimally contains the length measurements given in the variable in the \code{formula}.
#' @param species A string that contains the species name for which Gabelhouse length categories exist.  See \code{\link{psdVal}} for details.
#' @param units A string that indicates the type of units used for the length measurements.  Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param startcat A number that indicates the beginning of the first length-class.
#' @param w A number that indicates the width of length classes to create.
#' @param justPSDQ A logical that indicates whether just stock and quality (for PSD-Q calculations) categories should be used.  If \code{FALSE} (default) then the five Gabelhouse categories will be used.
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
#' @param show.abbrevs A logical that indicates if the abbreviations for the Gabelhouse length categories should be added to the top of the plot.
#' @param psd.add A logical that indicates if the calculated PSD values should be added to the plot (Default is \code{TRUE}).
#' @param psd.pos A string that indicates the position for the PSD values will be shown.  See details in \code{\link[graphics]{legend}}.
#' @param psd.cex A numeric value that indicates the character expansion for the PSD values text.
#' @param \dots Arguments to be passed to the low-level plotting functions.
#'
#' @return None.  However, a graphic is produced.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 6-Size Structure.
#'
#' @seealso See \code{\link{psdVal}}, \code{\link{psdCalc}}, \code{\link{psdAdd}}, \code{\link{PSDlit}}, \code{\link{lencat}}, \code{\link{tictactoe}}, \code{\link{lencat}}, and \code{\link{rcumsum}} for related functionality.
#'
#' @references Ogle, D.H.  2016.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Guy, C.S., R.M. Neumann, and D.W. Willis.  2006.  New terminology for proportional stock density (PSD) and relative stock density (RSD): proportional size structure (PSS).  Fisheries 31:86-87.    [Was (is?) from http://pubstorage.sdstate.edu/wfs/415-F.pdf.]
#'
#' Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson.  2006.  Proportional size distribution (PSD): A further refinement of population size structure index terminology.  Fisheries 32:348.  [Was (is?) from http://pubstorage.sdstate.edu/wfs/450-F.pdf.]
#'
#' Willis, D.W., B.R. Murphy, and C.S. Guy.  1993.  Stock density indices: development, use, and limitations.  Reviews in Fisheries Science 1:203-222.  [Was (is?) from http://web1.cnre.vt.edu/murphybr/web/Readings/Willis\%20et\%20al.pdf.]
#'
#' @keywords hplot
#'
#' @examples
#' ## Random length data
#' # suppose this is yellow perch to the nearest mm
#' mm <- c(rnorm(100,mean=125,sd=15),rnorm(50,mean=200,sd=25),rnorm(20,mean=300,sd=40))
#' # same data to the nearest 0.1 cm
#' cm <- mm/10
#' # same data to the nearest 0.1 in
#' inch <- mm/25.4
#' # put together as data.frame
#' df <- data.frame(mm,cm,inch)
#'
#' ## Example graphics
#' op <- par(mar=c(3,3,2,1),mgp=c(1.7,0.5,0))
#' # mm data using 10-mm increments
#' psdPlot(~mm,data=df,species="Yellow perch",w=10)
#' # cm data using 1-cm increments
#' psdPlot(~cm,data=df,species="Yellow perch",units="cm",w=1)
#' # inch data using 1-in increments
#' psdPlot(~inch,data=df,species="Yellow perch",units="in",w=1)
#' # same as first with some color changes
#' psdPlot(~mm,data=df,species="Yellow perch",w=10,substock.col="gray90",stock.col="gray30")
#' # ... but without the PSD values
#' psdPlot(~mm,data=df,species="Yellow perch",w=10,psd.add=FALSE)
#' # ... demonstrate use of xlim
#' psdPlot(~mm,data=df,species="Yellow perch",w=10,xlim=c(100,300))
#' 
#' ## different subsets of fish
#' # ... without any sub-stock fish
#' brks <- psdVal("Yellow Perch")
#' tmp <- Subset(df,mm>brks["stock"])
#' psdPlot(~mm,data=tmp,species="Yellow perch",w=10)
#' # ... without any sub-stock or stock fish
#' tmp <- Subset(df,mm>brks["quality"])
#' psdPlot(~mm,data=tmp,species="Yellow perch",w=10)
#' # ... with only sub-stock, stock, and quality fish ... only PSD-Q
#' tmp <- Subset(df,mm<brks["preferred"])
#' psdPlot(~mm,data=tmp,species="Yellow perch",w=10)
#' # ... with only sub-stock fish (don't run ... setup to give an error)
#' tmp <- Subset(df,mm<brks["stock"])
#' \dontrun{ psdPlot(~mm,data=tmp,species="Yellow perch",w=10) }
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
                    psd.add=TRUE,psd.pos="topleft",psd.cex=0.75,...) {
  ## Get variable name that contains the lengths
  cl <- iGetVarFromFormula(formula,data,expNumVars=1)
  if (!is.numeric(data[,cl])) stop("Variable in 'formula' must be numeric.",call.=FALSE)
  ## make sure there is data 
  if (nrow(data)==0) stop("'data' does not contain any rows.",call.=FALSE)
  ## get psd breaks for this species
  if (justPSDQ) brks <- psdVal(species,units=units)[1:3]
    else brks <- psdVal(species,units=units)  
  ## If xlim is provided then limit temporary df to fish in range of xlim
  if (!is.null(xlim)) {
    if (min(xlim)>brks["stock"]) stop("Minimum chosen length value in 'xlim' is greater than 'stock' size.",call.=FALSE)
    data <- data[data[,cl]>=min(xlim) & data[,cl]<=max(xlim),] 
  } else data <- data
  ## make an initial histogram to get the breaks and counts
  min.brk <- min(c(data[,cl],brks),na.rm=TRUE)
  max.brk <- max(data[,cl],na.rm=TRUE)+w
  h <- graphics::hist(data[,cl],right=FALSE,breaks=seq(min.brk,max.brk,w),plot=FALSE)
  ## Create xlim values if none were given
  if (is.null(xlim)) xlim=range(h$breaks)
  ## Create colors for the bars
  clr <- ifelse(h$breaks<brks["stock"],substock.col,stock.col)
  ## Plot the histogram with the new colors
  graphics::plot(h,col=clr,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,yaxs="i",...)
  ## add psd category lines
  graphics::abline(v=brks[-1],col=psd.col,lty=psd.lty,lwd=psd.lwd)
  if (show.abbrevs) graphics::axis(3,at=brks[-1],labels=toupper(substring(names(brks)[-1],1,1)))
  ## add PSD calculations
  if (psd.add) {
    res <- iAddPSDLeg(formula,data,cl,brks,species,units,psd.pos,psd.cex)
    graphics::box()
  }
}

iAddPSDLeg <- function(formula,data,cl,brks,species,units,psd.pos,psd.cex) {
  # get PSDs
  suppressWarnings(psds <- psdCalc(formula,data,species=species,units=units,method="multinomial",what="traditional",drop0Est=FALSE))
  # reduce to only those that are >0 (drop=FALSE is needed in case it reduces to only one)
  psds <- psds[psds[,"Estimate"]>0,,drop=FALSE]
  # get two sample sizes
  n <- nrow(data)
  n.stock <- nrow(Subset(data,data[,cl]>brks["stock"]))
  # put it all together
  psdleg <- paste(c("n","n[stock]",rownames(psds)),"=",formatC(c(n,n.stock,psds[,"Estimate"]),format="f",digits=0))
  graphics::legend(psd.pos,psdleg,cex=psd.cex,box.col="white",bg="white",inset=0.002)
  return(psdleg)
}