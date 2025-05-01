#' @title Length-frequency histogram with Gabelhouse lengths highlighted.
#'
#' @description Constructs a length-frequency histogram with Gabelhouse lengths highlighted.
#'
#' @details Constructs a length-frequency histogram with the stock-sized fish highlighted, the Gabelhouse lengths marked by vertical lines, and the (traditional) PSD-X values superimposed. The length of fish plotted on the x-axis can be controlled with \code{xlim}, however, the minimum value in \code{xlim} must be less than the stock length for that species.
#' 
#' This plot is meant to be illustrative and not of “publication-quality.” Thus, only some aspects of the plot can be modified to change its appearance.
#' 
#' See examples and \href{https://fishr-core-team.github.io/FSA/articles/Computing_PSDs.html}{this article} for a demonstration.
#'
#' @param formula A formula of the form \code{~length} where \dQuote{length} generically represents a variable in \code{data} that contains length measurements. Note that this formula can only contain one variable.
#' @param data A data.frame that minimally contains the length measurements given in the variable in the \code{formula}.
#' @param species A string that contains the species name for which Gabelhouse length categories exist. See \code{\link{psdVal}} for details.
#' @param group A string that contains the sub-group of \code{species} for which to find the Gabelhouse lengths; e.g., things like \dQuote{landlocked}, \dQuote{lentic}.
#' @param units A string that indicates the type of units used for the length measurements. Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param startcat A number that indicates the beginning of the first length-class.
#' @param w A number that indicates the width of length classes to create.
#' @param justPSDQ A logical that indicates whether just stock and quality (for PSD-Q calculations) categories should be used. If \code{FALSE} (default) then the five Gabelhouse categories will be used.
#' @param main A string that serves as the main label for the histogram.
#' @param xlab A string that serves as the label for the x-axis.
#' @param ylab A string that serves as the label for the y-axis.
#' @param xlim A numeric vector of length two that indicates the minimum and maximum values (i.e., fish lengths) for the x-axis.
#' @param ylim A numeric vector of length two that indicates the minimum and maximum values for the y-axis.
#' @param substock.col A string that indicates the color to use for the bars representing under-stock size fish.
#' @param stock.col A string that indicates the color to use for the bars representing stock size fish.
#' @param psd.col A string that indicates the color to use for the vertical lines at the Gabelhouse length category values.
#' @param psd.lty A numeric that indicates the line type to use for the vertical lines at the Gabelhouse length category values.
#' @param psd.lwd A numeric that indicates the line width to use for the vertical lines at the Gabelhouse length category values.
#' @param show.abbrevs A logical that indicates if the abbreviations for the Gabelhouse length categories should be added to the top of the plot.
#' @param psd.add A logical that indicates if the calculated PSD values should be added to the plot (default is \code{TRUE}).
#' @param psd.pos A string that indicates the position for where the PSD values will be shown. See details in \code{\link[graphics]{legend}}.
#' @param psd.cex A numeric value that indicates the character expansion for the PSD values text.
#' @param \dots Arguments to be passed to the low-level plotting functions.
#'
#' @return None. However, a graphic is produced.
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: 6-Size Structure.
#'
#' @seealso See \code{\link{psdVal}}, \code{\link{psdCalc}}, \code{\link{psdAdd}}, \code{\link{PSDlit}}, \code{\link{lencat}}, \code{\link{tictactoe}}, \code{\link{lencat}}, and \code{\link{rcumsum}} for related functionality.
#'
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Guy, C.S., R.M. Neumann, and D.W. Willis. 2006. New terminology for proportional stock density (PSD) and relative stock density (RSD): proportional size structure (PSS). Fisheries 31:86-87. [Was (is?) from http://pubstorage.sdstate.edu/wfs/415-F.pdf.]
#'
#' Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson. 2006. Proportional size distribution (PSD): A further refinement of population size structure index terminology. Fisheries 32:348. [Was (is?) from http://pubstorage.sdstate.edu/wfs/450-F.pdf.]
#'
#' Willis, D.W., B.R. Murphy, and C.S. Guy. 1993. Stock density indices: development, use, and limitations. Reviews in Fisheries Science 1:203-222. [Was (is?) from http://web1.cnre.vt.edu/murphybr/web/Readings/Willis\%20et\%20al.pdf.]
#'
#' @keywords hplot
#'
#' @examples
#' #===== Random length data for Yellow Perch (for example) to the nearest mm
#' set.seed(633437)
#' yepdf <- data.frame(yepmm=round(c(rnorm(100,mean=125,sd=15),
#'                                   rnorm(50,mean=200,sd=25),
#'                                   rnorm(20,mean=270,sd=40)),0),
#'                     species=rep("Yellow Perch",170))
#'
#' #===== Example graphics
#' op <- par(mar=c(3,3,2,1),mgp=c(1.7,0.5,0))
#' #----- Using 10-mm increments
#' psdPlot(~yepmm,data=yepdf,species="Yellow Perch",w=10)
#' psdPlot(~yepmm,data=yepdf,species="Yellow Perch",w=10,substock.col="gray90",
#'         stock.col="gray30")
#' #----- Same, but without the PSD values
#' psdPlot(~yepmm,data=yepdf,species="Yellow Perch",w=10,psd.add=FALSE)
#' par(op)
#'
#' @export psdPlot
psdPlot <- function(formula,data,species="List",group=NULL,units=c("mm","cm","in"),
                    startcat=0,w=1,justPSDQ=FALSE,
                    main="",xlab="Length",ylab="Number",
                    xlim=NULL,ylim=c(0,max(h$counts)*1.05),
                    substock.col="white",stock.col="gray90",
                    psd.col="black",psd.lty=2,psd.lwd=1,
                    show.abbrevs=TRUE,
                    psd.add=TRUE,psd.pos="topleft",psd.cex=0.75,...) {
  ## Get variable name that contains the lengths
  cl <- iGetVarFromFormula(formula,data,expNumVars=1)
  if (!is.numeric(data[,cl])) STOP("Variable in 'formula' must be numeric.")
  ## make sure there is data 
  if (nrow(data)==0) STOP("'data' does not contain any rows.")
  ## get psd breaks for this species
  if (justPSDQ) brks <- psdVal(species,group,units=units)[1:3]
    else brks <- psdVal(species,group,units=units)  
  ## If xlim is provided then limit temporary df to fish in range of xlim
  if (!is.null(xlim)) {
    if (min(xlim)>brks["stock"])
      STOP("Minimum length value in 'xlim' is greater than 'stock' size.")
    data <- data[data[,cl]>=min(xlim) & data[,cl]<=max(xlim),] 
  } else data <- data
  ## make an initial histogram to get the breaks and counts
  # nocov start
  min.brk <- min(c(data[,cl],brks),na.rm=TRUE)
  max.brk <- max(data[,cl],na.rm=TRUE)+w
  h <- graphics::hist(data[,cl],right=FALSE,breaks=seq(min.brk,max.brk,w),
                      plot=FALSE)
  ## Create xlim values if none were given
  if (is.null(xlim)) xlim <- range(h$breaks)
  ## Create colors for the bars
  clr <- ifelse(h$breaks<brks["stock"],substock.col,stock.col)
  ## Plot the histogram with the new colors
  graphics::plot(h,col=clr,xlim=xlim,ylim=ylim,main=main,
                 xlab=xlab,ylab=ylab,yaxs="i",...)
  ## add psd category lines
  graphics::abline(v=brks[-1],col=psd.col,lty=psd.lty,lwd=psd.lwd)
  if (show.abbrevs) graphics::axis(3,at=brks[-1],
                    labels=toupper(substring(names(brks)[-1],1,1)))
  ## add PSD calculations
  if (psd.add) {
    res <- iAddPSDLeg(formula,data,cl,brks,species,units,psd.pos,psd.cex)
  }
  graphics::box()
  # nocov end
}

iAddPSDLeg <- function(formula,data,cl,brks,species,units,psd.pos,psd.cex) { # nocov start
  # get PSDs
  suppressWarnings(psds <- psdCalc(formula,data,species=species,
                                   units=units,method="multinomial",
                                   what="traditional",drop0Est=FALSE))
  # reduce to only those that are >0 (drop=FALSE is needed
  #    in case it reduces to only one)
  psds <- psds[psds[,"Estimate"]>0,,drop=FALSE]
  # get two sample sizes
  n <- nrow(data)
  n.stock <- nrow(droplevels(subset(data,data[,cl]>brks["stock"])))
  # put it all together
  psdleg <- paste(c("n","n[stock]",rownames(psds)),"=",
                  formatC(c(n,n.stock,psds[,"Estimate"]),
                          format="f",digits=0))
  graphics::legend(psd.pos,psdleg,cex=psd.cex,box.col="white",
                   bg="white",inset=0.002)
  psdleg
} # nocov end
