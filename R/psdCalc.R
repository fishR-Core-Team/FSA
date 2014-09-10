#' @title Convenience function for caculating PSD-X values.
#'
#' @description Convenience function for caculating PSD-X values for all lengths in the Gabelhouse five-cell categories.
#'
#' @details This computes the PSD-X values, with associated confidence intervals, for each represented length in the Gabelhouse five-cell length categories.
#'
#' @aliases psdCalc
#'
#' @param formula A formula of the form \code{~length} where \dQuote{length} generically represents a variable in \code{data} that contains length measurements.  Note that this formula can only contain one variable.
#' @param data A data.frame that minimally contains the length measurements given in the variable in the \code{formula}.
#' @param species A string that contains the species name for which five-cell length categories exist.  See \code{\link{psdVal}} for details.
#' @param units A string that indicates the type of units used for the length measurements.  Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param addLens A numeric vector that contains minimum length definitions for additional categories.  See \code{\link{psdVal}} for details.
#' @param addNames A string vector that contains names for the additional length categories added with \code{addLens}.  See \code{\link{psdVal}} for details.
#' @param conf.level A number that indicates the level of confidence to use for constructing confidence intervals (default is \code{0.95}).
#' @param digits A numeric that indicates the number of decimals to round the result to.
#'
#' @return A matrix with columns that contain the computed PSD-X value and the associated confidence intervals.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{psdVal}}, \code{\link{psdPlot}}, \code{\link{psdDataPrep}}, \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{tictactoeAdd}}, \code{\link{lencat}}, and \code{\link{rcumsum}}.
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
#' yepdf <- data.frame(yepmm=c(rnorm(100,mean=125,sd=15),rnorm(50,mean=200,sd=25),
#'                     rnorm(20,mean=300,sd=40)),
#'                     species=rep("Yellow Perch",170))
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",units="mm",digits=1)
#'
#' ## all values above stock value (troubleshooting a problem)
#' yepdf2 <- subset(yepdf,yepmm>=130)
#' psdCalc(~yepmm,data=yepdf2,species="Yellow perch",units="mm",digits=1)
#' 
#' ## all values above quality value (troubleshooting a problem)
#' yepdf3 <- subset(yepdf,yepmm>=200)
#' psdCalc(~yepmm,data=yepdf3,species="Yellow perch",units="mm",digits=1)
#' 
#' @export psdCalc
psdCalc <- function(formula,data,species="List",units=c("mm","cm","in"),
                    addLens=NULL,addNames=NULL,conf.level=0.95,
                    digits=getOption("digits")) {
  units <- match.arg(units)
  ## check if the data.frame has data
  if (nrow(data)==0) stop("'data' does not contain any rows.",call.=FALSE)
  ## get name of length variable from the formula
  cl <- iGetVarFromFormula(formula,data,expNumVars=1)
  ## find psd lengths for this species     
  psdbrks <- psdVal(species,units=units,incl.zero=FALSE,addLens=addLens,addNames=addNames)
  ## restrict data to above the stock length
  print(psdbrks)
  dftemp <- data[data[,cl]>=psdbrks[1],]
  print(dftemp)
  # if nothing in data.frame then send error
  if (nrow(dftemp)==0) stop("There are no stock-length fish in the sample.",call.=FALSE)
  ## add the length categorization variable, dropping unused levels
  dftemp <- lencat(formula,data=dftemp,breaks=psdbrks,vname="lcatr",use.names=TRUE,drop.levels=TRUE)
  ## get reverse cumulative sum table and number of stock fish
  rcum <- rcumsum(table(dftemp$lcatr))
  # if rcum does not have the stock length then a value must be added
  if (names(rcum)[1]!="stock") rcum <- c(rcum[1],rcum)
  n.stock <- rcum[1]
  ## make PSD calculations
  psds <- (rcum[-1]/n.stock)*100
  ## get CIs
  cis <- 100*binCI(as.vector(rcum[-1]),n.stock,conf.level=conf.level)
  # put together
  res <- cbind(psds,cis)
  # remove temporary data.frame
  rm(dftemp)
  # label
  rownames(res) <- iPSDLabels(rownames(res))
  colnames(res)[1] <- "Estimate" 
  round(res,digits)
}

##############################################################
# INTERNAL function to create labels for the PSD values
##############################################################
iPSDLabels <- function(psdbrks) {
  # check if any breaks are labeled with numeric values
  suppressWarnings(psdnums <- which(!is.na(as.numeric(names(psdbrks)))))
  # convert breaks names to one letter
  psdnms <- toupper(substring(psdbrks,1,1))
  # but put numeric labels back in
  psdnms[psdnums] <- psdbrks[psdnums]
  # add on PSD prefix and return
  paste("PSD-",psdnms,sep="")
}
