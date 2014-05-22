#'Convenience function for caculating PSD-X values.
#'
#'Convenience function for caculating PSD-X values for all lengths in the Gabelhouse five-cell categories.
#'
#'This computes the PSD-X values, with associated confidence intervals, for each represented length in the Gabelhouse five-cell length categories.
#'
#' @aliases psdCalc pssCalc
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
#' @source Guy, C.S., R.M. Neumann, and D.W. Willis.  2006.  New terminology for proportional stock density (PSD) and relative stock density (RSD): proportional size structure (PSS).  Fisheries 31:86-87. \url{http://pubstorage.sdstate.edu/wfs/415-F.pdf}
#'
#'Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson.  2006.  Proportional size distribution (PSD): A further refinement of population size structure index terminology.  Fisheries 32:348. \url{http://www.montana.edu/mtcfru/Guy/Publication\%20pdf/PSD\%20pub.pdf}
#'
#'Willis, D.W., B.R. Murphy, and C.S. Guy.  1993.  Stock density indices: development, use, and limitations.  Reviews in Fisheries Science 1:203-222. \url{http://web1.cnre.vt.edu/murphybr/web/Readings/Willis\%20et\%20al.pdf}
#'
#' @keywords hplot
#'
#' @examples
#'## Random length data
#'# suppose this is yellow perch to the nearest mm
#'yepdf <- data.frame(yepmm=c(rnorm(100,mean=125,sd=15),rnorm(50,mean=200,sd=25),
#'                    rnorm(20,mean=300,sd=40)))
#'
#'psdCalc(~yepmm,data=yepdf,species="Yellow perch",units="mm",digits=1)
#'
#' @export psdCalc pssCalc
psdCalc <- pssCalc <- function(formula,data,species="List",units=c("mm","cm","in"),
                               addLens=NULL,addNames=NULL,conf.level=0.95,
                               digits=getOption("digits")) {
  # INTERNAL function
  psdLabels <- function(psdbrks) {
    # get rid of zero and stock names
    psdbrks <- psdbrks[-c(1,2)]
    # check if any breaks are labeled with numeric values
    suppressWarnings(psdnums <- which(!is.na(as.numeric(names(psdbrks)))))
    # convert breaks names to one letter
    psdnms <- toupper(substring(names(psdbrks),1,1))
    # but put numeric labels back in
    psdnms[psdnums] <- names(psdbrks)[psdnums]
    # add on PSD prefix and return
    paste("PSD-",psdnms,sep="")
  }  # end internal function
  
  # MAIN FUNCTION
  units <- match.arg(units)
  cl <- getVarFromFormula(formula,data,expNumVars=1)
  # find psd lengths for this species     
  psdbrks <- psdVal(species,units=units,incl.zero=TRUE,addLens=addLens,addNames=addNames)
  dftemp <- lencat(formula,data=data,breaks=psdbrks,vname="lcatr")
  # make RSD calculations
  rcum <- rcumsum(table(dftemp$lcatr))
  n.stock <- rcum[2]
  rcum <- rcum[-c(1,2)]
  rsds <- (rcum/n.stock)*100
  # get CIs
  cis <- 100*binCI(as.vector(rcum),n.stock,conf.level=conf.level)
  # put together
  res <- cbind(rsds,cis)
  # remove temporary data.frame
  rm(dftemp)
  # label
  rownames(res) <- psdLabels(psdbrks)[1:length(rsds)]
  colnames(res)[1] <- "Value" 
  round(res,digits)
}
