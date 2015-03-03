#' @title Convenience function for calculating PSD-X and PSD X-Y values.
#'
#' @description Convenience function for calculating (traditional) PSD-X and (incremental) PSD X-Y values for all Gabelhouse lengths and increments thereof.
#'
#' @details Computes the (traditional) PSD-X and (incremental) PSD X-Y values, with associated confidence intervals, for each Gabelhouse length.  All PSD-X and PSD X-Y values are printed if \code{what="all"} (DEFAULT), only PSD-X values are printed if \code{what="traditional"}, only PSD X-Y values are printed if \code{what="incremental"}, and nothing is printed (but the matrix is still returned) if \code{what="none"}.
#' 
#' Confidence intervals can be computed with either the multinomial (Default) or binomial distribution as set in \code{method}.  See details in \code{\link{psdCI}} for more information.
#'
#' @aliases psdCalc
#'
#' @param formula A formula of the form \code{~length} where \dQuote{length} generically represents a variable in \code{data} that contains the observed lengths.  Note that this formula may only contain one variable and it must be numeric.
#' @param data A data.frame that minimally contains the observed lengths given in the variable in \code{formula}.
#' @param species A string that contains the species name for which Gabelhouse lengths exist.  See \code{\link{psdVal}} for details.
#' @param units A string that indicates the type of units used for the lengths.  Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param what A string that indicates the type of PSD values that will be printed.  See details.
#' @param drop0Est A logical that indicates whether the PSD values that are zero should be dropped from the output.
#' @param method A character that identifies the confidence interval method to use.  See details in \code{\link{psdCI}}.
#' @param addLens A numeric vector that contains minimum lengths for additional categories.  See \code{\link{psdVal}} for details.
#' @param addNames A string vector that contains names for the additional lengths added with \code{addLens}.  See \code{\link{psdVal}} for details.
#' @param justAdds A logical that indicates whether just the values related to the length sin \code{addLens} should be returned.
#' @param conf.level A number that indicates the level of confidence to use for constructing confidence intervals (default is \code{0.95}).
#' @param showIntermediate A logical that indicates whether the number of fish in the category and the number of stock fish (i.e., \dQuote{intermediate} values) should be included in the returned matrix.  Default is to not include these values.
#' @param digits A numeric that indicates the number of decimals to round the result to.  Default is zero digits following the recommendation of Neumann and Allen (2007).
#'
#' @return A matrix with columns that contain the computed PSD-X or PSD X-Y values and associated confidence intervals.  If \code{showIntermediate=TRUE} then the number of fish in the category and the number of stock fish will also be shown.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{psdVal}}, \code{\link{psdPlot}}, \code{\link{psdAdd}}, \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{lencat}}, and \code{\link{rcumsum}}.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/SizeStructure.pdf}
#' 
#' @section Testing: Point estimate calculations match those constructed "by hand."
#'
#' @references
#' Guy, C.S., R.M. Neumann, and D.W. Willis.  2006.  \href{http://pubstorage.sdstate.edu/wfs/415-F.pdf}{New terminology for proportional stock density (PSD) and relative stock density (RSD): proportional size structure (PSS).}  Fisheries 31:86-87.  
#'
#' Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson.  2006.  \href{http://www.montana.edu/mtcfru/Guy/Publication\%20pdf/PSD\%20pub.pdf}{Proportional size distribution (PSD): A further refinement of population size structure index terminology.}  Fisheries 32:348.
#' 
#' Neumann, R. M. and Allen, M. S.  2007.  Size structure. In Guy, C. S. and Brown, M. L., editors, Analysis and Interpretation of Freshwater Fisheries Data, Chapter 9, pages 375-421. American Fisheries Society, Bethesda, MD.
#'
#' Willis, D.W., B.R. Murphy, and C.S. Guy.  1993.  \href{http://web1.cnre.vt.edu/murphybr/web/Readings/Willis\%20et\%20al.pdf}{Stock density indices: development, use, and limitations.}  Reviews in Fisheries Science 1:203-222. 
#'
#' @keywords hplot
#'
#' @examples
#' ## Random length data
#' # suppose this is yellow perch to the nearest mm
#' yepdf <- data.frame(yepmm=round(c(rnorm(100,mean=125,sd=15),rnorm(50,mean=200,sd=25),
#'                     rnorm(20,mean=300,sd=40)),0),
#'                     species=rep("Yellow Perch",170))
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",digits=1)
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",digits=1,drop0Est=TRUE)
#'
#' ## all values above stock value (troubleshooting a problem)
#' yepdf2 <- subset(yepdf,yepmm>=130)
#' psdCalc(~yepmm,data=yepdf2,species="Yellow perch")
#' 
#' ## all values above quality value (troubleshooting a problem)
#' yepdf3 <- subset(yepdf,yepmm>=200)
#' psdCalc(~yepmm,data=yepdf3,species="Yellow perch")
#' psdCalc(~yepmm,data=yepdf3,species="Yellow perch",drop0Est=TRUE)
#' 
#' ## all values below memorable value (troubleshooting a problem)
#' yepdf4 <- subset(yepdf,yepmm<300)
#' psdCalc(~yepmm,data=yepdf4,species="Yellow perch")
#' 
#' ## add a length
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=150)
#' 
#' ## add lengths with names
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=150,addNames="minLen")
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=c("minLen"=150))
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=c(150,275),addNames=c("minSlot","maxSlot"))
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=c("minLen"=150,"maxslot"=275))
#' 
#' ## add lengths with names, return just those values that use those lengths
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=c("minLen"=150),justAdds=TRUE)
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=c("minLen"=150),justAdds=TRUE,what="traditional")
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=c(150,275),addNames=c("minSlot","maxSlot"),justAdds=TRUE)
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=c(150,275),addNames=c("minSlot","maxSlot"),justAdds=TRUE,what="traditional")
#' 
#' ## different output types
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=150,what="traditional")
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=150,what="incremental")
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",addLens=150,what="none")
#' 
#' ## Show intermediate values
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",showInterm=TRUE)
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",what="traditional",showInterm=TRUE)
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",what="incremental",showInterm=TRUE)
#' 
#' ## Control the digits
#' psdCalc(~yepmm,data=yepdf,species="Yellow perch",digits=1)
#' 
#' @export psdCalc
psdCalc <- function(formula,data,species,units=c("mm","cm","in"),
                    method=c("multinomial","binomial"),conf.level=0.95,
                    addLens=NULL,addNames=NULL,justAdds=FALSE,
                    what=c("all","traditional","incremental","none"),drop0Est=TRUE,
                    showIntermediate=FALSE,digits=0) {
  method <- match.arg(method)
  what <- match.arg(what)
  units <- match.arg(units)
  ## make sure species is not missing
  if (missing(species)) stop("Must include a species name in 'species'.",call.=FALSE)
  ## find psd lengths for this species
  brks <- psdVal(species,units=units,incl.zero=FALSE,addLens=addLens,addNames=addNames)
  ## perform checks and initial preparation of the data.frame
  dftemp <- iPrepData4PSD(formula,data,brks["stock"],units)
  ## add the length categorization variable, don't drop unused levels
  dftemp <- lencat(formula,data=dftemp,breaks=brks,vname="lcatr",use.names=TRUE,droplevels=FALSE)
  ## get sample size (number of stock-length fish)
  n <- nrow(dftemp)
  ## make the proportions table
  ptbl <- prop.table(table(dftemp$lcatr))
  ## compute all traditional and interval PSD values
  res <- iGetAllPSD(ptbl,n=n,method=method,conf.level=conf.level,digits=digits)
  ## decided to keep intermediate calculation columns or not (in first two columns)
  if (!showIntermediate) res <- res[,-c(1:2)]
  ## return result
  k <- length(ptbl)
  switch(what,
         all=         {  },
         traditional= { res <- res[1:(k-1),] },
         incremental= { res <- res[k:nrow(res),] }
         )
  ## Drop estimates that are zero if requested to do so
  if (drop0Est) res <- res[res[,"Estimate"]>0,]
  ## Return just the additional lengths if requested to do so
  if (justAdds & !is.null(addLens)) {
    # add names to the additional lengths
    addLens <- iHndlAddNames(addLens,addNames)
    # find which rows in the result vector contain the 
    # additional lengths that the user is asking for
    tmp <- NULL
    for (i in names(addLens)) tmp <- c(tmp,grep(i,rownames(res)))
    res <- res[sort(tmp),]
  }
  ## Invisibly return the matrix if requested to do so
  if (what=="none") invisible(res)
    else round(res,digits)
}

# ============================================================
# Internal function to prepare the data.frame for ease of 
#   computing the PSD values.  Performs some checks and 
#   deletes the sub-stock fish.
# ============================================================
iPrepData4PSD <- function(formula,data,stock.len,units) {
  ## check if the data.frame has data
  if (nrow(data)==0) stop("'data' does not contain any rows.",call.=FALSE)
  ## get name of length variable from the formula
  cl <- iGetVarFromFormula(formula,data,expNumVars=1)
  if (!is.numeric(data[,cl])) stop("Variable in 'formula' must be numeric.",call.=FALSE)
  ## restrict data to above the stock length
  data <- data[data[,cl]>=stock.len,]
  ## assure that NA values in the length variable are removed
  data <- data[!is.na(data[,cl]),]
  # if nothing in data.frame then send error
  if (nrow(data)==0) {
    msg <- "There are no stock-length fish in the sample.\n"
    msg <- paste0(msg,"Note that units='",units,"' was used.\n")
    stop(msg,call.=FALSE)
  }
  # return new data.frame
  data
}

# ============================================================
# INTERNAL functions that will compute all available
#   traditional and incremental PSD values.  A matrix of
#   PSD values and confidence intervals will be returned.
# ============================================================
iMakePSDLabels <- function(nms) {
  # check if any names are not Gabelhouse names
  tmp <- which(!(nms %in% c("stock","quality","preferred","memorable","trophy")))
  # convert breaks names to one letter
  abb <- toupper(substring(nms,1,1))
  # but put non-Gabelhouse names back in
  abb[tmp] <- nms[tmp]
  # return abbreviations
  abb
}

iMakePSDIV <- function(ptbl) {
  ## Get number of categories
  k <- length(ptbl)
  ## Get category name abbreviations
  abb <- iMakePSDLabels(names(ptbl))
  ## make matrix for traditional PSDs
  tmp1 <- matrix(0,nrow=k-1,ncol=k)
  tmp1[upper.tri(tmp1)] <- 1
  rownames(tmp1) <- paste("PSD",abb[-1],sep="-")
  ## make identify matrix for incremental PSD
  tmp2 <- matrix(0,nrow=k-1,ncol=k)
  diag(tmp2) <- 1
  rownames(tmp2) <- paste("PSD ",abb[1:(k-1)],"-",abb[2:k],sep="")
  ## put together and return
  rbind(tmp1,tmp2)
}

iGetAllPSD <- function(ptbl,n,method,conf.level=0.95,digits) {
  ## Make a matrix of indicator variables for all PSDs
  id1 <- iMakePSDIV(ptbl)
  ## check if sample size is >20 (see Brenden et al. 2008), warn if not
  # do this here and suppress warnings for psdCI so that there is only one warning
  ns <- n*ptbl
  if (any(ns>0 & ns<20)) warning("Some category sample size <20, some CI coverage may be\n lower than ",100*conf.level,"%.",call.=FALSE)
  ## Compute all PSDs
  suppressWarnings(res <- t(apply(id1,MARGIN=1,FUN=psdCI,ptbl=ptbl,n=n,method=method,conf.level=conf.level,digits=digits)))
  ## Add the numerator (number in category) and denominator (stock) columns
  res <- cbind(res[,1]*n/100,n,res)
  colnames(res) <- c("num","stock","Estimate",iCILabel(conf.level))
  res
}
