#' @title Add Gabelhouse lengths for each species in an entire data frame.
#'
#' @description Adds the Gabelhouse lengths specific to a species to all individuals in an entire data frame.  The concept is to save the user from having to split each species into a separate data frame, then create the required length categorization variable, and then bind the separate data frames back together.
#'
#' @details The data frame provided in \code{data} must contain one column of length measurements and one column of species names.  All lengths must be in the same units.  Any species name that is not recognized as being a species with known Gabelhouse lengths will be considered as an \dQuote{other} type of fish (see below for how these fish can be handled).  Species that are known to have Gabelhouse length can be found with \code{\link{psdVal}()} (i.e., without any arguments).  Spelling and capitalization of species names have to be exactly (except for differences in capitalization) the same as in \code{data(PSDlit)}.  See \code{\link{recodeSpecies}} for one method to change species names before using this function).
#'
#' A new variable will be created in the returned data frame that contains the Gabelhouse lengths that correspond to the observed lengths.  If \code{use.names=TRUE} (the default) then the \dQuote{levels} of the new variable will be the names of the Gabelhouse length categories -- i.e., \dQuote{stock}, \dQuote{Quality}, etc., with the possibility that \dQuote{sub-stock} length fish will be labeled with \dQuote{zero}.  If \code{use.names=FALSE} then the levels of the new variable will be the numeric value representing each Gabelhouse length, again with the possibility that \dQuote{sub-stock} fish will be given a value of zero.
#'
#' The default behavior is for \dQuote{sub-stock} fish and \dQuote{other} fish (see above for the definition of an \dQuote{Other} fish) to be included in the resulting data frame, but with a \code{0} or \code{zero} in the new length categorization variable for the sub-stock fish and a \code{NA} in the new length categorization variable for the \dQuote{other} fish.  The \dQuote{sub-stock} fish can be excluded from the final data frame with \code{remove.substock=TRUE} and the \dQuote{other} fish can be excluded with \code{remove.other=TRUE}.
#'
#' @aliases psdDataPrep

#' @param formula A formula of the form \code{length~species} where \dQuote{length} generically represents a variable that contains length measurements and \dQuote{species} generically represents a variable that contains species names.  Note that this formula can only contain two variables and must have the length variable on the left-hand-side and the species variable on the right-hand-side.
#' @param data A data.frame that minimally contains the length measurements and species names given in the variables in the \code{formula}.
#' @param vname A string that contains the name for the new length category variable (defaults to \dQuote{PSDcat}).
#' @param units A string that indicates the type of units used for the lengths.  Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param remove.substock A logical that indicates whether to exclude individual fish with lengths that are less than the \dQuote{stock} size for that species.  See details.
#' @param remove.other A logical that indicates whether to exclude all individuals of a fish species for which the Gabelhous lengths do not exist in \code{data(PSDlit)}.  See details.
#' @param use.names A logical that indicates whether the categories returned in \code{vname} are numeric (\code{=FALSE}) or word (\code{=TRUE}; default) representations of the Gabelhouse lengths.  See details.
#'
#' @return The original data frame in \code{data} but with a new variable \code{vname} appended that contains the PSD-related length categories.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @seealso \code{\link{psdVal}}, \code{\link{psdCalc}}, \code{\link{psdPlot}}, \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{lencat}}, \code{\link{rcumsum}}, \code{\link{recodeSpecies}}, and \code{\link{wrDataPrep}}.
#'
#' @section fishR vignette: none yet.
#' 
#' @references
#' Guy, C.S., R.M. Neumann, and D.W. Willis.  2006.  \href{http://pubstorage.sdstate.edu/wfs/415-F.pdf}{New terminology for proportional stock density (PSD) and relative stock density (RSD): proportional size structure (PSS).}  Fisheries 31:86-87.  
#'
#' Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson.  2006.  \href{http://www.montana.edu/mtcfru/Guy/Publication\%20pdf/PSD\%20pub.pdf}{Proportional size distribution (PSD): A further refinement of population size structure index terminology.}  Fisheries 32:348.
#'
#' Willis, D.W., B.R. Murphy, and C.S. Guy.  1993.  \href{http://web1.cnre.vt.edu/murphybr/web/Readings/Willis\%20et\%20al.pdf}{Stock density indices: development, use, and limitations.}  Reviews in Fisheries Science 1:203-222. 
#'
#' @keywords manip
#' 
#' @examples
#' ## Create random data for three species
#' set.seed(345234534)  # only for repeatability
#' dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
#' dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
#' dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
#' dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
#' dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
#' dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
#' d <- rbind(dbg,dlb,dbt)
#' str(d)
#'
#' # Rename variables to match PSDlit -- see recodeSpecies()
#' d2 <- recodeSpecies(~species,data=d,c("LMB"),c("Largemouth Bass"))
#' levels(d2$species1)
#'
#' # Example where species without PSD values and sub-stock fish are retained (the defaults)
#' d3 <- psdDataPrep(tl~species1,data=d2,units="mm")
#' str(d3)
#' xtabs(~species1+PSDcat,data=d3)
#'
#' # Example where species without known PSD values are removed, but sub-stock fish are not
#' d4 <- psdDataPrep(tl~species1,data=d2,units="mm",remove.other=TRUE)
#' str(d4)
#' with(d4,table(species1,PSDcat))
#'
#' # Example where both sub-stock and species without known PSD values are excluded
#' d5 <- psdDataPrep(tl~species1,data=d2,units="mm",remove.substock=TRUE,remove.other=TRUE)
#' str(d5)
#' with(d5,table(species1,PSDcat))
#' with(d5,table(species1)) 
#'
#' # Example where category names are not used
#' d6 <- psdDataPrep(tl~species1,data=d2,units="mm",use.names=FALSE)
#' str(d6)
#' with(d6,table(species1,PSDcat))
#'
#' @export
psdDataPrep <- function(formula,data,vname="PSDcat",
                        units=c("mm","cm","in"),
                        remove.substock=FALSE,remove.other=FALSE,
                        use.names=TRUE) {
  ## Some checks
  units <- match.arg(units)
  # get variable names and check data type
  cl <- iGetVarFromFormula(formula,data,expNumVars=2)
  cspp <- cl[2]
  if (is.numeric(data[,cspp])) stop("Variable on RHS of 'formula' is numeric (and, thus, not species names).",call.=FALSE)
  cl <- cl[1]
  if (!is.numeric(data[,cl])) stop("Variable on LHS of 'formula' is non-numeric (and, thus, not fish lengths).",call.=FALSE)
  ## get the PSD literature values data frame
  PSDlit <- get(data("PSDlit", envir = environment()), envir = environment())
  ## if supposed to remove substock fish then don't include zero in PSD breaks
  incl.zero <- !remove.substock
  ## what species exist in the data frame
  specs <- levels(data[,cspp])
  # which species in the data frame are and are not found in the PSDlit list
  areLogical <- specs %in% levels(PSDlit$species)
  noPSD <- specs[which(!areLogical)]
  if (length(noPSD)>0) warning("These species do not have known Gabelhouse lengths:\n ",paste(noPSD,collapse=", "),call.=FALSE)
  specs <- specs[which(areLogical)]
  
  # cycle through each species where PSD values are known
  for (i in specs) {
    # get the Gabelhouse length categories
    glhse <- psdVal(i,units=units,incl.zero=incl.zero)
    # isolate the current species
    idf <- data[data[,cspp]==i,]
    # if incl.zero not used then eliminate all fish below stock size
    if (!incl.zero) idf <- idf[idf[,cl]>=glhse[1],]
    # add the Gabelhouse length categories to the data frame
    idf <- lencat(as.formula(paste("~",cl,sep="")),data=idf,breaks=glhse,vname=vname,use.names=use.names)
    # bind current species to the new data frame being created
    if (i==specs[1]) ndf <- idf
      else ndf <- rbind(ndf,idf)
  }
  # add back in "other" fish if asked not to remove
  if (!remove.other & length(noPSD)>0) {
    odf <- data[data[,cspp] %in% noPSD,]
    odf[,vname] <- NA
    ndf <- rbind(ndf,odf)
  }
  # factor the species variable so that it will remove other species names if they are not include
  ndf[,cspp] <- factor(ndf[,cspp])
  # return the new data frame
  ndf
}