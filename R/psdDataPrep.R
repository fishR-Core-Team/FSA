#'Add five-cell length categories specific to a species to an entire data frame.
#'
#'This function adds the five-cell length categories specific to a species to all
#'individuals in an entire data frame.  The concept is to save the user from having
#'to split each species into a separate data frame and then creating the required
#'length categorization variable.
#'
#'This function requires that the data frame provided in \code{data} contains one
#'column of length measurementsfish and one column of species names.  All lengths
#'must be in the same units.  Any species name that is not recognized as being a
#'species with known five-cell length category values (and stored in \code{data(PSDlit)})
#'will be thought of as an \dQuote{other} type of fish (see below for how these
#'fish can be handled).  Species that are known to have the five-cell length categories
#'can be found with \code{\link{psdVal}()} (i.e., using that function without any
#'arguments).  Note that spelling and capitalization have to be exactly the same
#'(see \code{\link{recodeSpecies}} for one method for changing species names before
#'using this function).
#'
#'This function will create a new variable in the returned data frame that contains
#'the five-cell length categories that correspond to the given lengths.
#'If \code{use.catnames=TRUE} (the default) then the \dQuote{levels} of the new 
#'variable will be the word names of the five-cell categories -- i.e., \dQuote{stock},
#'\dQuote{Quality}, etc. with the possibility that \dQuote{sub-stock} length fish
#'will be labelled with \dQuote{zero}.  If \code{use.catnames=FALSE} then the levels
#'of the new variable will be the numeric value representing the minimum value
#'in each five-cell category, again with the possibility that \dQuote{sub-stock} fish
#'will be given a value of zero.
#'
#'The default is for \dQuote{sub-stock} fish and \dQuote{other} fish (see above
#'for the definition of an \dQuote{Other} fish) to be included in the resulting
#'data frame, but with a \code{0} or \code{zero} in the new length categorization
#'variable for the sub-stock fish and a \code{NA} in the new length categorization
#'variable for the \dQuote{other} fish.  The \dQuote{sub-stock} fish can be excluded
#'from the final data frame by using \code{remove.substock=TRUE} and the \dQuote{other}
#'fish can be excluded by using \code{remove.other=TRUE}.
#'
#'@aliases psdDataPrep pssDataPrep
#'@param formula A formula of the form \code{length~species} where \dQuote{length}
#'generically represents a variable in \code{data} that contains length measurements
#'and \dQuote{species} generically represents a variable in \code{data} that contains
#'species names.  Note that this formula can only contain two variables and they
#'must be in the length first, species second order.
#'@param data A data.frame that minimally contains the length measurements and species
#'names given in the variables in the \code{formula}.
#'@param psdname A string that contains the name for the new length category variable 
#'(defaults to \dQuote{PSDcat}).
#'@param units A string indicating the type of units used for the length measurements.
#'Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and 
#'\code{in} for inches.
#'@param remove.substock A logical indicating whether to exclude individual fish
#'with length measurements that are less than the \dQuote{stock} size for that species.
#'See details.
#'@param remove.other A logical indicating whether to exclude all individuals for a
#'fish species for which the five-cell length categories do not exist in \code{data(PSDlit)}.
#'See details.
#'@param use.catnames A logical indicating whether the categories returned in \code{psdname}
#'are numeric (\code{=FALSE}) or word (\code{=TRUE}; default) representations of the
#'five-cell categories.  See details.
#'@return The original data frame in \code{data} but with the new variable \code{psdname}
#'which contains the PSD-related length categories.
#'@seealso \code{\link{psdVal}}, \code{\link{psdCalc}}, \code{\link{psdPlot}}, 
#' \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{tictactoeAdd}},
#' \code{\link{lencat}}, \code{\link{rcumsum}}, \code{\link{recodeSpecies}},
#' and \code{\link{wrDataPrep}}.
#'@section fishR vignette: none yet.
#'@source Guy, C.S., R.M. Neumann, and D.W. Willis.  2006.  New terminology for
#'proportional stock density (PSD) and relative stock density (RSD): proportional
#'size structure (PSS).  Fisheries 31:86-87.
#'
#'Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson.  2006.  Proportional
#'size distribution (PSD): A further refinement of population size structure
#'index terminology.  Fisheries 32:348.
#'
#'@export psdDataPrep pssDataPrep
#'@keywords manip
#'@examples
#'## Create some random data for three species
#'set.seed(345234534)  # just to control the randomization
#'dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
#'dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
#'dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
#'dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
#'dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
#'dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
#'d <- rbind(dbg,dlb,dbt)
#'str(d)
#'
#'# Rename variables to match PSDlit -- see recodeSpecies()
#'d2 <- recodeSpecies(~species,data=d,c("LMB"),c("Largemouth Bass"))
#'levels(d2$species1)
#'
#'# Example where species without PSD values and sub-stock fish are retained (the defaults)
#'d3 <- psdDataPrep(tl~species1,data=d2,units="mm")
#'str(d3)
#'with(d3,table(species1,PSDcat))
#'
#'# Example where species without known PSD values are removed, but sub-stock fish are not
#'d4 <- psdDataPrep(tl~species1,data=d2,units="mm",remove.other=TRUE)
#'str(d4)
#'with(d4,table(species1,PSDcat))
#'
#'# Example where both sub-stock and species without known PSD values are excluded
#'d5 <- psdDataPrep(tl~species1,data=d2,units="mm",remove.substock=TRUE,remove.other=TRUE)
#'str(d5)
#'with(d5,table(species1,PSDcat))
#'with(d5,table(species1)) 
#'
#'# Example where category names are not used
#'d6 <- psdDataPrep(tl~species1,data=d2,units="mm",use.catnames=FALSE)
#'str(d6)
#'with(d6,table(species1,PSDcat))
#'
psdDataPrep <- pssDataPrep <- function(formula,data,psdname="PSDcat",units=c("mm","cm","in"),
                        remove.substock=FALSE,remove.other=FALSE,use.catnames=TRUE) {
  units <- match.arg(units)
  cl <- getVarFromFormula(formula,data,expNumVars=2)
  cspp <- cl[2]
  cl <- cl[1]
  incl.zero <- !remove.substock
  # get the PSD literature values data frame
  PSDlit <- get(data("PSDlit", envir = environment()), envir = environment())
  # what species exist in the data frame
  specs <- levels(data[,cspp])
  # which species in the data frame are and are not found in the PSDlit list
  areLogical <- specs %in% levels(PSDlit$species)
  noPSD <- specs[which(!areLogical)]
  if (length(noPSD)>0 & remove.other) warning("The following species do not have known Gabelhouse five-cell length categories:\n ",paste(noPSD,collapse=", "),call.=FALSE)
  specs <- specs[which(areLogical)]
  
  # initiate a blank data frame that has same columns as old data frame
  ndf <- data[-c(1:nrow(data)),]
  # cycle through each species where PSD values are known
  for (i in 1:length(specs)) {
    # get the Gabelhouse length categories
    glhse <- psdVal(specs[i],units=units,incl.zero=incl.zero)
    # isolate the current species
    idf <- data[data[,cspp]==specs[i],]
    # if incl.zero not used then eliminate all fish below stock size
    if (!incl.zero) idf <- idf[idf[,cl]>=glhse[1],]
    # add the Gabelhouse length categories to the data frame
    idf <- lencat(as.formula(paste("~",cl,sep="")),data=idf,breaks=glhse,vname=psdname,use.catnames=use.catnames)
    # if asked, change numerical category names to stock, quality, etc. names
#    if (use.catnames) {
#      idf[,psdname] <- factor(idf[,psdname])
#      # must use match so that names of levels line up with numerical values
#      levels(idf[,psdname]) <- names(glhse)[match(levels(idf[,psdname]),glhse)]    
#    }
    # bind current species to the new data frame being created
    ndf <- rbind(ndf,idf)
  }
  # add back in "other" fish if asked not to remove
  if (!remove.other & length(noPSD)>0) {
    odf <- data[data[,cspp] %in% noPSD,]
    odf[,psdname] <- NA
    ndf <- rbind(ndf,odf)
  }
  # make sure the new length categories and species name variables are factored
#  if (use.catnames) ndf[,psdname] <- factor(ndf[,psdname],levels=names(glhse))
#    else ndf[,psdname] <- factor(ndf[,psdname])
  ndf[,cspp] <- factor(ndf[,cspp])
  # return the new data frame
  ndf
}
