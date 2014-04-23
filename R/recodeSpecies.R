#'Recodes species names according to user choice or to have capitalization only
#'for the first word in the name.
#'
#'The \code{psdVal} or \code{wrVal} functions contain literature information required
#'to compute proportional size stucture and relative weights for species for which
#'five-cell length categories (i.e., \dQuote{stock}, \dQuote{quality}, etc.) and
#'standard weight equations are known.  These functions require the user to provide
#'the name of a species and then return the pertinent information.  Automation of
#'the calculations is inhibited because users often have their species names in a
#'different form than what is required by these functions.  This function is used
#'to recode given species names into a form that is required by \code{psdVal} and
#'\code{wrVal}.
#'
#'This function requires that the data frame provided in \code{data} contains one
#'column of fish species names (in \code{cspp}).  Species names found in \code{cspp}
#'that should be changed are included as a vector of strings in \code{oldnames}.
#'Corresponding new species names are then included as a vector of srings in 
#'\code{newnames}.  The user should assure that the order of the names in these two
#'vectors match.  If the lengths of \code{oldnames} and \code{newnames} are not equal,
#'then the function will be terminated.
#'
#'The species names in the data frames accessed by \code{psdVal} and \code{wrVal}
#'all words of the species name capitalized.  This function will return names that
#'match this convention if \code{doCapFirst="all"} (the default).
#'
#'This function is often used as a pre-cursor to \code{\link{psdDataPrep}} and
#'\code{\link{wrDataPrep}}.
#'
#'@aliases recodeSpecies
#'@param formula A formula of the form \code{~species} where \dQuote{species}
#'generically represents a variable in \code{data} that contains species names.
#'Note that this formula can only contain one variable.
#'@param data A data.frame that minimally contains the length measurements given
#'in the variable in the \code{formula}.
#'@param oldnames A vector of strings that contains names in \code{cspp} that the
#'the user wants to change to the names found in \code{newnames}.  See details.
#'@param newnames A vector of strings that will be the new names in the new \code{nspp}
#'variable for the species names found in \code{oldnames}.  See details.
#'@param nspp A string that will be the name of the new \dQuote{species} variable
#'in the returned data frame.  The default is to append a \dQuote{1} to the original
#'variable name in \code{cspp}.
#'@param doCapFirst A string that indicates whether the resulting names in 
#'\code{nspp} should have \code{all} (the default) or only the \code{first} word
#'of the name capitalized.  Using \code{none} will keep the capitalization of the
#'species name as in the original data frame.  See details.
#'@return The original data frame in \code{data} but with the new variable \code{nspp}
#'which contains the new species names.
#'@seealso \code{\link{psdDataPrep}} and \code{\link{wrDataPrep}}.
#'@section fishR vignette: none yet.
#'@export
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
#'# Example of just changing the all species names to both words capitalized
#'d1 <- recodeSpecies(~species,data=d)
#'levels(d1$species1)
#'
#'# Example of just changing the all species names to just first word capitalized
#'d1a <- recodeSpecies(~species,data=d,doCapFirst="first")
#'levels(d1a$species1)
#'
#'# Example of changing one name and the both words capitalized of all names
#'d2 <- recodeSpecies(~species,data=d,c("LMB"),c("Largemouth bass"))
#'levels(d2$species1)
#'
#'# Example of changing one name but not the case of all the names
#'d3 <- recodeSpecies(~species,data=d,c("LMB"),c("Largemouth Bass"),doCapFirst="none")
#'levels(d3$species1)
#'
#'# Example of changing the resulting variable name
#'d4 <- recodeSpecies(~species,data=d,c("LMB"),c("Largemouth Bass"),nspp="newSpecies")
#'str(d4)
#'
recodeSpecies <- function(formula,data,oldnames=NULL,newnames=NULL,
                          nspp=paste(cspp,"1",sep=""),doCapFirst=c("all","first","none")) {
  doCapFirst <- match.arg(doCapFirst)
  cspp <- getVarFromFormula(formula,data,expNumVars=1)
  if (doCapFirst=="none" & is.null(newnames)) stop("With capFirst=='none' and nothing in newnames,\n there is nothing for this function to do.",call.=FALSE)
  # initialize new "species" variable, either with the old "species" variable or
  #   the old "species" variable but with the case changed to only first name capitalized
  if (doCapFirst=="none") data[,nspp] <- as.character(data[,cspp])
    else data[,nspp] <- apply(matrix(data[,cspp]),1,capFirst,doCapFirst)
  # If no newnames then nothing else to do
  if (!is.null(newnames)) {
    # if appropriate, make sure that only the first name is capitalized
    if (doCapFirst!="none") newnames <- apply(matrix(newnames),1,capFirst,doCapFirst)
    # make sure that newnames and oldnames have the same number of names
    if (length(oldnames) != length(newnames)) stop("'oldnames' and 'newnames' must be the same length")
    # change the oldnames to the new names
    for (i in 1:length(oldnames)) data[data[,cspp]==oldnames[i],nspp] <- newnames[i]
  } 
  # do a final factoring of the new names
  data[,nspp] <- factor(data[,nspp])
  # return the modified data frame
  data
}
