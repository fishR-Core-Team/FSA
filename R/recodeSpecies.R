#' @title Recodes species names according to user choice or to change the capitalization.
#'
#' @description \code{\link{psdVal}} and \code{\link{wsVal}} contain literature information required to compute proportional size stucture and relative weights for species for which Gabelhouse length categories (i.e., \dQuote{stock}, \dQuote{quality}, etc.) and standard weight equations are known.  To return the pertinent information, \code{\link{psdVal}} and \code{\link{wsVal}} require the user to provide the name of a species.  Efficiency of calculations is inhibited because users often have their species names in a different form than what is required by these functions.  This function is used to recode given species names into a form that is required by \code{\link{psdVal}} and \code{\link{wsVal}}, though it is flexible enough to recode to any names that the user chooses.
#'
#' @details This function requires that the data frame provided in \code{data} to contains at least one column of fish species names, which is given in \code{formula}.  Species names found in in this variable that should be changed should be included as a vector of strings in \code{oldnames}.  Corresponding new species names are then included as a vector of strings in \code{newnames}.  The user should assure that the order of the names in these two vectors match.  If the lengths of \code{oldnames} and \code{newnames} are not equal, then the function will throw an error.
#'
#' The species names in the data frames accessed by \code{\link{psdVal}} and \code{\link{wsVal}} have all words of the species name capitalized.  This function will return names that match this convention if \code{doCapFirst="all"} (the default).
#'
#' This function is often used as a pre-cursor to \code{\link{psdAdd}} and \code{\link{wrAdd}}.
#'
#' @aliases recodeSpecies
#'
#' @param data A data.frame that minimally contains the species names given in the variable in the \code{formula}.
#' @param formula A formula of the form \code{~species} where \dQuote{species} generically represents a variable that contains species names.  Note that this formula can only contain one variable.
#' @param oldnames A vector of strings that contains species names that the the user wants to change to the names found in \code{newnames}.  See details.
#' @param newnames A vector of strings that will be the new species names in the returned vector.  See details.
#' @param doCapFirst A string that indicates whether the resulting species names should have \code{all} (the default) or only the \code{first} word of the name capitalized.  Using \code{none} will keep the capitalization of the species name as in the original variable.  See details.
#'
#' @return A factored vector of new species names.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{psdAdd}} and \code{\link{wrAdd}}.
#'
#' @section fishR vignette: none yet.
#'
#' @keywords manip
#'
#' @examples
#' ## Create some random data for three species
#' # just to control the randomization
#' set.seed(345234534)
#' dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
#' dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
#' dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
#' dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
#' dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
#' dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
#' d <- rbind(dbg,dlb,dbt)
#' str(d)
#'
#' # Example of just changing the all species names to both words capitalized
#' d$spec1 <- recodeSpecies(d,~species)
#' levels(d$spec1)
#'
#' # Example of just changing the all species names to just first word capitalized
#' d$spec2 <- recodeSpecies(d,~species,doCapFirst="first")
#' levels(d$spec2)
#'
#' # Example of changing one name and the both words capitalized of all names
#' d$spec3 <- recodeSpecies(d,~species,oldn=c("LMB"),newn=c("Largemouth bass"))
#' levels(d$spec3)
#'
#' # Example of changing one name but not the case of all the names
#' d$spec4 <- recodeSpecies(d,~species,oldn=c("LMB"),newn=c("Largemouth Bass"),doCapFirst="none")
#' levels(d$spec4)
#'
#' # Example of using a data.frame that contains the matching names
#' spnms <- data.frame(old=c("BFT","BG","LMB"),
#'                     new=c("Bluefin Tuna","Bluegill","Largemouth Bass"))
#' d$spec5 <- recodeSpecies(d,~species,oldn=spnms$old,newn=spnms$new,doCapFirst="none")
#' levels(d$spec5)
#'
#' @export
recodeSpecies <- function(data,formula,
                           doCapFirst=c("all","first","none"),
                           oldnames=NULL,newnames=NULL) {
  doCapFirst <- match.arg(doCapFirst)
  tmp <- iHndlFormula(formula,data,expNumR=0,expNumE=1,expNumENums=0,expNumEFacts=1)
  if (tmp$vnum!=1) stop("'formula' must have only one variable.",call.=FALSE)
  if (tmp$vclass!="factor") stop("Variable in 'formula' is not a factor (thus, not species).",call.=FALSE)
  ## get just the species name variable as a character (not a factor)
  tmp <- as.character(tmp$mf[,1])
  ## check if something in oldnames or newnames that there is something in the other
  if (is.null(oldnames) & !is.null(newnames)) {
    warning("'oldnames' not defined when 'newnames' is; 'newnames' is ignored",call.=FALSE)
    newnames <- NULL
  }
  if (is.null(newnames) & !is.null(oldnames)) {
    warning("'newnames' not defined when 'oldnames' is; 'oldnames' is ignored",call.=FALSE)
    oldnames <- NULL
  }
  ## Check if nothing to do, if so return old names and give
  ##   warning, otherwise make changes
  if (doCapFirst=="none" & is.null(newnames)) {
    # If nothing to do then give warning ... original namew will be returned
    warning("With capFirst=='none' and nothing in newnames,\n there is nothing for this function to do.",call.=FALSE)
  } else { ## there is something to do.
    if (!is.null(newnames)) {
      # make sure that oldnames and newnames are characters (rather than factors)
      newnames <- as.character(newnames)
      oldnames <- as.character(oldnames)
      # make sure that newnames and oldnames have the same number of names
      if (length(oldnames) != length(newnames)) stop("'oldnames' and 'newnames' must be the same length",call.=FALSE)
      # change the oldnames to the new names
      for (i in 1:length(oldnames)) tmp[tmp==oldnames[i]] <- newnames[i]
    } 
    if (doCapFirst!="none") tmp <- capFirst(tmp,doCapFirst)
  }
  # do a final factoring of the new names and return the result
  factor(tmp)
}