#' @title Finds Gabelhouse lengths (for PSD calculations) for a species.
#'
#' @description Returns a vector with the five Gabelhouse lengths for a chosen species.
#'
#' @param species A string that contains the species name for which to find Gabelhouse lengths. See details.
#' @param units A string that indicates the units for the returned lengths. Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param incl.zero A logical that indicates if a zero is included in the first position of the returned vector (DEFAULT) or not. This position will be named \dQuote{substock}. See details.
#' @param addLens A numeric vector that contains minimum length definitions for additional categories. See details.
#' @param addNames A string vector that contains names for the additional length categories added with \code{addLens}. See details.
#' @param showJustSource A logical that indicates whether just the literature source information should be returned (\code{TRUE}) or not. If \code{TRUE} this will NOT return any of the Gabelhouse length information.
#'
#' @details Finds the Gabelhouse lengths from \code{data(PSDlit)} for the species given in \code{species}. The species name must be spelled exactly (within capitalization differences) as it appears in \code{data(PSDlit)}. Type \code{psdVal()} to see the list of species and how they are spelled.
#'
#' A zero is included in the first position of the returned vector if \code{incl.zero=TRUE}. This is useful when computing PSD values with a data.frame that contains fish smaller than the stock length.
#'
#' Additional lengths may be added to the returned vector with \code{addLens}. Names for these lengths can be included in \code{addNames}. If \code{addNames} is non-NULL, then it must be of the same length as \code{addLens}. If \code{addLens} is non-NULL but \code{addNames} is NULL, then the default names will be the same as the lengths in \code{addLens}. The \code{addLens} argument is useful for calculating PSD values that are different from the Gabelhouse lengths.
#'
#' @return A vector of minimum values for length categories for the chosen species.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 6-Size Structure.
#' 
#' @seealso See \code{\link{psdCalc}}, \code{\link{psdPlot}}, \code{\link{psdAdd}}, \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{lencat}}, and \code{\link{rcumsum}} for related functionality.
#'
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR/}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Guy, C.S., R.M. Neumann, and D.W. Willis. 2006. New terminology for proportional stock density (PSD) and relative stock density (RSD): proportional size structure (PSS). Fisheries 31:86-87. [Was (is?) from http://pubstorage.sdstate.edu/wfs/415-F.pdf.]
#'
#' Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson. 2006. Proportional size distribution (PSD): A further refinement of population size structure index terminology. Fisheries 32:348. [Was (is?) from http://pubstorage.sdstate.edu/wfs/450-F.pdf.]
#'
#' Willis, D.W., B.R. Murphy, and C.S. Guy. 1993. Stock density indices: development, use, and limitations. Reviews in Fisheries Science 1:203-222. [Was (is?) from http://web1.cnre.vt.edu/murphybr/web/Readings/Willis\%20et\%20al.pdf.]
#'
#' @keywords manip
#'
#' @examples
#' # List all of the species
#' psdVal()
#' # Demonstrate typical usages
#' psdVal("Yellow perch")
#' psdVal("Walleye",units="cm")
#' psdVal("Bluegill",units="in")
#' psdVal("Bluegill",units="in",incl.zero=FALSE)
#' psdVal("Bluegill")
#' # Demonstrate that it will work with mis-capitalization
#' psdVal("bluegill")
#' psdVal("Yellow Perch")
#' # Demonstrate adding in user-defined categories
#' psdVal("Bluegill",units="in",addLens=7)
#' psdVal("Bluegill",units="in",addLens=7,addNames="MinLen")
#' psdVal("Bluegill",units="in",addLens=c(7,9),addNames=c("MinSlot","MaxSlot"))
#' psdVal("Bluegill",units="in",addLens=c("MinLen"=7))
#' psdVal("Bluegill",units="in",addLens=c("MinSlot"=7,"MaxSlot"=9))
#' psdVal("Bluegill",showJustSource=TRUE)
#'
#' @export psdVal
psdVal <- function(species="List",units=c("mm","cm","in"),incl.zero=TRUE,
                   addLens=NULL,addNames=NULL,showJustSource=FALSE) {
  units <- match.arg(units)
  # load RSDlit data frame into this function's environment
  # data/get combination are used to avoid the "no global binding" note at CHECK
  PSDlit <- get(utils::data("PSDlit",envir=environment()),envir=environment())
  # continue if species name is correct
  if (iPSDLitCheck(PSDlit,species <- capFirst(species))) {
    if (showJustSource) {
      PSDlit[PSDlit$species==species,c(1,12)]
    } else {
      # identify columns based on units
      ifelse(units=="in",cols <- 2:6,cols <- 7:11)
      # get the length categories
      PSDvec <- as.matrix(PSDlit[PSDlit$species==species,cols])[1,]
      # convert to mm if necessary
      if (units=="mm") PSDvec <- PSDvec*10
      names(PSDvec) <- c("stock","quality","preferred","memorable","trophy")
      # add a zero category if asked to
      if (incl.zero) {
        PSDvec <- c(0,PSDvec)
        names(PSDvec)[1] <- "substock"
      }
      # add additional lengths if asked to
      if (!is.null(addLens)) {
        # add names to the addLens vector
        addLens <- iHndlAddNames(addLens,addNames)
        # handle duplicated values
        tmp <- which(PSDvec %in% addLens)
        if (length(tmp>0)) {
          WARN("At least one Gabelhouse length that was in 'addLens' has been removed.")
          PSDvec <- PSDvec[-tmp]
        }
        # append the new lens to the Gabelhouse lengths
        PSDvec <- c(PSDvec,addLens)
        # re-order so the new values are within the Gabelhouse lengths
        PSDvec <- PSDvec[order(PSDvec)]
      }        
      PSDvec
    }
  }
}


# ==============================================================================
# Internal -- check species name against the'data' data.frame (usually PSDLit)
# ==============================================================================
iPSDLitCheck <- function(data,species) {
  OK <- FALSE
  if (length(species)!=1) STOP("'species' can have only one name.")
  else if (species=="List") iListSpecies(data)
       else if (!any(unique(data$species)==species))
         STOP("The Gabelhouse lengths do not exist for ",species,
              ".\n  Type psdVal() for a list of available species.\n\n")
            else OK <- TRUE
  OK
}

# ==============================================================================
# An internal function to handle adding names to the specific values to be added
#   to the vector of Gabelhouse length categories. Used in psdVal() (here) and
#   psdCalc().
# ==============================================================================
iHndlAddNames <- function(addLens,addNames) {
  ## check to make sure that addLens is not named
  if (is.null(names(addLens))) {
    ## if addNames is Null then use addLens values as the names
    if (is.null(addNames)) names(addLens) <- as.character(addLens)
    else {
      ## Otherwise change the names to what is in addNames
      ##   but make sure they are the same length first
      if (length(addLens)!=length(addNames))
        STOP("'addLens' and 'addNames' have different lengths.")
      names(addLens) <- addNames
    }
  }
  ## return the named addLens vector
  addLens
}