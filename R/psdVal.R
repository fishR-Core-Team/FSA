#'Finds Gabelhouse five-cell length categories (for PSD) for a particular species.
#'
#'Returns a vector with the Gabelhouse five-cell length categories for a particular species.
#'
#'Finds the Gabelhouse five-cell length categories for the given species names in the
#'\code{data(PSDlit)} data frame.  The species names must be spelled exactly like
#'they appear in the \code{data(PSDlit)} data frame (type \code{psdVal()} to see
#'the list of species and how they are spelled).
#'
#'If \code{incl.zero=TRUE} then a zero will be included in the first postion of
#'the returned vector and the five-cell length categories will follow.  This is
#'useful when computing PSD values as a sample of fish may have individuals smaller
#'than the stock length.
#'
#'The \code{addLens} argument can be used to add additional lengths to the vector
#'of length categories returned.  Names for these lengths can be included in the
#'\code{addNames} argument.  If \code{addNames} is non-NULL then it must of the
#'same length as \code{addLens}.  If \code{addLens} is non-NULL but \code{addNames}
#'is NULL then the default names will be the same as the lengths in \code{addLens}.
#'The \code{addLens} argument is useful for calculating PSD values that are different
#'from the usual five-cell categories.
#'
#'@rdname psdVal
#'@aliases psdVal pssVal
#'@param species A string that contains the species name to find coefficients for.
#'@param units A string that indicates the type of units used for the length measurements.
#'hoices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and 
#'\code{in} for inches.
#'@param incl.zero A logical that indicates whether to include a zero in the first
#'position of the returned vector (DEFAULT) or not.  See details.
#'@param addLens A numeric vector that contains minimum length definitions for
#'additional categories.  See details.
#'@param addNames A string vector that contains names for the additional length
#'categories added with \code{addLens}.  See details.
#'@return A vector of the five-cell length categories for the given species.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{psdCalc}}, \code{\link{psdPlot}}, \code{\link{psdDataPrep}},
#' \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{tictactoeAdd}},
#' \code{\link{lencat}}, and \code{\link{rcumsum}}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/SizeStructure.pdf}
#'@source Guy, C.S., R.M. Neumann, and D.W. Willis.  2006.  New terminology for
#'proportional stock density (PSD) and relative stock density (RSD): proportional
#'size structure (PSS).  Fisheries 31:86-87.
#'
#'Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson.  2006.  Proportional
#'size distribution (PSD): A further refinement of population size structure
#'index terminology.  Fisheries 32:348.
#'
#'@export psdVal pssVal
#'@keywords manip
#'@examples
#'# List all of the species
#' psdVal()
#'# Demonstrate typical usages
#' psdVal("Yellow perch")
#' psdVal("Walleye",units="cm")
#' psdVal("Bluegill",units="in")
#' psdVal("Bluegill",units="in",incl.zero=FALSE)
#' psdVal("Bluegill")
#'# Demonstrate that it will work with mis-capitalization
#' psdVal("bluegill")
#' psdVal("Yellow Perch")
#'# Demonstrate adding in user-defined categories
#' psdVal("Bluegill",units="in",addLens=7)
#' psdVal("Bluegill",units="in",addLens=7,addNames="MinLen")
#' psdVal("Bluegill",units="in",addLens=c(7,9))
#'
psdVal <- pssVal <- function(species="List",units=c("mm","cm","in"),incl.zero=TRUE,
                             addLens=NULL,addNames=NULL) {
  units <- match.arg(units)
  # load RSDlit data frame into this function's environment
  # the data/get combination are used to avoid the "no global binding" note at CHECK
  PSDlit <- get(data("PSDlit", envir = environment()), envir = environment())
  OK <- psdLitCheck(PSDlit,species <- capFirst(species))                        # check on PSDLit and species names
  if (!is.null(OK)) {                                                           # continue if species name is correct
    ifelse(units=="in",cols <- 2:6,cols <- 7:11)                                # identify columns based on units
    PSDvec <- as.matrix(PSDlit[PSDlit$species==species,cols])[1,]               # get the length categories
    if (units=="mm") PSDvec <- PSDvec*10                                        # convert to mm if necessary
    names(PSDvec) <- c("stock","quality","preferred","memorable","trophy")      # rename vector
    if (incl.zero) {                                                            # add a zero category if asked to
      PSDvec <- c(0,PSDvec)
      names(PSDvec)[1] <- "zero"
    }
    if (!is.null(addLens)) {                                                    # add additional lengths if asked to
      if (is.null(addNames)) addNames <- addLens
      if (length(addLens)!=length(addNames)) stop("Length of 'addNames' does not equal length of 'addLens'.")
      names(addLens) <- addNames
      PSDvec <- c(PSDvec,addLens)
      PSDvec <- PSDvec[order(PSDvec)]
    }        
    PSDvec
  }    
}
