#' @title Finds Gabelhouse five-cell length categories (for PSD) for a particular species.
#'
#' @description Returns a vector with the Gabelhouse five-cell length categories for a particular species.
#'
#' @details Finds the Gabelhouse five-cell length categories for the given species names in the \code{data(PSDlit)} data frame.  The species names must be spelled exactly like they appear in the \code{data(PSDlit)} data frame (type \code{psdVal()} to see the list of species and how they are spelled).
#'
#' If \code{incl.zero=TRUE} then a zero will be included in the first postion of the returned vector and the five-cell length categories will follow.  This is useful when computing PSD values as a sample of fish may have individuals smaller than the stock length.
#'
#' The \code{addLens} argument can be used to add additional lengths to the vector of length categories returned.  Names for these lengths can be included in the \code{addNames} argument.  If \code{addNames} is non-NULL then it must of the same length as \code{addLens}.  If \code{addLens} is non-NULL but \code{addNames} is NULL then the default names will be the same as the lengths in \code{addLens}.  The \code{addLens} argument is useful for calculating PSD values that are different from the usual five-cell categories.
#'
#' @aliases psdVal
#'
#' @param species A string that contains the species name to find coefficients for.
#' @param units A string that indicates the type of units used for the length measurements.  Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param incl.zero A logical that indicates whether to include a zero in the first position of the returned vector (DEFAULT) or not.  See details.
#' @param addLens A numeric vector that contains minimum length definitions for additional categories.  See details.
#' @param addNames A string vector that contains names for the additional length categories added with \code{addLens}.  See details.
#'
#' @return A vector of the five-cell length categories for the given species.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{psdCalc}}, \code{\link{psdPlot}}, \code{\link{psdDataPrep}}, \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{tictactoeAdd}}, \code{\link{lencat}}, and \code{\link{rcumsum}}.
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
#' psdVal("Bluegill",units="in",addLens=c(7,9))
#'
#' @export psdVal
psdVal <- function(species="List",units=c("mm","cm","in"),incl.zero=TRUE,
                   addLens=NULL,addNames=NULL) {
  units <- match.arg(units)
  # load RSDlit data frame into this function's environment
  # the data/get combination are used to avoid the "no global binding" note at CHECK
  PSDlit <- get(data("PSDlit", envir = environment()), envir = environment())
  # check on PSDLit and species names
  OK <- iPSDLitCheck(PSDlit,species <- capFirst(species))
  # continue if species name is correct
  if (!is.null(OK)) {
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
      names(PSDvec)[1] <- "zero"
    }
    # add additional lengths if asked to
    if (!is.null(addLens)) {
      if (is.null(addNames)) addNames <- addLens
      if (length(addLens)!=length(addNames)) stop("Length of 'addNames' does not equal length of 'addLens'.")
      names(addLens) <- addNames
      PSDvec <- c(PSDvec,addLens)
      PSDvec <- PSDvec[order(PSDvec)]
    }        
    PSDvec
  }    
}



iPSDLitCheck <- function(data,species) {
  OK <- FALSE
  if (species=="List") iListSpecies(data)
  else if (!any(levels(data$species)==species)) {
    stop("The five-cell categories do not exist for your choice of species.\n  Please type psdVal() for a list of available species.\n\n",call.=FALSE)
  }
  else OK <- TRUE
}
