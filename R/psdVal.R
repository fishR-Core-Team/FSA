#' @title Finds Gabelhouse lengths (for PSD calculations) for a species.
#'
#' @description Returns a vector with the five Gabelhouse lengths for a chosen species.
#'
#' @param species A string that contains the species name for which to find Gabelhouse lengths. See details.
#' @param group A string that contains the sub-group of `species` for which to find the Gabelhouse lengths. Will be things like \dQuote{"landlocked"}, \dQuote{"lentic"}.
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
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: 6-Size Structure.
#' 
#' @seealso See \code{\link{psdCalc}}, \code{\link{psdPlot}}, \code{\link{psdAdd}}, \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{lencat}}, and \code{\link{rcumsum}} for related functionality.
#'
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
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
#' #===== List all available species in PSDlit
#' psdVal()
#' 
#' #===== Demonstrate typical usages
#' psdVal("Bluegill")
#' psdVal("Bluegill",units="in")
#' psdVal("Bluegill",units="in",incl.zero=FALSE)
#' psdVal("Bluegill",showJustSource=TRUE)
#' 
#' #===== Demonstrate that some species have sub-groups
#' psdVal("Brown Trout",group="lentic")
#' psdVal("Brown Trout",group="lotic")
#' psdVal("Palmetto Bass",group="revised")
#' psdVal("Palmetto Bass",group="original")
#' 
#' #===== Demonstrate adding user-defined categories
#' psdVal("Bluegill",units="in",addLens=7)
#' psdVal("Bluegill",units="in",addLens=7,addNames="MinLen")
#' psdVal("Bluegill",units="in",addLens=c(7,9),addNames=c("MinSlot","MaxSlot"))
#' psdVal("Bluegill",units="in",addLens=c("MinLen"=7))
#' psdVal("Bluegill",units="in",addLens=c("MinSlot"=7,"MaxSlot"=9))
#'
#' @export psdVal
psdVal <- function(species="List",group=NULL,units=c("mm","cm","in"),
                   addLens=NULL,addNames=NULL,
                   incl.zero=TRUE,showJustSource=FALSE) {
  units <- match.arg(units)
  #====== Load PSDlit data frame into this function's environment
  PSDlit <- FSA::PSDlit
  #====== Check species name, stop if incorrect, continue if correct
  if (iPSDLitCheck(PSDlit,species <- capFirst(species))) {
    #----- Reduce to just that species
    PSDlit <- PSDlit[PSDlit$species==species,]
    #----- Handle possible sub-group
    PSDlit <- iPSDHndlGroup(PSDlit,species,group)
    #----- Prepare result
    if (showJustSource) {
      ifelse(is.null(group),cols <- c(1,14),cols <- c(1,2,15))
      PSDlit[,cols]
    } else {
      #..... Identify columns based on units
      ifelse(units=="in",cols <- 3:8,cols <- 9:14)
      if (is.null(group)) cols <- cols-1
      #..... get the length categories
      PSDvec <- as.matrix(PSDlit[,cols])[1,]
      names(PSDvec) <- gsub("\\..*","",names(PSDvec))
      #..... remove zero category (substock) if asked
      if (!incl.zero) PSDvec <- PSDvec[!names(PSDvec)=="substock"]
      #..... convert to mm if necessary
      if (units=="mm") PSDvec <- PSDvec*10
      #..... add additional lengths if asked to
      if (!is.null(addLens)) {
        # add names to the addLens vector
        addLens <- iHndlAddNames(addLens,addNames)
        # handle duplicated values
        tmp <- which(PSDvec %in% addLens)
        if (length(tmp>0)) {
          WARN("The following Gabelhouse length categories were removed because ",
               "they were duplicated with a length in 'addLens=': ",
               paste(names(PSDvec)[tmp],collapse=", "),".")
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
# Internal -- check species name against the 'dat' data.frame (usually PSDLit)
# ==============================================================================
iPSDLitCheck <- function(dat,species) {
  OK <- FALSE
  if (length(species)!=1) STOP("'species' can have only one name.")
  else if (species=="List") iListSpecies(dat)
       else if (!any(unique(dat$species)==species))
         STOP("The Gabelhouse lengths do not exist for \"",species,
              "\". Type psdVal() for a list of available species.\n\n")
            else OK <- TRUE
  OK
}

# ==============================================================================
# Internal -- handle possible use of group for sub-groups of a species
# ==============================================================================
iPSDHndlGroup <- function(dat,species,group) {
  if (any(!is.na(dat$group))) {
    #..... There are groups in dat, user did not supply group= so stop
    if (is.null(group))
      STOP("\"",species,"\" has Gabelhouse categories for these sub-groups: ",
           paste(unique(dat$group),collapse=", "),
           ". Please use 'group=' to select one of these groups.\n\n")
    #..... There are groups in dat, user supplied group=, is it good?
    if (!group %in% unique(dat$group))
      STOP("There is no \"",group,"\" group for \"",species,"\". ",
           "Please select from one of these groups: ",
           paste(unique(dat$group),collapse=", "),".\n\n")
    #..... There are groups in dat, user supplied group= is good, reduce df
    dat <- droplevels(dat[dat$group==group,])
  } else {
    #..... There are no groups in dat ... check if user supplied group=
    if (!is.null(group)) WARN("There are no groups for \"",species,
                              "\"; thus, your 'group=' has been ignored.")
    #..... drop group variable from df
    dat <- dat[,!names(dat)=="group"]
  }
  dat
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
