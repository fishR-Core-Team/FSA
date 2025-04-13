#' @title Finds standard weight equation coefficients for a particular species.
#'
#' @description Returns a vector that contains all known or a subset of information about the standard weight equation for a given species, type of measurement units, and reference percentile.
#'
#' @details This function extract all known information from \code{\link{WSlit}} about the following standard weight equation,
#'
#' \deqn{log_{10}(Ws) = log_{10}(a) + blog_{10}(L) + blog_{10}(L)^{2}}
#'
#' See \code{\link{WSlit}} for more information about the meaning of each value returned.
#' 
#' Note from above that the coefficients are returned for the TRANSFORMED model. Thus, to obtain the standard weight (Ws), the returned coefficients are used to compute the common log of Ws which must then bed raised to the power of 10 to compute the Ws.
#'
#' @param species A string that contains the species name for which to find Ws coefficients. See details.
#' @param group A string that contains the sub-group of `species` for which to find the Ws coefficients. Will be things like \dQuote{"lotic"}, \dQuote{"lentic"}, \dQuote{"female"}, \dQuote{"male"}, etc.
#' @param units A string that indicates whether the coefficients for the standard weight equation to be returned are in (\code{"metric"} (DEFAULT; mm and g) or \code{"English"} (in and lbs) units.
#' @param ref A numeric that indicates which percentile the equation should be returned for. Note that the vast majority of equations only exist for the \code{75}th percentile (DEFAULT).
#' @param method A string that indicates which equation-derivation method should be used (one of `RLP`, `EmP`, or `Other`). Defaults to `NULL` which will result in the only method available being returned or an error asking the user to choose a method equations from more than one method are available (which is the case for very few species).
#' @param simplify A logical that indicates whether the \sQuote{units}, \sQuote{ref}, \sQuote{measure}, \sQuote{method}, \sQuote{comments}, and \sQuote{source} fields should be included (\code{=FALSE}) or not (\code{=TRUE}; DEFAULT). See details.
#'
#' @return A one row data frame from \code{\link{WSlit}} that contains all known information about the standard weight equation for a given species, type of measurement units, and reference percentile if \code{simplify=FALSE}. If \code{simplify=TRUE} then only the species; minimum and maximum length for which the standard equation should be applied; and intercept, slope, and quadratic  coefficients for the standard weight equation. Note that the maximum length and the quadratic coefficient will not be returned if they do not exist in \code{\link{WSlit}}.
#'
#' If no arguments are given to this function, a species name is mis-spelled, or if a standard weight equation does not exist (in \code{\link{WSlit}}) for a particular species, then a warning will be issued and a list of species names will be printed.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @seealso See \code{\link{wrAdd}} and \code{\link{WSlit}} for related functionality.
#'
#' @section IFAR Chapter: 8-Condition.
#'
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' @keywords manip
#'
#' @examples
#' #===== List all available Ws equations
#' wsVal()
#' 
#' #===== Find equations for Bluegill, in different formats
#' wsVal("Bluegill")
#' wsVal("Bluegill",units="metric")
#' wsVal("Bluegill",units="English")
#' wsVal("Bluegill",units="English",simplify=TRUE)
#' 
#' #===== Find equation for Cutthroat Trout, demonstration group
#' wsVal("Cutthroat Trout",group="lotic")
#' wsVal("Cutthroat Trout",group="lentic")
#' 
#' #===== Find equation for Ruffe, demonstration quadratic formula
#' wsVal("Ruffe",units="metric",ref=75,simplify=TRUE)
#' wsVal("Ruffe",units="metric",ref=50,simplify=TRUE)
#'
#' #===== Add Ws & Wr values to a data frame (for one species) ... also see wrAdd()
#' #----- Get Ws equation info
#' wsBG <- wsVal("Bluegill",units="metric")
#' wsBG
#' #----- Get example data
#' data(BluegillLM,package="FSAdata")
#' str(BluegillLM)
#' #----- Add Ws (eqn is on log10-log10 scale ... so log10 len, 10^ result)
#' BluegillLM$ws <- 10^(wsBG[["int"]]+wsBG[["slope"]]*log10(BluegillLM$tl))
#' #----- Change Ws for fish less than min.TL to NA
#' BluegillLM$ws[BluegillLM$tl<wsBG[["min.TL"]]] <- NA
#' #----- Add Wr
#' BluegillLM$wr <- BluegillLM$wght/BluegillLM$ws*100
#' #----- Examine results
#' peek(BluegillLM,n=6)
#' 
#' #----- Same as above but using dplyr
#' data(BluegillLM,package="FSAdata")   # reset to original for this example
#' if (require(dplyr)) {
#'   BluegillLM <- BluegillLM %>%
#'     mutate(ws=10^(wsBG[["int"]]+wsBG[["slope"]]*log10(tl)),
#'            ws=ifelse(tl<wsBG[["min.TL"]],NA,ws),
#'            wr=wght/ws*100)
#'   peek(BluegillLM,n=6)
#' }
#'
#' @export
wsVal <- function(species="List",group=NULL,
                  units=c("metric","English"),ref=NULL,method=NULL,
                  simplify=FALSE) {
  #===== load WSlit data frame into this functions environment
  type <- measure <- NULL   # avoiding bindings warning in RCMD CHECK
  units <- match.arg(units)
  WSlit <- FSA::WSlit
  
  
  if (length(species)>1) STOP("'species' must contain only one name.")
  if (species=="List") iListSpecies(WSlit)
  else {
    #===== Make checks on species
    #----- Species given, make sure in WSlit, then reduce data.frame to that species
    if (!any(unique(WSlit$species)==species)) {
      tmp <- paste0("There is no Ws equation in 'WSlit' for ",iStrCollapse(species),".")
      if (any(unique(WSlit$species)==capFirst(species)))
        STOP(tmp," However, there is an entry for ",iStrCollapse(capFirst(species)),
             " (note spelling, including capitalization).\n\n")
      else STOP(tmp," Type 'wsVal()' to see a list of available species.\n\n")
    } else df <- droplevels(WSlit[WSlit$species==species,])
    
    #===== Determine if "group"s for that species and then handle
    if (any(!is.na(df$group))) {
      #----- There are groups in WSlit, user did not supply group= so stop
      if (is.null(group))
        STOP(iStrCollapse(species)," has Ws equations for these sub-groups: ",
             iStrCollapse(unique(df$group)),
             ". Please use 'group=' to select the equation for one of these groups.\n\n")
      #----- There are groups in WSlit, user supplied group=, is it good?
      if (!group %in% unique(df$group))
        STOP("There is no ",iStrCollapse(group)," group for ",iStrCollapse(species),
             ". Please select from one of these groups: ",
             iStrCollapse(unique(df$group),last="or"),".\n\n")
      #----- There are groups in WSlit, user supplied group= is good, reduce df
      df <- droplevels(df[df$group==group,])
    } else {
      #----- There are no groups in WSlit ... check if user supplied group=
      if (!is.null(group)) WARN("There are no groups for ",iStrCollapse(species),
                                "; thus, your 'group=' has been ignored.")
      #---- drop group variable from df
      df <- df[,!names(df)=="group"]
    }
    
    #===== Checks on method
    tmp <- unique(df$method)
    #----- If more than one method in WSlit but method NULL then force a choice
    #      otherwise (i.e., one method and method NULL) then continue with df
    if (is.null(method) & length(tmp)>1) 
      STOP("Ws equations exist for both the RLP and EmP 'method's for ",
           iStrCollapse(species),". Please select one or the other with 'method='.")
    if (!is.null(method)) {
      if (!any(unique(df$method)==method))
        STOP("There is no Ws equation for ",iStrCollapse(species)," derived from the '",
             method,"' method. Possible methods for ",iStrCollapse(species)," are: ",
             iStrCollapse(unique(df$method),last="or"),".\n\n")
      df <- droplevels(df[df$method==method,])
    }
    
    #===== Make checks on units (if OK reduce data frame to those units)
    if (!any(unique(df$units)==units)) {
      STOP("There is no Ws equation in '",units,"' units for ",iStrCollapse(species),
           ". However, there is a Ws equation in '",unique(df$units),"' units for ",
           iStrCollapse(species),".\n\n")
    } else df <- droplevels(df[df$units==units,])
    
    #===== Make checks on ref (if OK reduce data frame to that ref)
    tmp <- unique(df$ref)
    #----- If more than one ref in WSlit but ref is NULL then force a choice
    #      otherwise (i.e., one ref and ref is NULL) then continue with df
    if (is.null(ref) & length(tmp)>1) 
      STOP("Ws equations exist for more than one 'ref'erence value for ",
           iStrCollapse(species),
           ". Please select one of the following values with 'ref=': ",
           iStrCollapse(tmp,last="or",quotes=""),".\n\n")
    if (!is.null(ref)) {
      if (!any(unique(df$ref)==ref))
        STOP("There is no Ws equation for ",iStrCollapse(species),
             " with a reference value of '",
             ref,"'. Possible reference values for ",iStrCollapse(species),
             " are: ",iStrCollapse(unique(df$ref),last="or",quotes=""),".\n\n")
      df <- droplevels(df[df$ref==ref,])
    }

    #===== Wrap-up to return the info
    # Should be a single row data frame if it gets to this point
    # If comments says "none" then drop the comment variable
    if (df$comment=="none") df <- df[,!names(df)=="comment"]
    # If function is linear (as opposed to quadratic) then drop the quad variable
    if (is.na(df$quad)) df <- df[,!names(df)=="quad"]
    # Change "min.len" and "max.len" variables to ".TL" or ."FL" as appropriate
    tmp <- paste(c("min","max"),df$measure,sep=".")
    names(df)[names(df) %in% c("min.len","max.len")] <- tmp
    # Remove max.len if it is NA
    if (is.na(df[,tmp[2]])) df <- df[,!names(df)==tmp[2]]
    # If told to simplify then only get certain values
    if (simplify)
      df <- df[,names(df) %in% c("species",tmp,"int","slope","quad")]
    df
  }
}
