#' @title Finds standard weight equation coefficients for a particular species.
#'
#' @description Returns a vector that contains information about the standard weight equation for a given species, including type of measurement units, reference percentile, method used to derive the equation, and literature source.
#'
#' @details This function extracts all known information from \code{\link{WSlit}} about the following standard weight equation,
#'
#' \deqn{log_{10}(Ws) = log_{10}(a) + blog_{10}(L) + blog_{10}(L)^{2}}
#'
#' See \code{\link{WSlit}} for more information about the meaning of each value returned.
#' 
#' Note from above that the coefficients are returned for the TRANSFORMED model. Thus, to obtain the standard weight (Ws), the returned coefficients are used to compute the common log of Ws which must then be raised to the power of 10 to compute the Ws.
#' 
#' Some species have length categories separated by sub-group. For example, length categories exist for both lentic and lotic populations of Brown Trout. The length values for a sub-group may be obtained by either including the species name in \code{species} and the sub-group name in \code{group} or by using the combined species and sub-group name, with the sub-group name in parentheses, in \code{species}. Both methods are demonstrated in the examples. Note that an error is returned if a species has sub-groups but neither method is used to define the sub-group.
#' 
#' See examples and \href{https://fishr-core-team.github.io/FSA/articles/Computing_Relative_Weights.html}{this article} for a demonstration.
#'
#' @param species A string that contains the species name for which to find Ws coefficients. See details.
#' @param group A string that contains the sub-group of \code{species} for which to find the Ws coefficients; e.g., things like \dQuote{lotic}, \dQuote{lentic}, \dQuote{female}, \dQuote{male}.
#' @param units A string that indicates whether the coefficients for the standard weight equation to be returned are in \code{"metric"} (DEFAULT; mm and g) or \code{"English"} (in and lbs) units.
#' @param ref A numeric that indicates which percentile the equation should be returned for. Note that the vast majority of equations only exist for the \code{75}th percentile (DEFAULT).
#' @param method A string that indicates which equation-derivation method should be used (one of \code{"RLP"}, \code{"EmP"}, or \code{"Other"}). Defaults to \code{NULL} which will result in the only method available being returned or an error asking the user to choose a method for equations for which more than one method is available (which is the case for very few species).
#' @param simplify A logical that indicates whether the \sQuote{units}, \sQuote{ref}, \sQuote{measure}, \sQuote{method}, \sQuote{comments}, and \sQuote{source} fields should be included (\code{=FALSE}) or not (\code{=TRUE}; DEFAULT). See details.
#' @param dat Data.frame of Gabelhouse length categories for all species. Defaults to `WSlit` and is generally not used by the user (this simplifies use of this function in \code{wrAdd}).
#'
#' @return A one row data frame from \code{\link{WSlit}} that contains all known information about the standard weight equation for a given species, type of measurement units, and reference percentile if \code{simplify=FALSE}. If \code{simplify=TRUE} then only the species; minimum and maximum length for which the standard equation should be applied; and intercept, slope, and quadratic  coefficients for the standard weight equation. Note that the maximum length and the quadratic coefficient will not be returned if they do not exist in \code{\link{WSlit}} for the species.
#'
#' If no arguments are given to this function then a list of available species names in \code{\link{WSlit}} will be printed. If the species name is mis-spelled (or mis-capitalized), multiple standard weight equations exist for the species (such that \code{group}, \code{ref}, or \code{method} should be used), or if a standard weight equation does not exist for the species in \code{\link{WSlit}}, then an error will be issued.
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
#' #===== Find equations for Yellow Perch, in different formats
#' wsVal("Yellow Perch")
#' wsVal("Yellow Perch",units="metric")   # same as default
#' wsVal("Yellow Perch",units="English")
#' wsVal("Yellow Perch",units="English",simplify=TRUE)
#' 
#' #===== Find equation for Ruffe, demonstrating quadratic formula
#' wsVal("Ruffe",units="metric",ref=75,simplify=TRUE)
#' wsVal("Ruffe",units="metric",ref=50,simplify=TRUE)
#'
#' #===== Find equation for Brown Trout, which has equations for sub-groups
#' #-----   demonstrating use of group= argument
#' wsVal("Brown Trout",group="lotic")
#' wsVal("Brown Trout",group="lentic")
#' #-----   demonstrating group combined in species name, so no group= arg
#' wsVal("Brown Trout (lotic)")
#' wsVal("Brown Trout (lentic)")
#' 
#' #===== Add Ws & Wr values to a data frame (for one species) ... also see wrAdd()
#' #----- Example data from PSDWRtest, simplify variables for this example
#' yepdf <- subset(PSDWRtest,species=="Yellow Perch",select=c("species","len","wt"))
#' str(yepdf)
#' 
#' #----- Get Ws equation info
#' ( wsYEP <- wsVal("Yellow Perch",units="metric") )
#' 
#' #----- Add Ws (eqn is on log10-log10 scale ... so log10 length, 10^ result)
#' yepdf$ws <- 10^(wsYEP[["int"]]+wsYEP[["slope"]]*log10(yepdf$len))
#' 
#' #----- Change Ws for fish less than min.TL to NA
#' yepdf$ws[yepdf$len<wsYEP[["min.TL"]]] <- NA
#' 
#' #----- Add Wr
#' yepdf$wr <- yepdf$wt/yepdf$ws*100
#' 
#' #----- Examine results
#' peek(yepdf,n=6)
#' 
#' #----- Same as above but using dplyr
#' if (require(dplyr)) {
#'   yepdf <- PSDWRtest %>% filter(species=="Yellow Perch") %>% select(species,len,wt) %>%
#'     mutate(ws=10^(wsYEP[["int"]]+wsYEP[["slope"]]*log10(len)),
#'            ws=ifelse(len<wsYEP[["min.TL"]],NA,ws),
#'            wr=wt/ws*100)
#'   peek(yepdf,n=6)
#' }
#'
#' @export
wsVal <- function(species="List",group=NULL,
                  units=c("metric","English"),ref=NULL,method=NULL,
                  simplify=FALSE,dat=NULL) {
  #===== load WSlit data frame into this functions environment
  type <- measure <- NULL   # avoiding bindings warning in RCMD CHECK
  if (is.null(dat)) dat <- FSA::WSlit

  units <- match.arg(units)
  if (length(species)>1) STOP("'species' must contain only one name.")
  if (species=="List") iListSpecies(dat)
  else {
    #===== Make checks on species
    #----- Species given, make sure in dat/WSlit, then reduce data.frame to that species
    spNames <- unique(dat$species)
    if (!any(spNames==species)) {
      tmp <- paste0("There is no Ws equation in 'WSlit' for ",iStrCollapse(species),".")
      if (any(spNames==capFirst(species)))
        STOP(tmp," However, there is an entry for ",iStrCollapse(capFirst(species)),
             " (note spelling, including capitalization).\n\n")
      else STOP(tmp,iSuggestSpecies(species,spNames),
                " Type 'wsVal()' to see a list of available species.\n\n")
    } else df <- droplevels(dat[dat$species==species,])
    
    #===== Determine if "group"s for that species and then handle
    if (any(!is.na(df$group))) {
      #----- There are groups in dat/WSlit, user did not supply group= so stop
      if (is.null(group))
        STOP(iStrCollapse(species)," has Ws equations for these sub-groups: ",
             iStrCollapse(unique(df$group)),
             ". Please use 'group=' to select the equation for one of these groups.\n\n")
      #----- There are groups in dat/WSlit, user supplied group=, is it good?
      if (!group %in% unique(df$group))
        STOP("There is no ",iStrCollapse(group)," group for ",iStrCollapse(species),
             ". Please select from one of these groups: ",
             iStrCollapse(unique(df$group),last="or"),".\n\n")
      #----- There are groups in dat/WSlit, user supplied group= is good, reduce df
      df <- droplevels(df[df$group==group,])
    } else {
      #----- There are no groups in dat/WSlit ... check if user supplied group=
      if (!is.null(group)) WARN("There are no groups for ",iStrCollapse(species),
                                "; thus, your 'group=' has been ignored.")
      #---- drop group variable from df
      df <- df[,!names(df)=="group"]
    }
    
    #===== Checks on method
    tmp <- unique(df$method)
    #----- If more than one method in dat/WSlit but method NULL then force a choice
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
    #----- If more than one ref in dat/WSlit but ref is NULL then force a choice
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
