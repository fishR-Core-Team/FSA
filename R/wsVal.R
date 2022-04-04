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
#' @param species A string that contains the species name for which to find coefficients. See details.
#' @param units A string that indicates whether the coefficients for the standard weight equation to be returned are in (\code{"metric"} (DEFAULT; mm and g) or \code{"English"} (in and lbs) units.
#' @param ref A numeric that indicates which percentile the equation should be returned for. Note that the vast majority of equations only exist for the \code{75}th percentile (DEFAULT).
#' @param simplify A logical that indicates whether the \sQuote{units}, \sQuote{ref}, \sQuote{measure}, \sQuote{method}, \sQuote{comments}, and \sQuote{source} fields should be included (\code{=FALSE}) or not (\code{=TRUE}; DEFAULT). See details.
#'
#' @return A one row data frame from \code{\link{WSlit}} that contains all known information about the standard weight equation for a given species, type of measurement units, and reference percentile if \code{simplify=FALSE}. If \code{simplify=TRUE} then only the species; minimum and maximum length for which the standard equation should be applied; and intercept, slope, and quadratic  coefficients for the standard weight equation. Note that the maximum length and the quadratic coefficient will not be returned if they do not exist in \code{\link{WSlit}}.
#'
#' If no arguments are given to this function, a species name is mis-spelled, or if a standard weight equation does not exist (in \code{\link{WSlit}}) for a particular species, then a warning will be issued and a list of species names will be printed.
#' 
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link{wrAdd}} and \code{\link{WSlit}} for related functionality.
#'
#' @section IFAR Chapter: 8-Condition.
#'
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR/}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' @keywords manip
#'
#' @examples
#' wsVal()
#' wsVal("Bluegill")
#' wsVal("Bluegill",units="metric")
#' wsVal("Bluegill",units="English")
#' wsVal("Bluegill",units="English",simplify=TRUE)
#' wsVal("Ruffe",units="metric",simplify=TRUE)
#' wsVal("Ruffe",units="metric",ref=50,simplify=TRUE)
#'
#' @export
wsVal <- function(species="List",units=c("metric","English"),ref=75,simplify=FALSE) {
  type <- measure <- method <- NULL   # avoiding bindings warning in RCMD CHECK
  units <- match.arg(units)
  # load WSlit data frame into this functions environment
  WSlit <- FSA::WSlit
  # isolate only those data for which those units and ref exist
  df <- droplevels(WSlit[WSlit$units==units & WSlit$ref==ref,])
  # check to make sure that that species exists for that subset
  OK <- iwsLitCheck(df,species <- capFirst(species))
  # continue if species name is correct
  if (OK) {
    ## get the appropriate row from the data.frame
    WSvec <- df[df$species==species,]
    ## If comments says "none" then drop the comment variable
    if (WSvec$comment=="none") WSvec <- WSvec[,-which(names(WSvec)=="comment")]
    ## If function is linear (as opposed to quadratic) then drop the quad variable
    if (WSvec$type=="linear") WSvec <- WSvec[,-which(names(WSvec)=="quad")]
    ## Change "min.len" and "max.len" variables to ".TL" or ."FL" as appropriate
    tmp <- paste(c("min","max"),WSvec$measure,sep=".")
    names(WSvec)[which(names(WSvec) %in% c("min.len","max.len"))] <- tmp
    ## Remove max.len if it is NA
    if (is.na(WSvec[,tmp[2]])) WSvec <- WSvec[,-which(names(WSvec)==tmp[2])]
    ## If told to simplify then only get sertain values
    if (simplify) WSvec <- WSvec[,which(names(WSvec) %in% c("species",tmp,"int","slope","quad"))]
    WSvec
  }
  
}

iwsLitCheck <- function(data,species) {
  OK <- FALSE
  if (length(species)>1) STOP("'species' must contain only one name.")
  if (species=="List") iListSpecies(data)
  else if (!any(unique(data$species)==species)) {
    STOP("A Ws equation may not exist given your choices of species, units, and ref.\n  Please look carefully inside the data(WSlit) data frame.\n\n")
  }
    else OK <- TRUE
  OK
}

