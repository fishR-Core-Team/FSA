#'Appends standard weight and relative weight variables to a data frame.
#'
#'Computes the appropriate standard weight and relative weight for a species
#'and appends the values to an existing data frame.
#'
#'This function first computes the standard weight given the observed lengths
#'and the appropriate standard weight equation, from \code{wsVal()}, for the 
#'provided species.  Either the linear or quadratic equation has been preferred
#'for each species so only that equation will be used.  However, some species
#'have standard weight equations for different percentiles and use of these
#'equations must be chosen by the user with \code{ref}.
#'
#'Once the standard weight is computed, the relative weights is computed given the
#'observed weights in and the computed standard weight according to
#'
#'\deqn{\frac{W}{W_{S}}*100}
#'
#'The results of both computations are appended as columns to the original data frame.
#'
#'If a species name does not exist in the database then no results will be
#'returned but a list of species will be printed.
#'
#'@param formula A formula of the form \code{weight~length} where \dQuote{weight}
#'generically represents a variable in \code{data} that contains weight measurements
#'and \dQuote{length} generically represents a variable in \code{data} that contains
#'length measurements.  Note that this formula can only contain two variables and they
#'must be in the weight first, length second order.
#'@param data A data.frame that minimally contains the length and weight measurements
#'given in the variables in the \code{formula}.
#'@param species A string that contains the species name for which to make
#'computations.  See details.
#'@param units A string that indicates whether the coefficients for the (\code{"metric"}
#'(DEFAULT; mm and g) or \code{"English"} (in and lbs) units should be returned.
#'\code{"quadratic"} model should be returned.
#'@param ref A numeric that indicates which quantile the equation should be returned
#'for.  Note that the vast majority of equations only exist for the \code{75}th
#'percentile (DEFAULT).
#'@param remove.submin A logical that indicates how to handle fish below the minimum
#'length for which the standard weight equation holds.  If \code{FALSE} (DEFAULT)
#'then the standard and relative weights for the \dQuote{small} fish are replaced
#'with \code{NA}.  If \code{TRUE} then the \dQuote{small} fish are excluded from the
#'returned data frame.
#'@param wsname A string that contains the name for the new standard weight variable.
#'@param wrname A string that contains the name for the new relative weight variable.
#'@return Returns a data frame that consists of the original data frame,
#'\code{data}, but with the new standard and relative weight variables appended and
#'named as in \code{wsname} and \code{wrname}, respectively.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{wsVal}}, \code{\link{wrDataPrep}}, and \code{\link{WSlit}}.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}
#'@keywords manip
#'@examples
#'# A data frame of bluegill lengths and weights
#'data(BluegillLM)
#'str(BluegillLM)
#'  
#'# NAs put in place for "small" fish
#'new1 <- wrAdd(wght~tl,data=BluegillLM,species="Bluegill",units="metric")
#'str(new1)
#'tail(new1)
#'
#'# Small fish are removed from the returned data frame
#'new2 <- wrAdd(wght~tl,data=BluegillLM,species="Bluegill",units="metric",remove.submin=TRUE)
#'str(new2)
#'tail(new2)
#'  
#'# Different variable names
#'new3 <- wrAdd(wght~tl,data=BluegillLM,species="Bluegill",units="metric",
#'  wsname="Standard",wrname="Relative")
#'str(new3)
#'tail(new3)
#'
#'# Add English units (mm, lbs) to original data frame as a demonstration only
#'BluegillLM$tlE <- BluegillLM$tl/25.4
#'BluegillLM$wghtE <- BluegillLM$wght/454
#'new4 <- wrAdd(wghtE~tlE,data=BluegillLM,species="Bluegill",units="English")
#'str(new4)
#'# compare to tail(new1) to see slight differences due to conversion
#'tail(new4) 
#'
#'@export
wrAdd <- function(formula,data,species="List",units=c("metric","English"),
                  ref=75,remove.submin=FALSE,wsname="ws",wrname="wr") {
  units <- match.arg(units)
  wseqn <- wsVal(species,units=units,ref=ref)
  if (is.null(wseqn)) stop("Species Ws equation does not exist for choices of species, units, and ref.",.call=FALSE)
  cl <- getVarFromFormula(formula,data,expNumVars=2)
  cw <- cl[1]
  cl <- cl[2]
  nd <- data
  predlog <- wseqn$int+wseqn$slope*log10(nd[,cl])
  if (!is.na(wseqn$quad)) predlog <- predlog + wseqn$quad*(log10(nd[,cl])^2)
  nd$ws <- 10^predlog
  names(nd)[ncol(nd)] <- wsname
  nd$wr <- (nd[,cw]/nd[,wsname])*100
  names(nd)[ncol(nd)] <- wrname
  if (!remove.submin) nd[nd[,cl]<wseqn$min.len,c(wsname,wrname)] <- NA
    else nd <- Subset(nd,nd[,cl]>=wseqn$min.len)
  nd
}
