#' @title Computes a vector of relative weights specific to a species in an entire data frame.
#'
#' @description This computes a vector that contains the relative weight specific to each species for all individuals in an entire data frame.
#'
#' @details This computes a vector that contains the relative weight specific to each species for all individuals in an entire data frame.  The vector can be appended to an existing data.frame to create a variable that contains the relative weights for each individual.  The relative weight value will be \code{NA} for each individual for which a standard weight equation does not exist in \code{\link{WSlit}}, a standard weight equation for the units given in \code{units=} does not exist in \code{\link{WSlit}}, a standard weight equation for the 75th percentile does not exist in \code{\link{WSlit}}, or if the individual is shorter or longer than the lengths for which the standard weight equation should be applied.  Either the linear or quadratic equation has been listed as preferred for each species, so only that equation will be used.  The use of the 75th percentile is by far the most common and, because this function is designed for use on entire data frames, it will be the only percentile allowed.  Thus, to use equations for other percentiles, one will have to use \dQuote{manual} methods.  See \code{\link{WSlit}} and \code{\link{wsVal}} for more details about types of equations, percentiles, finding which species have published standard weight equations, etc.  See the examples and \code{\link{recodeSpecies}} for one method for changing species names to something that this function will recognize. 
#'
#' @aliases wrAdd
#'
#' @param data A data.frame that minimally contains variables of the the observed lengths, observed weights, and the species names given in the \code{formula=}.
#' @param formula A formula of the form \code{weight~length+species} where \dQuote{weight} generically represents a variable that contains weight measurements, \dQuote{length} generically represents a variable that contains length measurements, and \dQuote{species} generically represents a variable that contains species names.  Note that this formula can only contain three variables and they must be in the order of weight first, length second, species third.
#' @param units A string that indicates whether the weight and length data in \code{formula} are in (\code{"metric"} (DEFAULT; mm and g) or \code{"English"} (in and lbs) units.
#'
#' @return Returns A numeric vector that contains the computed relative weights, in the same order as in \code{data=}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{recodeSpecies}}, \code{\link{wsVal}}, and \code{\link{WSlit}}.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}.
#'
#' @keywords manip
#'
#' @examples
#' ## Create random data for three species
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
#' ## Note that the name for largemouth bass does not match what
#' ##   is found in WSlit and is thus treated as an unknown species
#' d$Wr <- wrAdd(d,wt~tl+species)
#' d
#'  
#' ## Rename species to match WSlit -- see recodeSpecies()
#' d$species1 <- recodeSpecies(d,~species,oldn=c("LMB"),newn=c("Largemouth Bass"))
#' levels(d$species1)
#' d$Wr <- wrAdd(d,wt~tl+species1)
#' d
#' 
#' ## Example with only one species in the data.frame
#' bg <- subset(d,species=="Bluegill")
#' bg <- droplevels(bg)
#' bg$Wr2 <- wrAdd(bg,wt~tl+species1)
#' 
#' @export
wrAdd <- function(data,formula,units=c("metric","English")) {
  units <- match.arg(units)
  
  ## Prepare the Ws literature values data frame
  # get is used to eliminate problem with rcmd check
  WSlit <- get(data("WSlit", envir = environment()), envir = environment())
  # isolate only those data for which those units and ref=75 exist
  WSlit <- droplevels(WSlit[WSlit$units==units & WSlit$ref==75,])
  
  ## Perform some checks on the formula
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=2,expNumENums=1,expNumEFacts=1)
  if (tmp$vnum!=3) stop("'formula' must have one variable on the left-hand-side\n and two variables on the right-hand-side.",call.=FALSE)
  if (!tmp$metExpNumR) stop("'formula' must have a left-hand-side.",call.=FALSE)
  if (tmp$Rclass!="numeric") stop("Variable on left-hand-side of 'formula' is not numeric (thus, not weights).",call.=FALSE)
  if (!tmp$metExpNumE) stop("'formula' must have a right-hand-side with two variables.",call.=FALSE)
  if (!tmp$metExpNumENums) stop("'formula' must have one and only one numeric variable (lengths) on right-hand-side.",call.=FALSE)
  if (!tmp$metExpNumEFacts) stop("'formula' must have one and only one factor variable (species) on right-hand-side.",call.=FALSE)
  ## modify data.frame
  # reduce to length, weight, species data (in that order)
  data <- tmp$mf[,c(tmp$ENumPos,tmp$Rpos,tmp$EFactPos)]
  # add rownums variable to data.frame so that it can be sorted
  # back to original order at the end
  data$rownums <- 1:nrow(data)
  # add Wr variable with all NAs
  data$Wr <- rep(NA,nrow(data))
  
  ## get list of species (3rd column)
  specs <- levels(factor(data[,3]))
  
  ## initiate a blank new data frame with same columns as old data frame
  ndata <- data[-c(1:nrow(data)),]
  
  ## cycle through each species where WS equations are known
  for (i in 1:length(specs)) {
    ## isolate the current species
    tmp <- data[data[,3]==specs[i],]
    ## compute Wr
    if (specs[i] %in% levels(WSlit$species)) {
      # standard weight exists for this species
      wseqn <- WSlit[WSlit$species==specs[i],]
      # predict log Ws
      predlog <- wseqn$int+wseqn$slope*log10(tmp[,1])
      # If quadratic coefficient exists then use it
      if (!is.na(wseqn$quad)) predlog <- predlog + wseqn$quad*(log10(tmp[,1])^2)
      # convert to Wr and add to tmp
      tmp$Wr <- tmp[,2]/(10^predlog)*100
      # change Wr for lengths less than min.len to NA
      tmp$Wr[tmp[,1]<wseqn$min.len] <- NA
      # change Wr for lengths greater than max.len (if it exists) to NA
      if (!is.na(wseqn$max.len)) tmp$Wr[tmp[,1]>wseqn$max.len] <- NA
    }
    # bind current species to the new data frame being created
    ndata <- rbind(ndata,tmp)
  }
  ## reorder the data.frame to match original rows
  ndata <- ndata[order(ndata$rownums),]
  ## return just the vector of Wr values
  ndata$Wr
}