#' @title Computes a vector of relative weights specific to a species in an entire data frame.
#'
#' @description This computes a vector that contains the relative weight specific to each species for all individuals in an entire data frame.
#'
#' @details This computes a vector that contains the relative weight specific to each species for all individuals in an entire data frame. The vector can be appended to an existing data.frame to create a variable that contains the relative weights for each individual. The relative weight value will be \code{NA} for each individual for which a standard weight equation does not exist in \code{\link{WSlit}}, a standard weight equation for the units given in \code{units=} does not exist in \code{\link{WSlit}}, or if the individual is shorter or longer than the lengths for which the standard weight equation should be applied. Either the linear or quadratic equation has been listed as preferred for each species, so only that equation will be used.
#' 
#' The species names in \code{species} must match the spelling and capitalization of \code{species} in \code{\link{WSlit}}. Use \code{wsVal()} to see a list of all species for which standard weight equations exist in \code{\link{WSlit}} and, more importantly, how the species names are spelled and capitalized. See the examples for one method for changing species names to something that this function will recognize.
#' 
#' Some (few) species have more than one equation listed in \code{\link{WSlit}} (for the specified units). In these instances the user must select one of the equations to use with \code{WsOpts}. \code{WsOpts} is a list of lists where the inside list contains one or more of \code{group}, \code{ref}, or \code{method} (see \code{\link{WSlit}}) required to specify a single equation for a particular species, which is the name of the inner list. See the examples for an illustration of how to use \code{WsOpts}.
#'
#' @param wt A numeric vector that contains weight measurements or a formula of the form \code{wt~len+spec} where \dQuote{wt} generically represents the weight variable, \dQuote{len} generically represents the length variable, and \dQuote{spec} generically represents the species variable. Note that this formula can only contain three variables and they must be in the order of weight first, length second, species third.
#' @param len A numeric vector that contains length measurements. Not used if \code{wt} is a formula.
#' @param spec A character or factor vector that contains the species names. Not used if \code{wt} is a formula.
#' @param data A data.frame that minimally contains variables of the the observed lengths, observed weights, and the species names given in the \code{formula=}.
#' @param units A string that indicates whether the weight and length data in \code{formula} are in (\code{"metric"} (DEFAULT; mm and g) or \code{"English"} (in and lbs) units.
#' @param WsOpts A named list that provides specific choices for \code{group}, \code{ref}, or \code{method} for species for which more than one standard weight equation exists in \code{\link{WSlit}}.
#' @param \dots Not used.
#'
#' @return Returns A numeric vector that contains the computed relative weights, in the same order as in \code{data=}.
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: 8-Condition.
#'
#' @seealso See \code{\link{wsVal}}, \code{\link{WSlit}}, and \code{\link{psdAdd}} for related functionality. See \code{\link[plyr]{mapvalues}} for help in changing species names to match those in \code{\link{WSlit}}.
#'
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#'
#' @keywords manip
#'
#' @examples
#' #===== Create random data for three species
#' #----- just to control the randomization
#' set.seed(345234534)
#' dbt <- data.frame(species=factor(rep(c("Bluefin Tuna"),30)),
#'                   tl=round(rnorm(30,1900,300),0))
#' dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
#' dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),
#'                   tl=round(rnorm(30,130,50),0))
#' dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
#' dlb <- data.frame(species=factor(rep(c("Largemouth Bass"),30)),
#'                   tl=round(rnorm(30,350,60),0))
#' dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
#' df <- rbind(dbt,dbg,dlb)
#' str(df)
#'
#' #===== Add Wr variable
#' #----- using formula interface
#' df$Wr1 <- wrAdd(wt~tl+species,data=df)
#' peek(df,n=10)
#' #----- same but with non-formula interface
#' df$Wr2 <- wrAdd(df$wt,df$tl,df$species)
#' peek(df,n=10)
#' #----- same but using dplyr
#' if (require(dplyr)) {
#'   df <- df |>
#'     mutate(Wr3=wrAdd(wt,tl,species))
#' }
#' #----- examine results
#' peek(df,n=10)
#'  
#' #===== Example with only one species in the data.frame
#' bg <- droplevels(subset(df,species=="Bluegill"))
#' bg$Wr4 <- wrAdd(wt~tl+species,data=bg)
#' bg
#' 
#' #===== Example with a species that has Ws eqns for multiple groups and a
#' #      group needs to be specified with WsOpts
#' wae <- data.frame(species=factor(rep(c("Walleye"),30)),
#'                   tl=round(rnorm(30,500,200),0))
#' wae$wt <- round(3.33e-06*wae$tl^3.16+rnorm(30,0,50),1)
#' # wae$Wr <- wrAdd(wt~tl+species,data=wae) # will err b/c multiple groups
#' wae$Wr <- wrAdd(wt~tl+species,data=wae,
#'                 WsOpts=list(Walleye=list(group="overall")))
#' peek(wae,n=10)
#' 
#' @rdname wrAdd
#' @export
wrAdd <- function (wt,...) {
  UseMethod("wrAdd") 
}

#' @rdname wrAdd
#' @export
wrAdd.default <- function(wt,len,spec,
                          units=c("metric","English"),WsOpts=NULL,...) {
  ###### Internal Function
  #===== Print error if no options given, but they are needed
  iErrOpts <- function(df) {
    #----- Restrict data.frame to species, group, ref, and method ... then show
    print(df[,c("species","group","ref","method")])
    #----- Error message for next two possible problems
    STOP("More than one Ws equation exists for \"",unique(df$species),
         "\". Please use a named list in 'opts=' to select one ",
         "equation for \"",unique(df$species),"\" by specifing 'group', ",
         "'ref', or 'method' as appropriate. See details in documentation ",
         "and above (for reference).")
  }
  
  #===== Find Ws eqn for a species, possibly given WsOpts
  iGetWs <- function(Wsdf,species,WsOpts) {
    #----- Make sure species is a character
    if (is.factor(species)) species <- as.character(species)
    #----- Reduce Wsdf to just for that species
    Wsdf <- droplevels(Wsdf[Wsdf$species==species,])
    #----- If >1 Ws eqn for species/units, then need user to further define
    #      with opts, otherise one Ws eqn is returned
    if (nrow(Wsdf)>1) {
      # ..... Need opts for species but none given (at all) so STOP
      if (is.null(WsOpts)) iErrOpts(Wsdf)
      # ..... Identify which species opts were given for
      spec_opts <- names(WsOpts)
      # ..... If current species not in opts then STOP
      if (!species %in% spec_opts) iErrOpts(Wsdf)
      # ..... If current species in opts then iteratively reduce WSlit result
      #       based on the criteria in opts
      crit_opts <- names(WsOpts[[species]])
      for (i in seq_along(crit_opts)) {
        tmp <- WsOpts[[species]][i]
        if (!names(tmp) %in% c("group","ref","method"))
          STOP("'",names(tmp),"' in 'WsOpts=' must be one of 'group', ",
               "'ref', or 'method'.")
        Wsdf <- droplevels(Wsdf[Wsdf[[names(tmp)]]==tmp,])
        if (nrow(Wsdf)==0)
          STOP("Use of '",crit_opts[i],"=",tmp[[1]],"' for \"",species,"\" did ",
               "not return a standard weight equation. Please reconsider your use ",
               "of 'WsOpts=' to restrict to only one equation for \"",species,"\".")
        else if (nrow(Wsdf)>1) {
          #..... send error if not reduced to only one Ws equation
          print(Wsdf[,c("species","group","ref","method")])
          STOP("Use of '",crit_opts[i],"=",tmp[[1]],"' for \"",species,"\" did ",
               "not result in a single standard weight equation. Please examine the ",
               "output above to appropriately make another choice for use ",
               "in 'WsOpts=' to restrict to only one equation for \"",species,"\".")
        }
      }
    }
    #----- Return appropriate part of Wsdf (i.e., WSlit)
    Wsdf
  }
  
  ###### END Internal Function

  ###### BEGIN Main Function
  #===== Some checks
  units <- match.arg(units)
  if (!is.numeric(wt)) STOP("'wt' must be numeric.")
  if (!is.numeric(len)) STOP("'len' must be numeric.")
  if (!inherits(spec,c("character","factor")))
    STOP("'spec' must be character or factor.")
  
  #===== Prepare the Ws literature values data frame
  #----- load WSlit data frame into this functions environment
  WSlit <- FSA::WSlit
  #----- isolate only those data for which those units exist
  WSlit <- droplevels(WSlit[WSlit$units==units,])
  
  #===== Create temporary results data.frame with length, weight, species,
  #      rownumbers, and Wr values (blank)
  data <- data.frame(len,wt,spec,rownums=seq_along(len),
                     Wr=rep(NA,length(len)))
  #===== Initiate a blank new data frame with same columns as old data frame
  ndata <- data[-c(seq_len(nrow(data))),]  
  #===== get list of species in data
  specs <- unique(spec)
  
  #===== cycle through each species where WS equations are known
  for (i in seq_along(specs)) {
    #----- isolate the current species
    tmp <- data[data[,3]==specs[i],]
    #----- compute Wr
    if (specs[i] %in% unique(WSlit$species)) {
      #..... standard weight exists for this species
      wseqn <- iGetWs(WSlit,specs[i],WsOpts)
      #..... predict log Ws
      predlog <- wseqn$int+wseqn$slope*log10(tmp[,1])
      #..... If quadratic coefficient exists then use it
      if (!is.na(wseqn$quad)) predlog <- predlog + wseqn$quad*(log10(tmp[,1])^2)
      #..... convert to Wr and add to tmp
      tmp$Wr <- tmp[,2]/(10^predlog)*100
      #..... change Wr for lengths less than min.len to NA
      tmp$Wr[tmp[,1]<wseqn$min.len] <- NA
      #..... change Wr for lengths greater than max.len (if it exists) to NA
      if (!is.na(wseqn$max.len)) tmp$Wr[tmp[,1]>wseqn$max.len] <- NA
    }
    #----- bind current species to the new data frame being created
    ndata <- rbind(ndata,tmp)
  }
  #===== reorder the data.frame to match original rows
  ndata <- ndata[order(ndata$rownums),]
  #===== return just the vector of Wr values
  ndata$Wr  
}

#' @rdname wrAdd
#' @export
wrAdd.formula <- function(wt,data,units=c("metric","English"),...) {
  #===== Perform some checks on the formula
  tmp <- iHndlFormula(wt,data,expNumR=1,expNumE=2,expNumENums=1,expNumEFacts=1)
  if (tmp$vnum!=3) STOP("'wt' must have one variable on the left-hand-side ",
                        "and two variables on the right-hand-side.")
  if (!tmp$metExpNumR) STOP("'wt' must have a left-hand-side.")
  if (!(tmp$Rclass %in% c("numeric","integer")))
    STOP("Variable on left-hand-side of 'wt' is not numeric (thus, not weights).")
  if (!tmp$metExpNumE) STOP("'wt' must have a right-hand-side with two variables.")
  if (!tmp$metExpNumENums)
    STOP("'wt' must have one and only one numeric variable (lengths) on right-hand-side.")
  if (!tmp$metExpNumEFacts)
    STOP("'wt' must have one and only one factor variable (species) on right-hand-side.")

  #===== Call the wrAdd.default
  wrAdd.default(tmp$mf[,tmp$Rpos],tmp$mf[,tmp$ENumPos],
                tmp$mf[,tmp$EFactPos],units,...)
}
