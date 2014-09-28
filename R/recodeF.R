#' @title Recodes factor or character names according to user choices.
#'
#' @description Recodes factor or character names according to user choices.
#'
#' @details Recodes a factor or character vector based on the original names in \code{ocodes} to the new names in \code{ncodes}.  The user should assure that the order of the names in these two vectors match.  If the lengths of \code{oldnames} and \code{newnames} are not equal, then the function will throw an error.
#' 
#' See \code{recode} in \pkg{car} for a more flexible method.  This function is more useful for a small number of conversions or if the conversions come from a \dQuote{lookup} data.frame.
#'
#' This function may often be used as a pre-cursor to \code{\link{psdAdd}} and \code{\link{wrAdd}}.
#'
#' @param x A character or factor vector that contains the original names or a formula of the form \code{~species} where \dQuote{species} generically represents a variable that contains species names.
#' @param data A a data.frame that minimally contains the variable in \code{x} if \code{x} is a formula.  Note that this formula can only contain one variable.
#' @param ocodes A character vector that contains names that the the user wants to change to the names found in \code{ncodes}.  See details.
#' @param ncodes A character vector that will be the new names in the returned vector.  See details.
#' @param \dots Not used.
#'
#' @return A character or factor vector with the new names.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{recode} in \pkg{car}.
#'
#' @section fishR vignette: none yet.
#'
#' @keywords manip
#'
#' @examples
#' ## Create some random data for three species
#' # just to control the randomization
#' set.seed(345234534)
#' dbt <- data.frame(species=factor(rep(c("bluefin tuna"),30)),tl=round(rnorm(30,1900,300),0))
#' dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
#' dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),tl=round(rnorm(30,130,50),0))
#' dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
#' dlb <- data.frame(species=factor(rep(c("LMB"),30)),tl=round(rnorm(30,350,60),0))
#' dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
#' d <- rbind(dbt,dbg,dlb)
#' str(d)
#' levels(d$species)
#' 
#' # Example of changing just one code
#' d$spec1 <- recodeF(d$species,c("LMB"),c("Largemouth bass"))
#' levels(d$spec1)
#' 
#' # Same as above but using the formula
#' d$spec1f <- recodeF(~species,data=d,o=c("LMB"),n=c("Largemouth bass"))
#' levels(d$spec1f)
#'
#' # Example of using a data.frame that contains the matching names
#' spnms <- data.frame(old=c("BFT","BG","LMB"),
#'                     new=c("Bluefin Tuna","Bluegill","Largemouth Bass"))
#' d$spec2 <- recodeF(d$species,spnms$old,spnms$new)
#' levels(d$spec2)
#' 
#' # Example of converting two names to one
#' d$spec3 <- recodeF(d$species,c("bluefin tuna","LMB"),c("other","other"))
#' levels(d$spec3)
#'
#' @rdname recodeF
#' @export
recodeF <- function (x,...) {
  UseMethod("recodeF") 
}

#' @rdname recodeF
#' @export
recodeF.default <- function(x,ocodes,ncodes,...) {
  ## Some checks
  #  get class of x
  classx <- class(x)
  if (!(classx %in% c("factor","character"))) stop("'x' must be a factor or character vector.\n See 'recode()' in 'car' package for more flexible method.",call.=FALSE)
  #  on ocodes and ncodes
  if (length(ocodes)!=length(ncodes)) stop("Length of 'ocodes' and 'ncodes' must be equal.",call.=FALSE)
  if (!(class(ocodes) %in% c("factor","character"))) stop("'ocodes' must be a character vector.\n See 'recode()' in 'car' package for more flexible method.",call.=FALSE)
  if (!(class(ncodes) %in% c("factor","character"))) stop("'ncodes' must be a character vector.\n See 'recode()' in 'car' package for more flexible method.",call.=FALSE)
  ## Make sure that all are character (rather than factor) vectors
  x <- as.character(x)
  ocodes <- as.character(ocodes)
  ncodes <- as.character(ncodes)
  ## Do the recodings
  for (i in 1:length(ocodes)) x[x==ocodes[i]] <- ncodes[i]
  ## Return recodings
  if (classx=="factor") x <- factor(x)
  x
}


#' @rdname recodeF
#' @export
recodeF.formula <- function(x,data,ocodes,ncodes,...) {
  tmp <- iHndlFormula(x,data,expNumR=0,expNumE=1,expNumENums=0,expNumEFacts=1)
  if (tmp$vnum!=1) stop("'formula' must have only one variable.",call.=FALSE)
  recodeF.default(tmp$mf[,1],ocodes,ncodes)
}
