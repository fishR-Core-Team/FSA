#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in FSA.
#'
#' @details Take note of the following uses:
#'  \itemize{
#'    \item \code{iCheckStartcatW} used in \code{\link{lencat}} and \code{\link{lenFreqExpand}}.
#'    \item \code{iCILabel} is used in \code{\link{binCI}}, \code{\link{bootCase}}, \code{\link{catchCurve}}, \code{\link{chapmanRobson}}, \code{\link{confint.nlsBoot}}, \code{\link{depletion}}, \code{\link{hyperCI}}, \code{\link{mrClosed}}, \code{\link{poiCI}}, \code{\link{predict.nlsBoot}}, \code{\link{removal}}.
#'    \item \code{iGetVarFromFormula} is used in \code{\link{ageKey}}, \code{\link{lencat}}, \code{\link{psdCalc}}, \code{\link{psdDataPrep}}, \code{\link{psdPlot}}, \code{\link{recodeSpecies}}, \code{\link{wrAdd}}, and \code{\link{wrDataPrep}}. 
#'    \item \code{iHndlFormula} is used in \code{\link{ageBias}}, \code{\link{agePrecision}}, \code{\link{growthModelSim}}, \code{\link{vbStarts}},  \code{\link{walfordPlot}}, and \code{\link{chapmanPlot}}.
#'    \item \code{iHndlMultWhat} is used in \code{\link{ageBias}} and \code{\link{agePrecision}}.
#'    \item \code{iLegendHelp} is used in \code{\link{fitPlot}} and \code{\link{residPlot}}.
#'    \item \code{iListSpecies} is used in \code{\link{psdVal}} and \code{\link{wsVal}}.
#'    \item \code{iMakeColor} is used in \code{\link{plotBinResp}} and \code{\link{tictactoe}}.
#'    \item \code{iTypeoflm} is used in \code{\link{fitPlot}} and \code{\link{residPlot}}.
#'  }
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach iCheckStartcatW iCILabel iGetVarFromFormula iHndlFormula iHndlMultWhat iLegendHelp iListSpecies iMakeColor iTypeoflm


##################################################################
## Sends a start-up message to the console when the package is loaded.
##################################################################
.onAttach <- function(lib,pkg,...) {
  ## Get version number -- basically code from readPkgVersion in SweaveListingUtils
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),fields="Version")
  ## Send message
  msg <- paste("\n\n")
  msg <- paste(msg,"############################################\n")
  msg <- paste(msg,"##","FSA package, version",vers,"            ##\n")
  msg <- paste(msg,"##   by Derek H. Ogle, Northland College  ##\n")
  msg <- paste(msg,"##                                        ##\n")
  msg <- paste(msg,"## Type ?FSA for documentation.           ##\n")
  msg <- paste(msg,"## Type citation('FSA') for citation      ##\n")
  msg <- paste(msg,"##   please cite if used in publication.  ##\n")
  msg <- paste(msg,"##                                        ##\n")
  msg <- paste(msg,"## See fishR.wordpress.com for more       ##\n")    
  msg <- paste(msg,"##   thorough analytical vignettes.       ##\n")
  msg <- paste(msg,"############################################\n\n")
  packageStartupMessage(msg)
}


iCheckStartcatW <- function(startcat,w,d) {
  # is w positive?
  if (w<=0) stop("\n Width value must be positive.",call.=FALSE)
  # is startcat positive? 
  if (startcat < 0) stop("\n Starting category values must be non-negative.",call.=FALSE)
  # is startcat less than minimum observation
  if (round(min(d,na.rm=TRUE),w) < round(startcat,w))
    stop("\n Starting category is larger than minimum observation.  Adjust the startcat that you used.",call.=FALSE)
  # get decimals for w and startcat
  # Used in next line to find number of decimals to use
  z <- c(1,0.1,0.01,0.001,0.0001,0.00001,0.000001,0.0000001)
  # Determine number of decimals from width value
  wdec <- which(w/z>=1)[1]-1
  # Determine number of decimals from startcat value
  ifelse(startcat>0,scdec <- which(startcat/z>=1)[1]-1,scdec <- 0)
  # does w have more than (or equal) decimals as startcat 
  if (scdec>wdec) stop("\n Starting category value should not have more decimals than width value",call.=FALSE)
  # return decimals
  list(scdec=scdec,wdec=wdec)
}


iCILabel <- function(conf.level,digits=1) paste(paste(round(100*conf.level,digits),"%",sep=""),c("LCI","UCI"))


iGetVarFromFormula <- function(formula,data,expNumVars=NULL) {
  varNms <- names(model.frame(formula,data=data))
  # don't "error" check the number of variables
  if (is.null(expNumVars)) varNms
  else if (length(varNms)!=expNumVars) stop("Function only works with formulas with ",expNumVars," variable",ifelse(expNumVars==1,".","s."))
  else varNms
}


iHndlFormula <- function(formula,data,expNumR=NULL,
                        expNumE=NULL,expNumENums=NULL,expNumEFacts=NULL) {
  
  mf <- model.frame(formula,data=data)
  if (ncol(mf)==1) {
    # Only one variable in the model frame.  Return only the model.frame, name of 
    #   that variable, and it's class.
    return(list(mf=mf,vnum=1,vname=names(mf),vclass=class(mf[,1])))
  } else {
    # More than one variable in the formula.
    # Must identify if there is a LHS.
    ifelse(attr(terms(formula),"response")==0,LHS <- FALSE, LHS <- TRUE)
    # See if more than one variable on LHS
    if (LHS) {
      fcLHS <- as.character(formula)[2]
      ifelse(any(c("*","+") %in% substring(fcLHS,1:nchar(fcLHS),1:nchar(fcLHS))),LHSgt1 <- TRUE, LHSgt1 <- FALSE)
      # STOP if there is more than one variable on LHS
      if (LHSgt1) stop("Function does not work with more than one variable on the LHS.",call.=FALSE)
      else {
        # There is a LHS and it has only one variable.
        Rpos <- Rnum <- 1
        Rname <- names(mf)[Rpos]
        Rmf <- mf[,Rpos]
        Rclass <- class(Rmf)
        Epos <- 2:ncol(mf)
        Enames <- names(mf)[Epos]
        Enum <- length(Enames)
        Emf <- mf[,Epos]
      }
    } else {
      # There is not a LHS
      Rnum <- 0
      Rpos <- Rname <- Rclass <- NULL
      Rmf <- NULL
      Emf <- mf
      Enames <- names(Emf)
      Enum <- length(Enames)
      Epos <- 1:Enum      
    }
    # find the class of each response and explanatory variable on the RHS
    if (Enum>0) ifelse(Enum==1,Eclass <- class(Emf), Eclass <- unlist(lapply(Emf,class)))
    # get positions of numeric and factor explanatory vars on RHS
    ENumPos <- which(Eclass %in% c("numeric","integer","AsIs"))
    EFactPos <- which(Eclass=="factor")
    # get number of numeric and number of factor explanatory vars on RHS
    ENumNum <- length(ENumPos)
    EFactNum <- length(EFactPos)
  }
  # Identify the type of data at hand
  Etype <- "mixed"
  if (ENumNum==0) Etype <- "factor"
  if (EFactNum==0) Etype <- "numeric"
  # Recreate the model frame data frame
  if (Rnum==0) df <- Emf
  else df <- data.frame(Rmf,Emf)
  names(df) <- c(Rname,Enames)
  # Check if the expected number of each type of variable was met
  metExpNumR <- metExpNumE <- metExpNumENums <- metExpNumEFacts <- NULL
  if (!is.null(expNumR)) ifelse(Rnum==expNumR,metExpNumR <- TRUE,metExpNumR <- FALSE)
  if (!is.null(expNumE)) ifelse(Enum==expNumE,metExpNumE <- TRUE,metExpNumE <- FALSE)
  if (!is.null(expNumENums)) ifelse(ENumNum==expNumENums,metExpNumENums <- TRUE,metExpNumENums <- FALSE)
  if (!is.null(expNumEFacts)) ifelse(EFactNum==expNumEFacts,metExpNumEFacts <- TRUE,metExpNumEFacts <- FALSE)
  # put it all together to return
  list(formula=formula,mf=df,vnum=Rnum+Enum,
       Rnum=Rnum,Rname=Rname,Rclass=Rclass,Rpos=Rpos,
       Etype=Etype,Enames=Enames,Eclass=Eclass,Enum=Enum,
       ENumNum=ENumNum,ENumPos=ENumPos,
       EFactNum=EFactNum,EFactPos=EFactPos,
       metExpNumR=metExpNumR,metExpNumE=metExpNumE,
       metExpNumENums=metExpNumENums,metExpNumEFacts=metExpNumEFacts)
}


iHndlMultWhat <- function(what,item) {
  ##################################################################
  ## Internal functions used to handle multiple what= arguments
  ## If more than one item then print a line return and return
  ##   the what vector without item in it.  This allows an easy way
  ##   to put space between multiple items without an extra space
  ##   after the last one.
  ##################################################################
  if (length(what)>1) {
    cat("\n")
    what[-pmatch(item, what)]
  }
}


iLegendHelp <- function(legend) {
  do.legend <- FALSE
  x <- y <- NULL
  if (class(legend)=="logical") {
    if(legend) {
      do.legend <- TRUE
      x <- locator(1)
    }
  } else if (!is.null(legend)) {
    do.legend <- TRUE
    if (class(legend)=="character") {
      x <- legend
    } else {
      x <- legend[1]; y <- legend[2]
    }
  }
  list(do.legend = do.legend, x=x, y=y)
}


iListSpecies <- function(d) {
  cat("\nSpecies name must be one of following.  Be careful of spelling and capitalization.\n")
  print(levels(d$species))
  return(invisible())
} # end internal function    


iMakeColor <- function(clr,transvalue) { 
  clrprts <- col2rgb(clr)/255
  rgb(clrprts[1,1],clrprts[2,1],clrprts[3,1],1/transvalue)
}


iTypeoflm <- function(mdl) {
  tmp <- iHndlFormula(formula(mdl),model.frame(mdl))
  if (tmp$Enum==0) stop("Object must have one response and at least one explanatory variable",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("Response variable must be numeric",call.=FALSE)
  if (tmp$Etype=="factor") { #ANOVA
    if (tmp$EFactNum>2) stop("Function only works for one- or two-way ANOVA.")
    if (tmp$EFactNum==2) lmtype <- "TWOWAY"
    else lmtype <- "ONEWAY"
  } else { # not an anova
    if (tmp$Enum==1) lmtype <- "SLR"
    else if (tmp$Etype=="mixed") lmtype <- "IVR"
    else if (all(grepl(tmp$Enames[1],tmp$Enames[-1]))) lmtype <- "POLY"
    else lmtype <- "MLR"
  }
  tmp <- c(list(type=lmtype,mdl=mdl),tmp)
  class(tmp) <- c(lmtype,"list")
  tmp
}
