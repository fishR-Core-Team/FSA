#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in FSA.
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach iAddLoessLine iCheckALK iCheckStartcatW iCILabel iGetVarFromFormula iHndlCols2use iHndlFormula iHndlMultWhat iLegendHelp iListSpecies iMakeColor iTypeoflm


##################################################################
## Sends a start-up message to the console when the package is loaded.
##################################################################
.onAttach <- function(lib,pkg,...) {
  ## Get version number -- basically code from readPkgVersion in SweaveListingUtils
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),fields="Version")
  vers <- paste0(vers,"      ",ifelse(nchar(vers)==5," ",""))
  ## Send message
  msg <- paste("\n\n")
  msg <- paste(msg,"############################################\n")
  msg <- paste(msg,"##      FSA package, version",vers,"##\n")
  msg <- paste(msg,"##    Derek H. Ogle, Northland College    ##\n")
  msg <- paste(msg,"##                                        ##\n")
  msg <- paste(msg,"## Run ?FSA for documentation.            ##\n")
  msg <- paste(msg,"## Run citation('FSA') for citation ...   ##\n")
  msg <- paste(msg,"##   please cite if used in publication.  ##\n")
  msg <- paste(msg,"##                                        ##\n")
  msg <- paste(msg,"## See fishR.wordpress.com for more       ##\n")    
  msg <- paste(msg,"##   thorough analytical vignettes.       ##\n")
  msg <- paste(msg,"############################################\n\n")
  packageStartupMessage(msg)
}


iAddLoessLine <- function(r,fv,lty.loess,lwd.loess,col.loess,trans.loess,span=0.75) {
  mdl <- loess(r~fv,span=span)
  xrng <- range(fv)
  xseq <- seq(from=xrng[1],to=xrng[2],length=80)
  pred <- predict(mdl,newdata=data.frame(fv=xseq),se=TRUE)
  y <- pred$fit
  ci <- pred$se.fit*qt(0.95/2+.5,pred$df)
  ymin <- y-ci
  ymax <- y+ci
  polygon(c(xseq,rev(xseq)),c(ymin,rev(ymax)),col=iMakeColor(col.loess,trans.loess),border=NA,xpd=FALSE)
  lines(y~xseq,lwd=lwd.loess,lty=lty.loess,col=col.loess,xpd=FALSE)
}  # end iAddLoessLine internal function


iCheckALK <- function(key,only1=FALSE,remove0rows=FALSE) {
  #### only1=TRUE ... only check if rows sum to 1, otherwise also check if they sum to 0
  #### remove0rows=TRUE ... remove the rows that sum to 0.
  
  ## Check that row names and column names can be considered as numeric
  options(warn=-1) ## turn off warnings
  tmp <- as.numeric(rownames(key))
  options(warn=0)
  if (any(is.na(tmp))) stop("The row names of 'key' must be numeric (and\n contain the minimum value of the lenth intervals).",call.=FALSE)
  options(warn=-1) ## turn off warnings
  tmp <- as.numeric(colnames(key))
  options(warn=0)
  if (any(is.na(tmp))) stop("The column names of 'key' must be numeric\n (and contain age values).",call.=FALSE)  
  ## Check if key is proportions, if not change to proportions
  if (any(key>1,na.rm=TRUE)) {
    warning("'key' contained values >1; to continue, assumed\n values were frequencies and converted to row proportions.",call.=FALSE)
    key <- prop.table(key,margin=1)
  }
  ## Remove rows that sum to 0 or NA (i.e., only keeps lens with data in key)
  key.rowSum <- rowSums(key,na.rm=TRUE)
  if (remove0rows & any(key.rowSum==0)) {
    warning("'key' contained rows that sum to 0; as requested,\n these rows were removed from the table.",call.=FALSE)
    key <- key[!is.na(key.rowSum) & key.rowSum!=0,]
  }
  ## Check if rows sum to 1 (allow for some minimal rounding error and does not consider zeroes )
  if (any(key.rowSum>0.01 & (key.rowSum<0.99 | key.rowSum>1.01))) warning("Key contained a row that does not sum to 1.",call.=FALSE)
  ## Check if rows sum to 0 (allow for some minimal rounding error)
  if (!only1 & !remove0rows & any(key.rowSum==0)) warning("Key contained rows that sum to 0.",call.=FALSE)
  ## Return the potentially modified key
  key
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


iHndlCols2use <- function(df,cols2use,cols2ignore) {
  ## if both cols2use and cols2ignore are NULL then return the original df
  if (is.null(cols2use) & is.null(cols2ignore)) {
    return(df)
  } else {
    ## Can't use both cols2use and cols2ignore
    if (!is.null(cols2use) & !is.null(cols2ignore)) stop("Cannot use both 'cols2use' and 'cols2ignore'.",call.=FALSE)
    ## Handle cols2use
    if (!is.null(cols2use)) {
      ## Convert character column names to numeric
      if (is.character(cols2use)) cols2use <- which(cols2use %in% names(df))
    } else {
      ## Handle col2ignore
      ## convert character column names to numeric
      if (is.character(cols2ignore)) cols2ignore <- which(cols2ignore %in% names(df))
      ## Convert numeric col2ignore to negative if all are positive
      if (all(cols2ignore>0)) cols2ignore <- -cols2ignore
      ## put negative cols2ignore into cols2use
      cols2use <- cols2ignore
    }
    ## Return data.frame of only columns asked for
    return(df[,cols2use])
  }
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
    # add one to positions if Rnum==1
    if (Rnum==1) {
      ENumPos <- ENumPos + 1
      EFactPos <- EFactPos + 1
    }
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
  ## Takes a color string and will make it transparent based on
  ##  the value of transvalue.  The transvalue value must be greater
  ##  than 0.  If transvalue is greater than 1 than it is interpreted
  ##  as the number of points plotted on top of each other before the
  ##  transparency is lost and is, thus, transformed to 1/transvalue.
  ## The return value is an rgb() color.
  if (transvalue < 0) stop("'transvalue' must be greater than 0.")
  if (transvalue > 1) transvalue <- 1/transvalue
  clrprts <- col2rgb(clr)/255
  rgb(clrprts[1,1],clrprts[2,1],clrprts[3,1],transvalue)
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
