#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in FSA.
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach iAddLoessLine iCheckALK iCheckStartcatW iCILabel iGetDecimals  iGetVarFromFormula iHndlCols2UseIgnore iHndlFormula iHndlMultWhat iLegendHelp iListSpecies iMakeColor iPlotExists is.wholenumber iTypeoflm STOP WARN


##################################################################
## Sends a start-up message to the console when the package is loaded.
##################################################################
.onAttach <- function(lib,pkg,...) {
  vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),fields="Version")
  msg <- paste0("## FSA v",vers,". See citation('FSA') if used in publication.\n")
  msg <- paste0(msg,"## Run fishR() for related website and fishR('IFAR') for related book.")    
  packageStartupMessage(msg)
}


iAddLoessLine <- function(r,fv,lty.loess,lwd.loess,col.loess,trans.loess,span=0.75) {
  options(warn=-1)  # suppress warnings from loess
  mdl <- stats::loess(r~fv,span=span)
  xseq <- seq(from=min(fv),to=max(fv),length=80)
  pred <- stats::predict(mdl,newdata=data.frame(fv=xseq),se=TRUE)
  options(warn=0)   # unsuppress warnings
  graphics::polygon(c(xseq,rev(xseq)),
          c(pred$fit-pred$se.fit*stats::qt(0.975,pred$df),
            rev(pred$fit+pred$se.fit*stats::qt(0.975,pred$df))),
          col=col2rgbt(col.loess,trans.loess),border=NA,xpd=FALSE)
  graphics::lines(pred$fit~xseq,lwd=lwd.loess,lty=lty.loess,col=col.loess,xpd=FALSE)
}  # end iAddLoessLine internal function


iCheckALK <- function(key,only1=FALSE,remove0rows=FALSE) {
  #### only1=TRUE ... only check if rows sum to 1, otherwise also check if they sum to 0
  #### remove0rows=TRUE ... remove the rows that sum to 0.
  
  ## Check that row names and column names can be considered as numeric
  options(warn=-1) ## turn off warnings
  tmp <- as.numeric(rownames(key))
  options(warn=0)
  if (any(is.na(tmp))) STOP("The row names of 'key' must be numeric (and\n contain the minimum value of the lenth intervals).")
  options(warn=-1) ## turn off warnings
  tmp <- as.numeric(colnames(key))
  options(warn=0)
  if (any(is.na(tmp))) STOP("The column names of 'key' must be numeric\n (and contain age values).")  
  ## Check if key is proportions, if not change to proportions
  if (any(key>1,na.rm=TRUE)) {
    WARN("'key' contained values >1; to continue, assumed\n values were frequencies and converted to row proportions.")
    key <- prop.table(key,margin=1)
  }
  ## Remove rows that sum to 0 or NA (i.e., only keeps lens with data in key)
  key.rowSum <- rowSums(key,na.rm=TRUE)
  if (remove0rows & any(key.rowSum==0)) {
    WARN("'key' contained rows that sum to 0; as requested,\n these rows were removed from the table.")
    key <- key[!is.na(key.rowSum) & key.rowSum!=0,]
  }
  ## Check if rows sum to 1 (allow for some minimal rounding error and does not consider zeroes )
  if (any(key.rowSum>0.01 & (key.rowSum<0.99 | key.rowSum>1.01))) WARN("Key contained a row that does not sum to 1.")
  ## Check if rows sum to 0 (allow for some minimal rounding error)
  if (!only1 & !remove0rows & any(key.rowSum==0)) WARN("Key contained rows that sum to 0.")
  ## Return the potentially modified key
  key
}


iCheckW <- function(w) {
  if (!is.numeric(w)) STOP("'w' must be numeric.")
  if (length(w)>1) STOP("'w' must be a single value.")
  if (w<=0) STOP("'w' must be positive.")
  # returns decimals of w
  iGetDecimals(w)
}

iCheckStartcat <- function(startcat,w,d) {
  if (!is.numeric(startcat)) STOP("'startcat' must be numeric.")
  if (length(startcat)>1) STOP("'startcat' must be a single value.")
  if (startcat<0) STOP("'startcat' must be non-negative.")
  if (round(min(d,na.rm=TRUE),w) < round(startcat,w)) STOP("'startcat' is larger than the minimum observation.")
  # return decimals of w
  iGetDecimals(startcat)
}

iCheckStartcatW <- function(startcat,w,d) {
  wdec <- iCheckW(w)
  scdec <- iCheckStartcat(startcat,w,d)
  # does w have more than (or equal) decimals as startcat 
  if (scdec>wdec) STOP("'startcat' should not have more decimals than 'w'.",
                       call.=FALSE)
  # return decimals
  list(scdec=scdec,wdec=wdec)
}


iCILabel <- function(conf.level,digits=1) paste(paste0(round(100*conf.level,digits),"%"),
                                                c("LCI","UCI"))



################################################################################
## Returns number of decimal places in a number
## 
## Completely from Peter Savicky in http://r.789695.n4.nabble.com/number-of-decimal-places-in-a-number-td4635697.html
################################################################################
iGetDecimals <- function(x) {
  if (!is.numeric(x)) STOP("'x' must be numeric.")
  if (length(x)>1) STOP("'x' must be a single value.")
  if (is.integer(x)) 0
  else {
    tmp <- format.info(x,digits=10)
    if (tmp[3]!=0) WARN("'x' will be presented in exponential notation.\nReturned decimals may not be what you expected.")
    tmp[2]
  }
}


iGetVarFromFormula <- function(formula,data,expNumVars=NULL) {
  varNms <- names(stats::model.frame(formula,data=data))
  # don't "error" check the number of variables
  if (is.null(expNumVars)) varNms
  else if (length(varNms)!=expNumVars) STOP("Function only works with formulas with ",expNumVars," variable",ifelse(expNumVars==1,".","s."))
  else varNms
}


iHndlCols2UseIgnore <- function(df,cols2use=NULL,cols2ignore=NULL) {
  iHndlCols2Use <- function(df,cols2use) {
    if (!inherits(cols2use,c("integer","numeric","character")))
      STOP("'cols2use' must be a numeric index or column name.")
    if (is.character(cols2use)) {
      ## Convert character column names to numeric
      cols2use <- which(names(df) %in% cols2use)
      if (length(cols2use)==0) STOP("None of columns in 'cols2use' exist in 'df'.")
    } else { ## numeric column choices
      if (any(cols2use<0) & any(cols2use>0)) STOP("'cols2use' must be all positive or all negative.")
      if (any(abs(cols2use)>ncol(df))) STOP("Some 'cols2use' do not exist in 'df'.")
    }
    cols2use
  }
  iHndlCols2Ignore <- function(df,col2ignore) {
    if (!inherits(cols2ignore,c("integer","numeric","character")))
      STOP("'cols2ignore' must be a numeric index or column name.")
    if (is.character(cols2ignore)) {
      ## Convert character column names to numeric
      cols2ignore <- which(names(df) %in% cols2ignore)
      if (length(cols2ignore)==0) STOP("None of columns in 'cols2ignore' exist in 'df'.")
    } else {
      if (any(cols2ignore<0) & any(cols2ignore>0)) STOP("'cols2ignore' must be all positive or all negative.")
      cols2ignore <- abs(cols2ignore)
      if (any(cols2ignore==0)) STOP("A 'cols2ignore' cannot be zero.")
      if (any(cols2ignore>ncol(df))) STOP("Some 'cols2ignore' do not exist in 'df'.")
    }    
    -cols2ignore
  }

  ## if both cols2use and cols2ignore are NULL, return the original df
  if (is.null(cols2use) & is.null(cols2ignore)) ind <- 1:ncol(df)
  else if (!is.null(cols2use) & !is.null(cols2ignore)) STOP("Cannot use both 'cols2use' and 'cols2ignore'.")
  else if (!is.null(cols2use)) ind <- iHndlCols2Use(df,cols2use)
  else ind <- iHndlCols2Ignore(df,cols2ignore)
  ## Return data.frame of only columns asked for
  res <- df[,ind,drop=FALSE]
  if (ncol(res)==0) WARN("Resultant data.frame contains no columns.")
  res
}


iHndlFormula <- function(formula,data,expNumR=NULL,
                         expNumE=NULL,expNumENums=NULL,expNumEFacts=NULL) {
  mf <- stats::model.frame(formula,data=data,na.action=NULL)
  if (ncol(mf)==1) {
    # One variable.  Return only model.frame, name of variable, and it's class.
    #   but handle an odd case where the item is an array by returning the mode
    return(list(mf=mf,vnum=1,vname=names(mf),
                vclass=ifelse(is.array(mf[,1]),mode(mf[,1]),class(mf[,1]))))
  } else {
    # More than one variable in the formula.
    # Must identify if there is a LHS.
    ifelse(attr(stats::terms(formula),"response")==0,LHS <- FALSE, LHS <- TRUE)
    # See if more than one variable on LHS
    if (LHS) {
      fcLHS <- as.character(formula)[2]
      ifelse(any(c("*","+") %in% substring(fcLHS,1:nchar(fcLHS),1:nchar(fcLHS))),LHSgt1 <- TRUE, LHSgt1 <- FALSE)
      # STOP if there is more than one variable on LHS
      if (LHSgt1) STOP("Function does not work with more than one variable on the LHS.")
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


iHndlMultWhat <- function(what,item,type=c("message","cat")) {
  ##################################################################
  ## Internal functions used to handle multiple what= arguments
  ## If more than one item then print a line return and return
  ##   the what vector without item in it.  This allows an easy way
  ##   to put space between multiple items without an extra space
  ##   after the last one.
  ##################################################################
  type <- match.arg(type)
  if (length(what)>1) {
    if (type=="message") message("\n")
    else cat("\n")
    what[-pmatch(item, what)]
  }
}


iLegendHelp <- function(legend) {
  do.legend <- FALSE
  x <- y <- NULL
  if (inherits(legend,"logical")) {
    if(legend) { # nocov start
      do.legend <- TRUE
      x <- graphics::locator(1)
    } # nocov end
  } else if (!is.null(legend)) {
    do.legend <- TRUE
    if (inherits(legend,"character")) {
      x <- legend
    } else {
      x <- legend[1]
      y <- legend[2]
    }
  }
  list(do.legend=do.legend,x=x,y=y)
}


iListSpecies <- function(d) {
  message("\nSpecies name must be one of following.  Be careful of spelling and capitalization.")
  print(levels(d$species))
  return(invisible())
} # end internal function    


iMakeColor <- function(col,transp) { 
  ## Takes a color string and will make it transparent based on
  ##  the value of transp.  The transp value must be greater
  ##  than 0.  If transp is greater than 1 than it is interpreted
  ##  as the number of points plotted on top of each other before the
  ##  transparency is lost and is, thus, transformed to 1/transp.
  ## The return value is an rgb() color.
  if (transp <= 0) STOP("'transp' must be greater than 0.")
  if (transp > 1) transp <- 1/transp
  colprts <- grDevices::col2rgb(col)/255
  grDevices::rgb(colprts[1,1],colprts[2,1],colprts[3,1],transp)
}



################################################################################
# Checks if a plot exists ... i.e., was there a plot.new
################################################################################
iPlotExists <- function() {
  # set options so that warnings are errors
  oldwarn <- options("warn")[[1]]
  options(warn=2)
  # if plot does not exist that par(new=TRUE) will error and then
  #   try will return a class of "try-error"
  res <- try(graphics::par(new=TRUE),silent=TRUE)
  # if errored then say FALSE (i.e., plot does not exist)
  res <- ifelse(class(res)=="try-error",FALSE,TRUE)
  # return
  options(warn=oldwarn)
  res
}


################################################################################
# Checks if a value is a whole number
################################################################################
is.wholenumber <- function(x,tol=.Machine$double.eps^0.5) {
  abs(x - round(x)) < tol 
}


iTypeoflm <- function(mdl) {
  if (any(class(mdl)!="lm")) STOP("'iTypeoflm' only works with objects from 'lm()'.")
  tmp <- iHndlFormula(stats::formula(mdl),stats::model.frame(mdl))
  if (tmp$Enum==0) STOP("Object must have one response and at least one explanatory variable")
  if (!tmp$Rclass %in% c("numeric","integer")) STOP("Response variable must be numeric")
  if (any(tmp$Eclass=="character")) WARN("An explanatory variable is a 'character' class. If behavior is different\n than you expected you may want to change this to a 'factor' class.")
  if (tmp$Etype=="factor") { #ANOVA
    if (tmp$EFactNum>2) STOP("Function only works for one- or two-way ANOVA.")
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

################################################################################
# same as stop() and warning() but with call.=FALSE as default
################################################################################
STOP <- function(...,call.=FALSE,domain=NULL) stop(...,call.=call.,domain=domain)
WARN <- function(...,call.=FALSE,immediate.=FALSE,noBreaks.=FALSE,domain=NULL) {
  warning(...,call.=call.,immediate.=immediate.,noBreaks.=noBreaks.,domain=domain)
}
