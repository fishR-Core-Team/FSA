#'Internal functions used in FSA.
#'
#'Internal functions used in FSA
#'
#' @note Take note of the following uses:
#'\itemize{
#'\item \code{typeoflm} is used in \code{\link{fitPlot}} and \code{\link{residPlot}}.
#'\item \code{ciLabel} is used in \code{\link{binCI}} and \code{\link{bootCase}}.
#'\item \code{hndlMultWhat} is used in \code{\link{ageBias}} and \code{\link{agePrecision}}.
#'\item \code{checkStartcatW} used in \code{\link{lencat}} and \code{\link{lenFreqExpand}}.
#'\code{\link{catchCurve}}, \code{\link{chapmanRobson}}, \code{\link{confint.nlsBoot}},
#'\code{\link{depletion}}, \code{\link{hyperCI}}, \code{\link{mrClosed}}, \code{\link{poiCI}}, and \code{\link{removal}}.
#'\item \code{ci.fp} and \code{ci.fp1} are used in \code{\link{fitPlot}}.
#'\item \code{addOutlierTestResults}, \code{addLoessLine}, and \code{getMainTitle} are all used in \code{\link{residPlot}}.
#'\item \code{listSpecies} is used in \code{\link{psdVal}}, \code{\link{psdCalc}}, \code{\link{psdPlot}}, \code{\link{wrAdd}}, and \code{\link{wsVal}}.
#'\item \code{getVarFromFormula} is used in \code{\link{ageKey}}, \code{\link{lencat}}, \code{\link{psdCalc}}, \code{\link{psdDataPrep}}, \code{\link{psdPlot}}, and \code{\link{recodeSpecies}}.
#'\item \code{psdLitCheck} is used in \code{\link{psdVal}}, \code{\link{psdCalc}}, and \code{\link{psdPlot}}.
#'\item \code{wsLitCheck} is used in \code{\link{wsVal}}
#'\item \code{makeColor} is used in \code{\link{plotBinResp}} and \code{\link{tictactoe}}.
#'\item \code{getAllDependencies}, \code{getFilePrefix}, \code{makeFilename}, \code{makeItemsToRemove}, \code{printProgressMsg}, \code{processSessionInfo} are all used in the \code{swvXXX} utility functions.
#'\item \code{pkolmogorov1x} and \code{quad_dens} are both used in \code{\link{ks2d1}} and \code{\link{ks2d2}}.
#'}
#'
#' @rdname FSA-internals
#' @keywords internal
#' @aliases .onAttach hndlFormula typeoflm 
#'ci.fp ci.fp1 
#'addOutlierTestResults addLoessLine getMainTitle
#'legendHelp listSpecies getAllDependencies getFilePrefix ciLabel hndlMultWhat
#'makeFilename makeItemsToRemove printProgressMsg processSessionInfo
#'checkStartcatW getVarFromFormula makeColor
#'pkolmogorov1x quad_dens
#'

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


##################################################################
### internal functions used for handling formulas 
##################################################################
getVarFromFormula <- function(formula,data,expNumVars=NULL) {
  varNms <- names(model.frame(formula,data=data))
  # don't "error" check the number of variables
  if (is.null(expNumVars)) varNms
  else if (length(varNms)!=expNumVars) stop("Function only works with formulas with ",expNumVars," variable",ifelse(expNumVars==1,".","s."))
  else varNms
}


hndlFormula <- function(formula,data,expNumR=NULL,
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


typeoflm <- function(mdl) {
  tmp <- hndlFormula(formula(mdl),model.frame(mdl))
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

##################################################################
### internal function to create pretty label for CIs
##################################################################
ciLabel <- function(conf.level,digits=1) {
  paste(paste(round(100*conf.level,digits),"%",sep=""),c("LCI","UCI"))
}


##################################################################
## Internal functions used to handle multiple what= arguments
##   in ageBias and agePRecision
## If more than one item then print a line return and return
##   the what vector without item in it.  This allows an easy way
##   to put space between multiple items without an extra space
##   after the last one.
##################################################################
hndlMultWhat <- function(what,item) {
  if (length(what)>1) {
    cat("\n")
    what[-pmatch(item, what)]
  }
}


##################################################################
### internal function in lencat and lenFreqExpand
##################################################################
checkStartcatW <- function(startcat,w,d) {
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


##################################################################
### internal functions used in fitPlot
##################################################################
ci.fp.1 <- function(x,conf.level) {
  t <- qt((1-conf.level)/2,gdata::nobs(x)-1)
  c(mean(x)-t*se(x),mean(x)+t*se(x))
}

ci.fp <- function(conf.level) {
  function(x) ci.fp.1(x,conf.level)
} 

##################################################################
### internal functions used in residPlot
##################################################################
addOutlierTestResults <- function(object,fv,r,alpha) {
  out <- car::outlierTest(object$mdl,cutoff=alpha)                   # get results
  num <- length(out$bonf.p)                                          # number of points returned by outlierTest
  if (num==1) {                                                      # If only one point returned then ...
    if (is.na(out$bonf.p)) num <- 0                                  #   if it is NA then p>1 ... so not a significant point
    else if (out$bonf.p>alpha) num <- 0                              #   if p>alpha then ... not a significant point
  }
  if (num>0) {                                                       # If there are significant points to be highlighted then ...
    obs <- names(out$bonf.p)                                         # Determine which observation(s) is/are "significant" outlier(s)
    if (num==1) ifelse(r[obs]<0,pos <- 3,pos <- 1)                   # Set text position based on sign of r if only one "outlier" is detected
    else pos <- thigmophobe(fv[obs],r[obs])                          # Use thigmophobe to find better text positions of more "outliers" are detected
    text(fv[obs],r[obs],obs,cex=1.25,col="red",pos=pos,xpd=TRUE)     # place labels
  }
}  # end addOutlierTestResults internal function

addLoessLine <- function(r,fv,loess.f,lwd.loess,lty.loess,col.loess) {
  lines(stats::lowess(r~fv,f=loess.f,iter=5),lwd=lwd.loess,lty=lty.loess,col=col.loess)
}  # end addLoessLine internal function

getMainTitle <- function(object,main) {
  if (is.null(main)) {                                                          # if no main title was sent to function
    frm.chr <- as.character(formula(object$mdl))                                # get formula parts
    main <- paste(frm.chr[2],frm.chr[1],frm.chr[3])                             # put together as a main title
  }
  main                                                                          # return the title (NULL if NULL was sent)
}  # end getMainTitle internal function

##################################################################
### internal functions used in residPlot and fitPlot
##################################################################
legendHelp <- function(legend) {
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

##################################################################
### internal functions used in plotBinResp and tictactoe
##################################################################
makeColor <- function(clr,transvalue) { 
  clrprts <- col2rgb(clr)/255
  rgb(clrprts[1,1],clrprts[2,1],clrprts[3,1],1/transvalue)
}


##################################################################
### internal functions used in psdVal and WrVal et al.
##################################################################
listSpecies <- function(d) {
  cat("\nSpecies name must be one of following.  Be careful of spelling and capitalization.\n")
  print(levels(d$species))
  return(invisible())
} # end internal function    

psdLitCheck <- function(data,species) {
  OK <- FALSE
  if (species=="List") listSpecies(data)
    else if (!any(levels(data$species)==species)) {
      stop("The five-cell categories do not exist for your choice of species.\n  Please type psdVal() for a list of available species.\n\n",call.=FALSE)
    }
    else OK <- TRUE
}

wsLitCheck <- function(data,species) {
  OK <- FALSE
  if (species=="List") listSpecies(data)
    else if (!any(levels(data$species)==species)) {
      stop("A Ws equation may not exist given your choices of species, units, and ref.\n  Please look carefully inside the data(WSlit) data frame.\n\n",call.=FALSE)
    }
    else OK <- TRUE
  OK
}


##################################################################
## Internal files used in swvUtils functions                                      
##################################################################
getAllDependencies <- function(pkgs) {
  inst <- installed.packages()                                                  # get a list of available packages ... this is needed for package.dependencies below
  inst <- inst[inst[,"Package"] %in% pkgs,]                                     #   isolate to the supplied packages
  deps <- NULL
  if (is.vector(inst)) n <- 1                                                   # a catch if there is only one package
  else n <- nrow(inst)
  for (i in 1:n) {                                                              # must cycle through provided packages
    d <- tools::package.dependencies(inst,,"Depends")[[i]]                      # get dependent packages for ith provided package
    if (is.matrix(d)) deps <- c(deps,d[,1])                                     # handles those with no dependent packages
    d <- tools::package.dependencies(inst,,"Imports")[[i]]                      # ditto for imported paxckages
    if (is.matrix(d)) deps <- c(deps,d[,1])
  }
  deps <- deps[deps!="R"]                                                       # remove "R" if it is listed as a dependent
  attr(deps,"names") <- NULL                                                    # remove names attribute
  deps <- sort(deps)                                                            # alphabetize
  unique(c(pkgs,deps))                                                          # return original list and dependents
}

getFilePrefix <- function(file) {
  # if no file is sent then find the .RNW file in the current working directory
  if (missing(file)) file <- unlist(strsplit(list.files(pattern=".Rnw",ignore.case=TRUE),"\\.Rnw"))
  file <- unlist(strsplit(file,"\\.Rnw"))                                       # break off .Rnw if it is there
  file <- unlist(strsplit(file,"\\.rnw"))                                       # break off .rnw if it is there  
  file
}

makeFilename <- function(file,extension,directory=NULL) {
  res <- paste(file,extension,sep="")
  if (!is.null(directory)) res <- paste(directory,res,sep="/")
  res
}

makeItemsToRemove <- function(moreItems) {
  mainItems <- c("Stangle","SweaveHooks","swvCode","swvFinish","#line","## ","graphics.off()","purl")
  c(mainItems,moreItems)
}

printProgressMsg <- function(msg) {
  cat(msg,".\n\n",sep="")
  flush.console()
}

processSessionInfo <- function() {
  mkLabel <- function(L, n) { # this is from print.sessionInfo in utils package
    vers <- sapply(L[[n]], function(x) x[["Version"]])
    pkg <- sapply(L[[n]], function(x) x[["Package"]])
    paste(pkg, vers, sep = "_")
  } # internal mkLabel
  ses <- sessionInfo()
  sys <- paste(Sys.info()["sysname"],", ",ses$platform,"\n",sep="") 
  vers <- paste(ses$R.version$version.string,"\n",sep="")
  bpkgs <- sort(ses$basePkgs) 
  bpkgsP <- paste(paste(sort(ses$basePkgs),collapse=", "),"\n",sep="")
  opkgs <- names(ses$otherPkgs)
  opkgsP <- paste(paste(sort(mkLabel(ses,"otherPkgs")),collapse=", "),"\n",sep="")
  lpkgs <- names(ses$loadedOnly)
  lpkgsP <- paste(paste(sort(mkLabel(ses,"loadedOnly")),collapse=", "),"\n",sep="")
  list(sys=sys,vers=vers,bpkgs=bpkgs,bpkgsP=bpkgsP,opkgs=opkgs,opkgsP=opkgsP,lpkgs=lpkgs,lpkgsP=lpkgsP)
}


##################################################################
## Internal files used in ks2d1 and ks2d2                                    
##################################################################
## taken from internal function in ks.test()
pkolmogorov1x <- function(x,n) {
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  j <- seq.int(from=0,to=floor(n*(1-x)))
  1-x*sum(exp(lchoose(n,j)+(n-j)*log(1-x-j/n)+(j-1)*log(x+j/n)))
}  ## end internal pkolmogorov1x

## compute densities in each quadrat around (x0,y0), which comes from d
quad_dens <- function(d,x,y,divbylen) {
  # get values of the point being tests
  x0 <- d[1]; y0 <- d[2]
  # compute the number of points in each quadrant off of that point
  res <- table(factor(sign(x-x0),levels=-1:1),
               factor(sign(y-y0),levels=-1:1))[c(1,3),c(1,3)]
  # calculate proportions in each quadrant off of that point ...
  if (divbylen) res/length(x)   # ... from entire data set
  else res/sum(res)             # ... adjusting for removal of density=0
}  ## end internal quad_dens
