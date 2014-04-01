#'Constructs length class/category variable.
#'
#'Constructs a new variable that contains the length class or category that an
#'individual fish belongs to and appends that new variable to the original
#'data.frame.
#'
#'If \code{breaks=NULL} (the default) then this function will create length
#'categories that begin with the value in \code{startcat} and continue by
#'values of \code{w} until a category value greater than the largest
#'observation in \code{data[,cl]}.  In this instance categories of different
#'widths are not allowed.  The length categorizations are left-inclusive and
#'right-exclusive by default (i.e., \code{right=FALSE}).  The number in the
#'\code{startcat} argument should be less than the smallest value in
#'\code{data[,cl]}.  In addition the number of decimals in \code{startcat} should
#'not be more than the number of decimals in \code{w}.  For example,
#'\code{startcat=0.4} and \code{w=1} will result in the function being
#'terminated with an error.
#'
#'If \code{breaks} is non-NULL then \code{startcat} and \code{w} will be
#'ignored.  In this instance the vector of values in \code{breaks} should begin
#'with a value at least as small as the minimum observation and end with a
#'value at least as big as the maximum observation.  If the lowest break value
#'is larger than the minimum observation then the function will stop with an
#'error.  If the largest break is smaller than the maximum observation then an
#'additional break larger than the maximum observation will be added to breaks.
#'The use of \code{breaks} allows the use of length categories of different widths.
#'
#'The observed values in the \code{data[,cl]} should be rounded to the
#'appropriate number of decimals to avoid misplacement of individuals into
#'incorrect length categories due to machine-precision issues.  For example,
#'see discussion in \code{all.equal} function.
#'
#'If no variable name is supplied in \code{vname} then the default variable
#'name will be \code{LCat}.
#'
#'@param formula A formula of the form \code{~length} where \dQuote{length} generically
#'represents a variable in \code{data} that contains length measurements.  Note
#'that this formula can only contain one variable.
#'@param data A data.frame that minimally contains the length measurements given
#'in the variable in the \code{formula}.
#'@param startcat A number indicating the beginning of the first length-class.
#'@param w A number indicating the width of length classes to create.
#'@param breaks A numeric vector of lower values for the break points of the
#'length categories.
#'@param right A logical indicating if the intervals should be closed on the
#'right (and open on the left) or vice versa.
#'@param vname A string containing the name for the new length class variable.
#'@param as.fact A logical indicating if the new variable should be returned as
#'a factor (\code{=TRUE}; default) or not.
#'@param drop.levels A logical indicating if the new variable should retain all
#'levels indicated in \code{breaks} (\code{=FALSE}; default) or not.  This is
#'ignored if \code{as.fact=FALSE}.
#'@param use.catnames A logical indicating whether the names for the values in 
#'\code{breaks} should be used for the levels in the new variable.  Will throw
#'a warning and then use default levels if \code{TRUE} but \code{names(breaks)}
#'is \code{NULL}.
#'@return Returns a data frame that consists of the original data frame, \code{data},
#'with the new length category variable appended and named as \code{vname}.
#'@export
#'@keywords manip
#'@examples
#'# random lengths measured to nearest 0.1 unit
#'df1 <- data.frame(len=round(runif(50,0.1,9.9),1))
#'
#'# length categories by 0.1 unit starting at 0
#'df1a <- lencat(~len,data=df1,startcat=0,w=0.1)
#'table(df1a$LCat)
#'
#'# length categories by 0.2 units starting at 0
#'df1b <- lencat(~len,data=df1,startcat=0,w=0.2)
#'table(df1b$LCat)
#'
#'# length categories by 0.2 units starting at 0.1
#'df1c <- lencat(~len,data=df1,startcat=0.1,w=0.2)
#'table(df1c$LCat)
#'
#'# length categories as set by breaks
#'df1d <- lencat(~len,data=df1,breaks=c(0,2,4,7,10))
#'table(df1d$LCat)
#'
#'
#'## A Second example
#'# random lengths measured to nearest unit
#'df2 <- data.frame(len=round(runif(50,10,117),0))    
#'
#'# length categories by 5 units starting at 0
#'df2a <- lencat(~len,data=df2,startcat=0,w=5)
#'table(df2a$LCat)
#'
#'# length categories by 5 units starting at 7
#'df2b <- lencat(~len,data=df2,startcat=7,w=5)
#'table(df2b$LCat)
#'
#'# length categories by 10 units starting at 5
#'df2c <- lencat(~len,data=df2,startcat=5,w=10)
#'table(df2c$LCat)
#'
#'# length categories as set by breaks
#'df2d <- lencat(~len,data=df2,breaks=c(5,50,75,150))
#'table(df2d$LCat)
#'
#'
#'## A Third example
#'# random lengths measured to nearest 0.1 unit
#'df3 <- data.frame(len=round(runif(50,10,117),1))
#'
#'# length categories by 5 units starting at 0
#'df3a <- lencat(~len,data=df3,startcat=0,w=5)
#'table(df3a$LCat)
#'
#'
#'## A Fourth example
#'# random lengths measured to nearest 0.01 unit
#'df4 <- data.frame(len=round(runif(50,0.1,9.9),2))
#'
#'# length categories by 0.1 unit starting at 0
#'df4a <- lencat(~len,data=df4,startcat=0,w=0.1)
#'table(df4a$LCat)
#'
#'# length categories by 2 unit starting at 0
#'df4b <- lencat(~len,data=df4,startcat=0,w=2)
#'table(df4b$LCat)
#'
#'
#'## A Fifth example -- with real data
#'data(SMBassWB)
#'#remove variables with "anu" and "radcap" just for simplicity
#'smb1 <- SMBassWB[,-c(8:20)]
#'
#'# summarize to help find a good starting category
#'summary(smb1$lencap)
#'
#'# 10 mm length classes - in default LCat variable
#'smb1 <- lencat(~lencap,data=smb1,startcat=50,w=10)
#'view(smb1)
#'
#'# 25 mm length classes - in custom variable name
#'smb1 <- lencat(~lencap,data=smb1,startcat=50,w=25,vname="LenCat25")
#'view(smb1)
#'
#'# using values from psdVal for Smallmouth Bass
#'smb1 <- lencat(~lencap,data=smb1,breaks=psdVal("Smallmouth Bass"),vname="LenPsd")
#'view(smb1)
#'# add category names
#'smb1 <- lencat(~lencap,data=smb1,breaks=psdVal("Smallmouth Bass"),vname="LenPsd2",use.catnames=TRUE)
#'view(smb1)
#'
lencat <- function(formula,data,startcat=0,w=1,breaks=NULL,right=FALSE,vname=NULL,
                   as.fact=TRUE,drop.levels=FALSE,use.catnames=FALSE) {
  make.vname <- function(vname,data) { ## internal function
    # if no name given then default to "LCat"
    if (is.null(vname)) vname <- "LCat"
    # create list of names that includes vname & vname with numbers appended 
    vnames <- c(vname,paste(vname,seq(1:100),sep=""))
    # find first instance where names match
    ind <- which(vnames %in% names(data))
    # if no match then go with given vname
    if (length(ind)==0) vname <- vname
      # if is match then go with name that is one index later in vnames
      else vname <- vnames[max(ind)+1]
    vname
  } ## end internal function

  cl <- getVarFromFormula(formula,data,expNumVars=1)
  if (is.null(breaks)) {
    decs <- checkStartcatW(startcat,w,data[,cl])  
    # Creates cut breaks (one more than max so that max is included in a break)
    breaks <- seq(startcat,max(data[,cl],na.rm=TRUE)+w,w)
    breaks <- round(breaks,decs$wdec)
  } else {
    if (min(data[,cl],na.rm=TRUE) < breaks[1])
      stop(paste("Lowest break (",breaks[1],") is larger than minimum observation (",min(data[,cl]),").\n  Adjust the breaks you used.",sep=""),call.=FALSE)
    if (max(data[,cl],na.rm=TRUE) >= max(breaks)) {
      breaks <- c(breaks,1.1*max(data[,cl],na.rm=TRUE))
    }
  }
  lcat <- breaks[cut(data[,cl],breaks,labels=FALSE,right=right,include.lowest=TRUE)]
  if (as.fact) {
    # Generally don't drop levels but need to drop the "extra" last level in all cases
    if (!drop.levels) lcat <- factor(lcat,levels=breaks[-length(breaks)])
    else lcat <- factor(lcat)
  }
  # Puts length class variable in data.frame
  nd <- data.frame(data,lcat)
  # Renames the new variable if so desired
  vname <- make.vname(vname,data)
  names(nd)[ncol(nd)] <- vname
  # adds labels variable
  if (use.catnames)
    if (is.null(names(breaks)))
      warning("Asked to use labels but 'breaks' is not named.  Used default lables",call.=FALSE)
    else {
    # if breaks has names, add new var with those names by comparing to numeric breaks
    # also delete old numeric breaks
    nd[,vname] <- names(breaks)[match(nd[,vname],breaks)]
    # make sure the labels are ordered as ordered in breaks
    nd[,vname] <- factor(nd[,vname],levels=names(breaks))
    # refactor to remove the unused breaks
    nd[,vname] <- factor(nd[,vname])
  }
  # Returns the new data.frame
  nd
}
