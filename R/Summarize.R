#'Summary statistics for a numeric or factor variable.
#'
#'Summary statistics for a single numeric or factor variable, possibly
#'separated by the levels of a factor variable.  Very similar to \code{summary}
#'for a numeric variables and \code{table} for factor variables.
#'
#'For numeric data this is the same as \code{summary} except that
#'\code{Summarize} includes the sample size, valid sample size (sample size
#'minus number of \code{NA}s) and standard deviation (i.e., \code{sd}).  Also
#'the output is ordered slightly differently.
#'
#'For a factor variable this function computes a frequency table, a percentage
#'table (if \code{percent=TRUE}), and a valid percentage table (percentage if
#'\dQuote{NA}s are excluded; if \code{percent=TRUE}).  The tables will contain
#'a total row if \code{addtotal=TRUE}.
#'
#'The \code{object} argument can be a formula of form \code{y~x} where \code{y}
#'can be either a numeric or factor variable and \code{x} can be only a factor
#'variable or \code{y~x*z} where \code{z} is a second factor variable.  More
#'complicated formulas are not supported.  When \code{y} is numeric then the
#'summary statistics of \code{y} will be computed for each level in \code{x} or
#'the combinations of the levels of \code{x} and \code{z}.  When \code{y} is a
#'factor then a two-way table will be computed.  If \code{addtotal=TRUE} then
#'row totals only will be added.  If \code{percent=TRUE} then a row percentages
#'table will be computed such that the percentages represent the percent in the
#'levels of \code{x} for each level of \code{y}.
#'
#'@aliases Summarize Summarize.default Summarize.formula
#'@param object A vector of numeric or factor data.
#'@param data An optional data frame that contains the variables in the model.
#'@param digits A numeric that indicates the number of decimals to round the
#'numeric summaries to.
#'@param addtotal A logical that indicates whether totals should be added to tables
#'(\code{=TRUE}, default) or not.
#'@param percent A logical that indicates whether frequency tables should
#'include percentages (\code{=TRUE}, default) or not for a single categorical
#'variable or a string that indicates the type of percents to compute for a two-way
#'table constructed wtih the formula.
#'@param percdigs A numeric that indicates the number of decimals to round the
#'percentage summaries to.
#'@param na.rm A logical that indicates whether numeric missing values
#'(\code{NA}) should be removed (\code{=TRUE}, default) or not.
#'@param exclude A string that contains the code that should be excluded from the
#'levels of the factor variable.
#'@param ... Other arguments to the generic \code{summary}, \code{sd}, or
#'\code{table} functions.
#'@return A named vector or data frame (when a quantitative variable is
#'separted by one or two factor variables) of summary statistics for numeric
#'data and a matrix of frequencies and, possibly, percentages for factor variables.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@note The \code{Summarize} function, when applied to a vector of quantitative
#'data, produces results of basic statistics similar to that provided by
#'\code{summary}.  The primary addition in the results of \code{Summarize} is
#'the inclusion of the standard deviation and the (potential) calculation of a
#'valid sample size.  When applied to a vector of categorical data,
#'\code{Summarize} produces a frequency table with (by default) percentages and
#'(perhaps) valid percentages.  The results for categorical data are NOT meant
#'to replace the \code{table} function but to provide an alternative and to
#'provide a useful result if the student provides it with categorical data.
#'
#'Students often need to look at basic statistics of a quantitative variable
#'separated for different levels of a categorical variable.  This type of
#'analysis can be made with \code{tapply}, \code{by}, or \code{aggregate} (or a
#'few other functions in other packages) but the use of these functions are not
#'obvious to newbie students or return results in a format that is not obvious
#'to newbie students.  Thus, the formula method to the \code{Summarize} generic
#'function allows newbie students to use a common notation (i.e., formula) to
#'easily compute summary statistics for a quantitative variable separated by
#'the levels of a factor.
#'@seealso \code{summary}, \code{table}, \code{tapply}, \code{summaryBy} in \pkg{doBy},
#'\code{describe} in \pkg{psych}, \code{describe} in \pkg{prettyR}, and 
#'\code{basicStats} in \pkg{fBasics}.
#'@keywords misc
#'@examples
#'## Create a numeric vector
#'y <- c(0,0,runif(98))
#'
#'# typical output of summary()
#'summary(y)   
#'
#'# this function           
#'Summarize(y)
#'
#'# this function, but controlling the number of digits
#'Summarize(y,digits=3)  
#'
#'## Factor vector (excluding "NA"s in second call)
#'x <- factor(sample(c("A","B","C","NA"),100,replace=TRUE))
#'Summarize(x)
#'Summarize(x,exclude="NA")
#'
#'## Factor vector with UNKNOWNs
#'z <- factor(sample(c("male","female","UNKNOWN"),100,replace=TRUE))
#'Summarize(z)
#'Summarize(z,exclude="UNKNOWN")
#'
#'## Numeric vector by levels of a factor variable
#'Summarize(y~x,digits=3)
#'Summarize(y~x,digits=3,exclude="NA")
#'Summarize(y~z,digits=3)
#'Summarize(y~z,digits=3,exclude="UNKNOWN")
#'
#'#### Using the data= argument
#'## create a data.frame with two extra quantitative variables
#'w <- sample(1:3,100,replace=TRUE)
#'v <- sample(1:3,100,replace=TRUE)
#'df <- data.frame(y,w,v,x,z)
#'
#'## Single variables using formula notation
#'Summarize(y~1,data=df,digits=3)
#'Summarize(x~1,data=df,exclude="NA")
#'
#'## Summarize quantitative by one or two factor variables
#'Summarize(y~z,data=df,digits=3,exclude="UNKNOWN")
#'Summarize(y~x*z,data=df,digits=3)
#'Summarize(y~x*z,data=df,digits=3,exclude="NA")
#'Summarize(y~x*z,data=df,digits=3,exclude=c("NA","UNKNOWN"))
#'
#'## What happens if RHS of formula is not a factor
#'Summarize(y~w,data=df)
#'Summarize(y~w*v,data=df)
#'
#'## Summarize factor variable by a factor variable
#'Summarize(x~z,data=df)
#'Summarize(x~z,data=df,exclude="NA")
#'Summarize(x~z,data=df,exclude=c("NA","UNKNOWN"))
#'
#'## Summarizing all variables in a data frame
#'lapply(as.list(df),Summarize,digits=4)
#'
#'@rdname Summarize
#'@export Summarize
Summarize <- function(object, ...) {
  UseMethod("Summarize")   
}

#'@rdname Summarize
#'@method Summarize default
#'@S3method Summarize default
Summarize.default <- function(object,digits=getOption("digits"),addtotal=TRUE,percent=TRUE,percdigs=2,na.rm=TRUE,exclude="",...) {
  ## internal function for quantitative data
  srzdQ <- function(object,digits,na.rm,...) {
    # remove NAs if na.rm==TRUE
    if (na.rm) object <- object[!is.na(object)]
    # get overall sample size
    n <- length(object)
    zrs <- length(object[object==0])
    mean <- mean(object,na.rm=na.rm,...)
    sd <- sd(object,na.rm=na.rm,...)
    s <- summary(object,na.rm=na.rm,...)[c("Min.","1st Qu.","Median","3rd Qu.","Max.")]   
    # count NAs and valid n if na.rm==FALSE
    if (!na.rm) {
      nas <- length(which(is.na(object)))
      valid.n <- n-nas
      res <- c(n,nas,valid.n,mean,sd,s,zrs/valid.n*100)
      names(res) <- c("n","NAs","nValid","mean","sd","min","Q1","median","Q3","max","percZero")
    } else {
      res <- c(n,mean,sd,s,zrs/n*100)
      names(res) <- c("n","mean","sd","min","Q1","median","Q3","max","percZero")
    }
    round(res,digits)
  } ## end srzQ internal function
  
  ## internal function for categorical data
  srzdC <- function(object,addtotal,percent,percdigs,exclude,...) {
    # Summary table
    res <- table(object,exclude=exclude,...)
    # Add percents to table if asked for
    if (percent) {
      # Add percents, forms a 2-col matrix
      res <- cbind(res,round(prop.table(res)*100,percdigs))
      colnames(res) <- c("freq","perc")                                          
      # Adds a valid percent column if any of the rownames are NA
      if (any(rownames(res)=="NA")) {
        # reorders rownames so that "NA" is last
        rn <- rownames(res)
        rn1 <- c(rn[rn!="NA"],"NA")
        res <- res[rn1,]
        vp <- c(round(prop.table(table(object,exclude="NA"))*100,percdigs),0)
        res <- cbind(res,vp)
        colnames(res) <- c("freq","perc","validPerc")
      }
    }
    # Adds a total of each column to table
    if (addtotal) res <- addmargins(res,margin=1,FUN=list(Total=sum),quiet=TRUE)
    res                                                                            
  } ## end srzC internal function
    
  ## Main function  
   # Do some checking on object type
   if (is.data.frame(object)) stop("Summarize does not work on data.frames.  See ??Summarize or ??summary.",call.=FALSE)
   if (is.matrix(object)) 
     if (is.numeric(object) & ncol(object)>1) stop("Summarize does not work on matrices.  See ??summary.",call.=FALSE)
       else object <- as.numeric(object[,1])                                    # convert 1-d numeric matrix to vector
   # Start processing
   if (is.numeric(object)) { srzdQ(object,digits,na.rm,...) }                   # quantitative data
     else if (is.factor(object)) { srzdC(object,addtotal,percent,percdigs,exclude,...) }   # Categorical data
       else summary(object)                                                     # A pass-through to the original summary function
}

#'@rdname Summarize
#'@method Summarize formula
#'@S3method Summarize formula
Summarize.formula <- function(object,data=NULL,digits=getOption("digits"),percent=c("row","column","total","none"),percdigs=2,addtotal=TRUE,na.rm=TRUE,exclude="",...) {
  ## internal function to check that a variable on RHS is a factor
   chckRHSfactors <- function(mf) {
     numvars <- ncol(mf)
     if (any(attr(attr(mf, "terms"),"dataClasses")[2:numvars]!="factor")) {
      warning("To continue, variable(s) on RHS of formula were converted to a factor.\n",call.=FALSE)
      for (i in 2:numvars) {
        mf[,i] <- factor(mf[,i])
      }
     }
     mf
   } ## end chckRHSfactors internal function
  ## internal function for quantitative data
  srzfQ <- function(object,mf,digits,na.rm,...) {
    if (dim(mf)[2]>3) stop("With a quantitative response (LHS), the RHS must contain only one or two factors.",call.=FALSE)
    mf <- chckRHSfactors(mf)
    if (dim(mf)[2]==2) { # Get results for quant variable by each level of a single factor variable
      intres <- tapply(mf[,1],mf[,2],Summarize,na.rm=na.rm)
      lvl.names <- names(mf)[2]
    } else { 
      mf[,4] <- interaction(mf[,2],mf[,3],sep=":")
      # Get results for quant variable by each level of interaction variable.
      intres <- tapply(mf[,1],mf[,4],Summarize,na.rm=na.rm)
      lvl.names <- names(mf)[c(2,3)]
    }
    # Put together as a matrix
    res <- round(do.call(rbind,intres),digits)
    # get colnames of tapply object
    res.names <- colnames(res)
    # split rownames of tapply object into component parts
    lvl.lbls <- t(data.frame(strsplit(rownames(res),"\\:")))
    # remove attribute names on the level labels
    attr(lvl.lbls,"dimnames") <- NULL
    # put together as a data.frame
    res <- data.frame(lvl.lbls,res)
    # make sure colnames of new data.frame make sense
    names(res) <- c(lvl.names,res.names)
    # eliminate row names
    rownames(res) <- NULL
    # eliminate rows that correspond to level in exclude
    if (!is.null(exclude)) {
      res <- res[!(res[,1] %in% exclude),]
      if (dim(mf)[2]>2) res <- res[!(res[,2] %in% exclude),]
    }
    res
  } ## end srzfQ internal function
  
  ## internal function for categorical data
  srzfC <- function(object,mf,percent,percdigs,addtotal,exclude,...) {
    if (dim(mf)[2]>2) stop("With a categorical response (LHS), the RHS must contain only one factor.",call.=FALSE)
    if (attr(attr(mf, "terms"),"dataClasses")[2]!="factor") {
      warning("To continue, variable(s) on RHS of formula were converted to a factor.\n",call.=FALSE)
      mf[,2] <- factor(mf[,2])
    }
    res <- table(mf[,2],mf[,1],exclude=exclude)
    if (percent!="none") {
      if (percent=="total") mrgn=NULL
        else mrgn <- c(1,2)[which(percent==c("row","column"))]
      res <- prop.table(res,margin=mrgn)*100
    }
    if (addtotal) {
      if(percent %in% c("total","none")) mrgn <- 1:2
        else mrgn <- c(2,1)[which(percent==c("row","column"))]
      res <- addmargins(res,margin=mrgn,FUN=list(Total=sum),quiet=TRUE)
    }
    if (percent!="none") res <- formatC(res,digits=percdigs,format="f")                                                                            
    res
  } ## end srzC internal function
    
  ## Main function  
   percent <- match.arg(percent)
   # get model frame
   mf <- model.frame(object,data=data)
   # start processing
   # handle case of simple formula with one variable first
   if (dim(mf)[2]==1) { Summarize(mf[,1],digits=digits,percent=ifelse(percent=="none",FALSE,TRUE),percdigs=percdigs,addtotal=addtotal,na.rm=na.rm,exclude=exclude,...) }
   else { # more complex formulas with more than one variable
    if (attr(attr(mf, "terms"), "dataClasses")[1]=="numeric") { srzfQ(object,mf,digits,na.rm,...) }  # quantitative response variable
    else { srzfC(object,mf,percent,percdigs,addtotal,exclude,...) }             # categorical response variable
  }
}
