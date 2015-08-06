#' @title Summary statistics for a numeric or factor variable.
#'
#' @description Summary statistics for a single numeric or factor variable, possibly separated by the levels of a factor variable.  Very similar to \code{summary} for a numeric variables and \code{table} for factor variables.
#'
#' @details For numeric data this is the same as \code{summary} except that \code{Summarize} includes the sample size, valid sample size (sample size minus number of \code{NA}s) and standard deviation (i.e., \code{sd}).  Also the output is ordered slightly differently.
#'
#'For a factor variable this function computes a frequency table, a percentage table (if \code{percent=TRUE}), and a valid percentage table (percentage if \dQuote{NA}s are excluded; if \code{percent=TRUE}).  The tables will contain a total row if \code{addtotal=TRUE}.
#'
#'The \code{object} argument can be a formula of form \code{y~x} where \code{y} can be either a numeric or factor variable and \code{x} can be only a factor variable or \code{y~x*z} where \code{z} is a second factor variable.  More complicated formulas are not supported.  When \code{y} is numeric then the summary statistics of \code{y} will be computed for each level in \code{x} or the combinations of the levels of \code{x} and \code{z}.  When \code{y} is a factor then a two-way table will be computed.  If \code{addtotal=TRUE} then row totals only will be added.  If \code{percent=TRUE} then a row percentages table will be computed such that the percentages represent the percent in the levels of \code{x} for each level of \code{y}.
#'
#' @note The \code{Summarize} function, when applied to a vector of quantitative data, produces results of basic statistics similar to that provided by \code{summary}.  The primary addition in the results of \code{Summarize} is the inclusion of the standard deviation and the (potential) calculation of a valid sample size.  When applied to a vector of categorical data, \code{Summarize} produces a frequency table with (by default) percentages and (perhaps) valid percentages.  The results for categorical data are NOT meant to replace the \code{table} function but to provide an alternative and to provide a useful result if the student provides it with categorical data.
#'
#' Students often need to look at basic statistics of a quantitative variable separated for different levels of a categorical variable.  This type of analysis can be made with \code{tapply}, \code{by}, or \code{aggregate} (or a few other functions in other packages) but the use of these functions are not obvious to newbie students or return results in a format that is not obvious to newbie students.  Thus, the formula method to the \code{Summarize} generic function allows newbie students to use a common notation (i.e., formula) to easily compute summary statistics for a quantitative variable separated by the levels of a factor.
#'
#' @aliases Summarize Summarize.default Summarize.formula
#'
#' @param object A vector of numeric or factor data.
#' @param data An optional data frame that contains the variables in the model.
#' @param digits A numeric that indicates the number of decimals to round the numeric summaries to.
#' @param addtotal A logical that indicates whether totals should be added to tables (\code{=TRUE}, default) or not.
#' @param percent A logical that indicates whether frequency tables should include percentages (\code{=TRUE}, default) or not for a single categorical variable or a string that indicates the type of percents to compute for a two-way table constructed wtih the formula.
#' @param percdigs A numeric that indicates the number of decimals to round the percentage summaries to.
#' @param na.rm A logical that indicates whether numeric missing values (\code{NA}) should be removed (\code{=TRUE}, default) or not.
#' @param exclude A string that contains the code that should be excluded from the levels of the factor variable.
#' @param \dots Other arguments to the generic \code{summary}, \code{sd}, or \code{table} functions.
#'
#' @return A named vector or data frame (when a quantitative variable is separted by one or two factor variables) of summary statistics for numeric data and a matrix of frequencies and, possibly, percentages for factor variables.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link{summary}}, \code{\link{table}}, and \code{\link{xtabs}} for related one dimenstional functionality.  See \code{\link{tapply}}, \code{\link[doBy]{summaryBy}} in \pkg{doBy}, \code{\link[psych]{describe}} in \pkg{psych}, \code{\link[prettyR]{describe}} in \pkg{prettyR}, and \code{\link[fBasics]{basicStats}} in \pkg{fBasics} for similar \dQuote{by} functionality.
#'
#' @keywords misc
#'
#' @examples
#' ## Create a numeric vector (with missing values)
#' n <- 102
#' y <- c(0,0,NA,NA,NA,runif(n-5))
#' ## Create a factor vector (with missing values)
#' g1 <- factor(sample(c("A","B","C","NA"),n,replace=TRUE))
#' ## Create a factor vector with unknowns
#' g2 <- factor(sample(c("male","female","UNKNOWN"),n,replace=TRUE))
#' # Put into a data.frame (with some extra variables)
#' d <- data.frame(dy=y,dg1=g1,dg2=g2,
#'                 dw=sample(1:3,n,replace=TRUE),
#'                 dv=sample(1:3,n,replace=TRUE))
#' 
#' # typical output of summary() for a numeric variable
#' summary(y)   
#'
#' # this function           
#' Summarize(y)
#' Summarize(~dy,data=d)
#' Summarize(dy~1,data=d)
#' # control the number of digits
#' Summarize(~dy,data=d,digits=3)  
#'
#' ## Factor vector (excluding "NA"s in second call)
#' Summarize(~dg1,data=d)
#' Summarize(~dg1,data=d,exclude="NA")
#'
#' ## Factor vector with UNKNOWNs
#' Summarize(~dg2,data=d)
#' Summarize(~dg2,data=d,exclude="UNKNOWN")
#'
#' ## Numeric vector by levels of a factor variable
#' Summarize(dy~dg1,data=d,digits=3)
#' Summarize(dy~dg1,data=d,digits=3,exclude="NA")
#' Summarize(dy~dg2,data=d,digits=3)
#' Summarize(dy~dg2,data=d,digits=3,exclude="UNKNOWN")
#'
#' ## What happens if RHS of formula is not a factor
#' Summarize(dy~dw,data=d)
#' Summarize(y~dw*dv,data=d)
#'
#' ## Summarize factor variable by a factor variable
#' Summarize(dg1~dg2,data=d)
#' Summarize(dg1~dg2,data=d,exclude="NA")
#' Summarize(dg1~dg2,data=d,exclude=c("NA","UNKNOWN"))
#' Summarize(dg1~dg2,data=d,percent="none")
#' Summarize(dg1~dg2,data=d,percent="column")
#' Summarize(dg1~dg2,data=d,percent="total")
#'
#' ## Summarizing all variables in a data frame
#' lapply(as.list(d),Summarize,digits=4)
#'
#' @rdname Summarize
#' @export
Summarize <- function(object, ...) {
  UseMethod("Summarize")   
}

#' @rdname Summarize
#' @export
Summarize.default <- function(object,digits=getOption("digits"),
                              addtotal=TRUE,percent=TRUE,percdigs=2,
                              na.rm=TRUE,exclude="",...) {
  ## Do some checking on object type
  if (is.data.frame(object)) stop("'Summarize' does not work with a data.frame.",call.=FALSE)
  if (is.matrix(object)) 
   if (is.numeric(object) & ncol(object)>1) stop("Summarize does not work with matrices.",call.=FALSE)
   else object <- as.numeric(object[,1])  # convert 1-d numeric matrix to vector
  ## Start processing
  if (is.numeric(object)) {
   # quantitative data
   iSummarizeQ1(object,digits,na.rm,...)
  } else if (is.factor(object)) {
    # Categorical data
    iSummarizeC1(object,addtotal,percent,percdigs,exclude,...)
  } else summary(object) # A pass-through to the original summary function
}

#' @rdname Summarize
#' @export
Summarize.formula <- function(object,data=NULL,digits=getOption("digits"),
                              percent=c("row","column","total","none"),percdigs=2,
                              addtotal=TRUE,na.rm=TRUE,exclude="",...) {
   percent <- match.arg(percent)
   ## Handle the formula
   tmp <- iHndlFormula(object,data,expNumR=1)
   ## Start Processing
   if (tmp$vnum==1) {
     ## Only one variable ... send first column of model.frame
     ## to summarize.default
     Summarize(tmp$mf[,1],digits=digits,percent=ifelse(percent=="none",FALSE,TRUE),
               percdigs=percdigs,addtotal=addtotal,na.rm=na.rm,exclude=exclude,...)
   } else {
     ## More than one variable
     if (!tmp$metExpNumR) stop("Must have one variable on RHS of formula with more than two variables",call.=FALSE)
     if (tmp$Rclass %in% c("numeric","integer")) {
       ## Quantitative response
       iSummarizeQf(tmp,digits,na.rm,exclude,...)
     } else {
       ## Categorical response
       iSummarizeCf(tmp,percent,percdigs,addtotal,exclude,...)
     }
   }
}


##############################################################
## Internal function for vector of quantitative data
##############################################################
iSummarizeQ1 <- function(object,digits,na.rm,...) {
  # remove NAs if na.rm==TRUE
  if (na.rm) object <- object[!is.na(object)]
  # get overall sample size
  n <- length(object)
  zrs <- length(object[object==0])
  mean <- mean(object,na.rm=na.rm,...)
  sd <- stats::sd(object,na.rm=na.rm,...)
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
}

##############################################################
## Internal function for vector of categorical data
##############################################################
iSummarizeC1 <- function(object,addtotal,percent,percdigs,exclude,...) {
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
  if (addtotal) res <- stats::addmargins(res,margin=1,FUN=list(Total=sum),quiet=TRUE)
  res                                                                            
}

##############################################################
## Internal function for formula with quantitative response
##############################################################
iSummarizeQf <- function(tmp,digits,na.rm,exclude,...) {
  ## Get the response variable
  nv <- tmp$mf[,tmp$Rpos]
  ## Make sure LHS is simple enough
  if (tmp$Enum>2) stop("With a quantitative response (LHS), the RHS\n must contain only one or two factors.",call.=FALSE)
  ## Handle the explanatory variables
  if (tmp$Enum==1) { ## Only one explanatory variable
    rv <- tmp$mf[,tmp$Enames]
    if (tmp$Eclass!="factor") {
      warning("RHS variable was converted to a factor.",call.=FALSE)
      rv <- factor(rv)
    }
    # Get results for quant variable by each level of a single factor variable
    intres <- tapply(nv,rv,Summarize,na.rm=na.rm)
  } else { ## Two explanatory variables
    rv <- tmp$mf[,tmp$Enames[1]]
    if (tmp$Eclass[1]!="factor") {
      warning("First RHS variable was converted to a factor.",call.=FALSE)
      rv <- factor(rv)
    }
    cv <- tmp$mf[,tmp$Enames[2]]
    if (tmp$Eclass[2]!="factor") {
      warning("Second RHS variable was converted to a factor.",call.=FALSE)
      cv <- factor(cv)
    }
    iv <- interaction(rv,cv,sep=":")
    # Get results for quant variable by each level of interaction variable.
    intres <- tapply(nv,iv,Summarize,na.rm=na.rm)
  }
  # Put together as a matrix
  res <- round(do.call(rbind,intres),digits)
  # get colnames of matrix version of tapply object
  res.names <- colnames(res)
  # split rownames of tapply object into component parts
  lvl.lbls <- t(data.frame(strsplit(rownames(res),"\\:")))
  # remove attribute names on the level labels
  attr(lvl.lbls,"dimnames") <- NULL
  # put together as a data.frame
  res <- data.frame(lvl.lbls,res)
  # make sure colnames of new data.frame make sense
  # should be names of explanatory variables and the colnames
  # from the tapply result
  names(res) <- c(tmp$Enames,res.names)
  # eliminate row names
  rownames(res) <- NULL
  # eliminate rows that correspond to level in exclude
  if (!is.null(exclude)) {
    res <- res[!(res[,1] %in% exclude),]
    if (tmp$Enum>2) res <- res[!(res[,2] %in% exclude),]
  }
  res
}

##############################################################
## Internal function for formula with categorical response
##############################################################
iSummarizeCf <- function(tmp,percent,percdigs,addtotal,exclude,...) {
  ## Get response variable
  rv <- tmp$mf[,tmp$Rpos]
  ## Check RHS has only one variable
  if (tmp$Enum>1) stop("With a categorical response (LHS), the RHS\n must contain only one factor.",call.=FALSE)
  ## Get explanatory variable, make sure it is a factor
  ev <- tmp$mf[,tmp$Enames[1]]
  if (tmp$Eclass!="factor") {
    warning("Variable on RHS of 'formula' converted to a factor.\n",call.=FALSE)
    ev <- as.factor(ev)
  }
  res <- table(ev,rv,exclude=exclude)
  if (percent!="none") {
    if (percent=="total") mrgn=NULL
    else mrgn <- c(1,2)[which(percent==c("row","column"))]
    res <- prop.table(res,margin=mrgn)*100
  }
  if (addtotal) {
    if(percent %in% c("total","none")) mrgn <- 1:2
    else mrgn <- c(2,1)[which(percent==c("row","column"))]
    res <- stats::addmargins(res,margin=mrgn,FUN=list(Total=sum),quiet=TRUE)
  }
  if (percent!="none") res <- round(res,digits=percdigs)                                                  
  res
}