#' @title Summary statistics for a numeric variable.
#'
#' @description Summary statistics for a single numeric variable, possibly separated by the levels of a factor variable or variables. This function is very similar to \code{\link[base]{summary}} for a numeric variable.
#'
#' @details This function is primarily used with formulas of the following types (where \code{quant} and \code{factor} generically represent quantitative/numeric and factor variables, respectively):
#' \tabular{ll}{
#'   Formula \tab Description of Summary \cr
#'   \code{~quant} \tab Numerical summaries (see below) of \code{quant}.\cr
#'   \code{quant~factor} \tab Summaries of \code{quant} separated by levels in \code{factor}.\cr
#'   \code{quant~factor1*factor2} \tab Summaries of \code{quant} separated by the combined levels in \code{factor1} and \code{factor2}.\cr
#' }
#' 
#' Numerical summaries include all results from \code{\link[base]{summary}} (min, Q1, mean, median, Q3, and max) and the sample size, valid sample size (sample size minus number of \code{NA}s), and standard deviation (i.e., \code{sd}). \code{NA} values are removed from the calculations with \code{na.rm=TRUE} (the DEFAULT). The number of digits in the returned results are controlled with \code{digits=}.
#' 
#' @note Students often need to examine basic statistics of a quantitative variable separated for different levels of a categorical variable. These results may be obtained with \code{\link[base]{tapply}}, \code{\link[base]{by}}, or \code{\link[stats]{aggregate}} (or with functions in other packages), but the use of these functions is not obvious to newbie students or return results in a format that is not obvious to newbie students. Thus, the formula method to \code{Summarize} allows newbie students to use a common notation (i.e., formula) to easily compute summary statistics for a quantitative variable separated by the levels of a factor.
#'
#' @aliases Summarize Summarize.default Summarize.formula
#'
#' @param object A vector of numeric data.
#' @param digits A single numeric that indicates the number of decimals to round the numeric summaries.
#' @param na.rm A logical that indicates whether numeric missing values (\code{NA}) should be removed (\code{=TRUE}, default) or not.
#' @param exclude A string that contains the level that should be excluded from a factor variable.
#' @param data A data.frame that contains the variables in \code{formula}.
#' @param nvalid A string that indicates how the \dQuote{validn} result will be handled. If \code{"always"} then \dQuote{validn} will always be shown and if \code{"never"} then \dQuote{validn} will never be shown. However, if \code{"different"} (DEFAULT), then \dQuote{validn} will only be shown if it differs from \dQuote{n} (or if at least one group differs from \dQuote{n} when summarized by multiple groups).
#' @param percZero A string that indicates how the \dQuote{percZero} result will be handled. If \code{"always"} then \dQuote{percZero} will always be shown and if \code{"never"} then \dQuote{percZero} will never be shown. However, if \code{"different"} (DEFAULT), then \dQuote{percZero} will only be shown if it is greater than zero (or if at least one group is greater than zero when summarized by multiple groups).
#' @param \dots Not implemented.
#'
#' @return A named vector or data frame (when a quantitative variable is separated by one or two factor variables) of summary statistics for numeric data.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link[base]{summary}} for related one dimensional functionality. See \code{\link[base]{tapply}}, \code{\link[doBy]{summaryBy}} in \pkg{doBy}, \code{\link[psych]{describe}} in \pkg{psych}, \code{\link[prettyR]{describe}} in \pkg{prettyR}, and \code{\link[fBasics]{basicStats}} in \pkg{fBasics} for similar \dQuote{by} functionality.
#'
#' @keywords misc
#'
#' @examples
#' ## Create a data.frame of "data"
#' n <- 102
#' d <- data.frame(y=c(0,0,NA,NA,NA,runif(n-5)),
#'                 w=sample(7:9,n,replace=TRUE),
#'                 v=sample(0:2,n,replace=TRUE),
#'                 g1=factor(sample(c("A","B","C",NA),n,replace=TRUE)),
#'                 g2=factor(sample(c("male","female","UNKNOWN"),n,replace=TRUE)),
#'                 g3=sample(c("a","b","c","d"),n,replace=TRUE),
#'                 stringsAsFactors=FALSE)
#' 
#' # typical output of summary() for a numeric variable
#' summary(d$y)   
#'
#' # this function           
#' Summarize(d$y,digits=3)
#' Summarize(~y,data=d,digits=3)
#' Summarize(y~1,data=d,digits=3)
#'
#' # note that nvalid is not shown if there are no NAs and
#' #   percZero is not shown if there are no zeros
#' Summarize(~w,data=d,digits=3)
#' Summarize(~v,data=d,digits=3)
#' 
#' # note that the nvalid and percZero results can be forced to be shown
#' Summarize(~w,data=d,digits=3,nvalid="always",percZero="always")
#' 
#' ## Numeric vector by levels of a factor variable
#' Summarize(y~g1,data=d,digits=3)
#' Summarize(y~g2,data=d,digits=3)
#' Summarize(y~g2,data=d,digits=3,exclude="UNKNOWN")
#'
#' ## Numeric vector by levels of two factor variables
#' Summarize(y~g1+g2,data=d,digits=3)
#' Summarize(y~g1+g2,data=d,digits=3,exclude="UNKNOWN")
#' 
#' ## What happens if RHS of formula is not a factor
#' Summarize(y~w,data=d,digits=3)
#'
#' ## Summarizing multiple variables in a data.frame (must reduce to numerics)
#' lapply(as.list(d[,1:3]),Summarize,digits=4)
#'
#' @rdname Summarize
#' @export
Summarize <- function(object, ...) {
  UseMethod("Summarize")   
}

#' @rdname Summarize
#' @export
Summarize.default <- function(object,digits=getOption("digits"),
                              na.rm=TRUE,exclude=NULL,
                              nvalid=c("different","always","never"),
                              percZero=c("different","always","never"),...) {
  nvalid <- match.arg(nvalid)
  percZero <- match.arg(percZero)
  ## Do some checking on object type
  if (is.data.frame(object)) STOP("'Summarize' does not work with a data.frame.")
  if (is.matrix(object)) {
    if (ncol(object)>1) STOP("'Summarize' does not work with matrices.")
    else {
      # convert 1-d matrices to vectors
      if (is.numeric(object)) object <- as.numeric(object[,1])
      else object <- as.factor(object[,1])
    }
  }
  ## Start processing
  if (!is.numeric(object)) STOP("'Summarize' only works with a numeric variable")
  else iSummarizeQ1(object,digits,na.rm,nvalid,percZero)
}

#' @rdname Summarize
#' @export
Summarize.formula <- function(object,data=NULL,digits=getOption("digits"),
                              na.rm=TRUE,exclude=NULL,
                              nvalid=c("different","always","never"),
                              percZero=c("different","always","never"),...) {
  nvalid <- match.arg(nvalid)
  percZero <- match.arg(percZero)
  ## Handle the formula
  tmp <- iHndlFormula(object,data,expNumR=1)
  ## Start Processing
  if (tmp$vnum==1) {
    ## Only one variable ... send first column of model.frame
    ## to summarize.default
    Summarize(tmp$mf[,1],digits=digits,na.rm=na.rm,
              exclude=exclude,nvalid,percZero,...)
  } else {
    ## More than one variable
    if (!tmp$metExpNumR) STOP("Must have one variable on LHS of formula with more than one variable")
    if (tmp$Rclass %in% c("numeric","integer")) {
      iSummarizeQf(tmp,digits,na.rm,exclude,nvalid,percZero)
    } else STOP("'Summarize' only works with a numeric variable on LHS.")
  }
}


##############################################################
## Internal function for vector of quantitative data
##############################################################
iSummarizeQ1 <- function(object,digits,na.rm,nvalid,percZero) {
  ## get overall sample size
  n <- length(object)
  ## get nvalid and number of NAs
  n.valid <- validn(object)
  ## count zeros (must get rid of NAs first ... 
  ##    otherwise counted as zeros)
  tmp <- object[!is.na(object)]
  zrs <- length(tmp[tmp==0])
  ## compute mean and sd
  mean <- mean(object,na.rm=na.rm)
  sd <- stats::sd(object,na.rm=na.rm)
  ## get other summary values
  tmp <- summary(object,na.rm=na.rm)[c("Min.","1st Qu.","Median","3rd Qu.","Max.")]
  res <- c(mean,sd,tmp)
  names(res) <- c("mean","sd","min","Q1","median","Q3","max")
  ## Add on percent that are zero???
  if (percZero=="always" | (percZero=="different" & zrs>0)) {
    res <- c(res,zrs/n.valid*100)
    names(res)[length(res)] <- "percZero"
  }
  ## Round all results to this point
  res <- round(res,digits)  
  ## Add on valid n???
  if (nvalid=="always" | (nvalid=="different" & n.valid!=n)) {
    res <- c(n.valid,res)
    names(res)[1] <- "nvalid"
  }
  ## Add on n
  res <- c(n,res)
  names(res)[1] <- "n"
  ## Return the result
  res
}

##############################################################
## Internal function for formula with quantitative response
##############################################################
iSummarizeQf <- function(tmp,digits,na.rm,exclude,nvalid,percZero) {
  ## Set defaults for whether variables should be unfactored or not
  unfactor1 <- unfactor2 <- FALSE
  ## Exclude values from factor variables if asked to
  if (!is.null(exclude)) {
    for (i in seq_along(exclude)) {
      tmp$mf <- droplevels(tmp$mf[apply(data.frame(tmp$mf[,-tmp$Rpos])!=exclude[i],1,all),])
    }
  }
  ## Get the response variable
  nv <- tmp$mf[,tmp$Rpos]
  ## Make sure LHS is simple enough
  if (tmp$Enum>2) STOP("With a quantitative response (LHS), the RHS\n may contain only one or two factors.")
  ## Get results for quant variable by each level of interaction variable.
  if (tmp$Enum==1) { ## Only one explanatory variable
    rv <- tmp$mf[,tmp$Enames]
    intres <- tapply(nv,rv,Summarize,na.rm=na.rm,nvalid="always",percZero="always")
  } else { ## Two explanatory variables
    rv <- tmp$mf[,tmp$Enames[1]]
    cv <- tmp$mf[,tmp$Enames[2]]
    iv <- interaction(rv,cv,sep=":")
    intres <- tapply(nv,iv,Summarize,na.rm=na.rm,nvalid="always",percZero="always")
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
  if (tmp$Enum==1) { ## Only one explanatory variable
    if (tmp$Eclass!="factor") mode(lvl.lbls) <- tmp$Eclass
      else lvl.lbls <- as.factor(lvl.lbls)
    res <- data.frame(lvl.lbls,res,stringsAsFactors=FALSE)
  } else {
    lvl.lbls1 <- lvl.lbls[,1]
    lvl.lbls2 <- lvl.lbls[,2]
    if (tmp$Eclass[1]!="factor") mode(lvl.lbls1) <- tmp$Eclass[1]
      else lvl.lbls1 <- as.factor(lvl.lbls1)
    if (tmp$Eclass[2]!="factor") mode(lvl.lbls2) <- tmp$Eclass[2]
      else lvl.lbls2 <- as.factor(lvl.lbls2)
    res <- data.frame(lvl.lbls1,lvl.lbls2,res,stringsAsFactors=FALSE)
  }
  # make sure colnames of new data.frame make sense
  # should be names of explanatory variables and the colnames
  # from the tapply result
  names(res) <- c(tmp$Enames,res.names)
  # eliminate row names
  rownames(res) <- NULL
  ## Remove validn column if asked to ("never") or not interesting (all = n)
  if (nvalid=="never" | (nvalid=="different" & all(res$n==res$nvalid))) {
    res <- res[,-which(names(res)=="nvalid")]
  }
  ## Remove percZero column if asked to ("never") or not interesting (none >0)
  if (percZero=="never" | (percZero=="different" & !any(res$percZero>0,na.rm=TRUE))) {
    res <- res[,-which(names(res)=="percZero")]
  }
  ## Return the result
  res
}