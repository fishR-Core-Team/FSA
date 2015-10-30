#' @title Proportions-at-age from an Age-Length Key
#' 
#' @description Computes the proportions-at-age (with standard errors) in a larger sample based on an age-length-key created from a subsample of ages through a two-stage random sampling design.  Follows the methods in Quinn and Deriso (1999).
#' 
#' @details The age-length key in \code{key} must have length intervals as rows and ages as columns.  The row names of \code{key} (i.e., \code{rownames(key)}) must contain the mininum values of each length interval (e.g., if an interval is 100-109 then the corresponding row name must be 100).  The column names of \code{key} (i.e., \code{colnames(key)}) must contain the age values (e.g., the columns can NOT be named with \dQuote{age.1}, for example).
#' 
#' The length intervals in the rows of \code{key} must contain all of the length intervals present in the larger sample.  Thus, the length of \code{len.n} must, at least, equal the number of rows in \code{key}.  If this constraint is not met, then the function will stop with an error message.
#' 
#' The values in \code{lenA.n} are equal to what the row sums of \code{key} would have been before \code{key} was converted to a row proportions table.  Thus, the length of \code{lenA.n} must also be equal to the number of rows in \code{key}.  If this constraint is not met, then the function will stop with an error message.
#' 
#' @param key A numeric matrix that contains the age-length key.  See details.
#' @param lenA.n A numeric vector of sample sizes for each length interval in the \emph{aged sample}.
#' @param len.n A numeric vector of sample sizes for each length interval in the \emph{complete sample} (i.e., all fish regardless of whether they were aged or not).
#' 
#' @return A data.frame with as many rows as ages (columns) present in \code{key} and the following three variables:
#' \itemize{
#'   \item age The ages.
#'   \item prop The proportion of fish at each age.
#'   \item se The SE for the proportion of fish at each age.
#'  }
#' 
#' @section Testing: The results from this function perfectly match the results in Table 8.4 (left) of Quinn and Deriso (1999) using \code{\link[FSAdata]{SnapperHG2}} from \pkg{FSAdata}.  The results also perfectly match the results from using \code{\link[fishmethods]{alkprop}} in \pkg{fishmethods}.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @section IFAR Chapter: 5-Age-Length Key.
#'
#' @seealso  See \code{\link{alkIndivAge}} and related functions for a completely different methodology.  See \code{\link[fishmethods]{alkprop}} from \pkg{fishmethods} for the exact same methodology but with a different format for the inputs.
#'
#' @references Ogle, D.H.  2015.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Lai, H.-L.  1987.  Optimum allocation for estimating age composition using age-length key. Fishery Bulletin, 85:179-185.
#' 
#' Lai, H.-L.  1993.  Optimum sampling design for using the age-length key to estimate age composition of a fish population. Fishery Bulletin, 92:382-388.
#' 
#' Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages.
#'
#' @keywords manip
#'
#' @examples
#' ## Get data with length measurements and some assigned ages
#' data(WR79)
#'
#' ## Example -- Even breaks for length categories
#' WR1 <- WR79
#' # add length intervals (width=5)
#' WR1$LCat <- lencat(WR1$len,w=5)
#' # get number of fish in each length interval in the entire sample
#' len.n <- xtabs(~LCat,data=WR1)
#' # isolate aged sample and get number in each length interval
#' WR1.age <- subset(WR1, !is.na(age))
#' lenA.n <- xtabs(~LCat,data=WR1.age)
#' # create age-length key
#' raw <- xtabs(~LCat+age,data=WR1.age)
#' ( WR1.key <- prop.table(raw, margin=1) )
#' 
#' # use age-length key to estimate age distribution of all fish
#' alkAgeDist(WR1.key,lenA.n,len.n)
#' 
#' @export
alkAgeDist <- function(key,lenA.n,len.n) {
  ## Some checks
  key <- iCheckALK(key)
  L <- nrow(key)
  if (length(lenA.n)!=L) stop("'lenA.n' and the 'key' have different numbers of length intervals.",call.=FALSE)
  if (length(len.n)!=L) stop("'len.n' and the 'key' have different numbers of length intervals.",call.=FALSE)
  
  ## total number of fish sampled
  N <- sum(len.n)
  ## proportion of total fish sampled by length interval
  l_i <- len.n/N
  ## Create a matrix of proportions-at-age with corresponding SE and CV.
  tmp <- t(apply(key,2,iALKAgeProp,l_i=l_i,n_i=lenA.n,N=N))
  res <- data.frame(as.numeric(rownames(tmp)),tmp)
  names(res) <- c("age","prop","se")
  row.names(res) <- NULL
  ## return the result
  res
}

## ===========================================================
## An internal function that allows the use of apply()
## in alkAgeDist() rather than using a for loop.  This computes
## the proportion at each age (p_j) using 8.14a and the SE
## (sqrt of var.p_j) of each proportion using 8.14b
## (note that only a single sum was used here) of Quinn and
## Deriso (1999).
## ===========================================================
iALKAgeProp <- function(p_jgi,l_i,n_i,N) {
  p_ij <- l_i*p_jgi
  p_j <- sum(p_ij,na.rm=TRUE)
  var_p_ij <- ((l_i^2)*p_jgi*(1-p_jgi))/(n_i-1) + (l_i*((p_jgi-p_j)^2))/N
  var.p_j <- sum(var_p_ij,na.rm=TRUE)
  c(p_j,sqrt(var.p_j))
}




#' @title Mean Values-at-age from an Age-Length Key
#' 
#' @description Computes the mean value-at-age in a larger sample based on an age-length-key created from a subsample of ages through a two-stage random sampling design.  The mean values could be mean length-, weight-, or fecundity-at-age, for example.  The methods of Bettoli and Miranda (2001) or Quinn and Deriso (1999) are used.  A standard deviation is computed for the Bettoli and Miranda (2001) method and standard error for the Quinn and Deriso (1999) method.  See the testing section notes.
#' 
#' @details The age-length key \code{key} must have length intervals as rows and ages as columns.  The row names of \code{key} (i.e., \code{rownames(key)}) must contain the mininum values of each length interval (e.g., if an interval is 100-109, then the corresponding row name must be 100).  The column names of \code{key} (i.e., \code{colnames(key)}) must contain the age values (e.g., the columns can NOT be named with \dQuote{age.1}, for example).
#' 
#' The length intervals in the rows of \code{key} must contain all of the length intervals present in the larger sample.  Thus, the length of \code{len.n} must, at least, equal the number of rows in \code{key}.  If this constraint is not met, then the function will stop with an error message.
#' 
#' Note that the function will stop with an error if the formula in \code{formula} does not meet the specific criteria outlined in the parameter list above.
#' 
#' @param key A numeric matrix that contains the age-length key.  See details.
#' @param formula A formula of the form \code{var~lencat+age} where \code{var} generically represents the variable to be summarized (e.g., length, weight, fecundity), \code{lencat} generically represents the variable that contains the length intervals, and \code{age} generically represents the variable that contains the assigned ages.
#' @param data A data.frame that minimally contains the length intervals, assessed ages, and the variable to be summarized (i.e., this should be the aged sample) as given in \code{formula}.
#' @param len.n A vector of sample sizes for each length interval in the \emph{complete sample} (i.e., all fish regardles of whether they were aged or not).
#' @param method A string that indicates which method of calculation should be used.  See details.
#' 
#' @return A data.frame with as many rows as ages (columns) present in \code{key} and the following three variables:
#' \itemize{
#'   \item age The ages.
#'   \item mean The mean value at each age.
#'   \item sd,se The SD if \code{method="BettoliMiranda"} or SE of the mean if \code{method="QuinnDeriso"} for the value at each age.
#'  }
#'
#' @section Testing: The results of these functions have not yet been rigorously tested.  The Bettoli and Miranda (2001) results appear, at least, approximately correct when compared to the results from \code{\link{alkIndivAge}}.  The Quinn and Deriso (1999) results appear at least approximately correct for the mean values, but do not appear to be correct for the SE values.  Thus, a note is returned with the Quinn and Deriso (1999) results that the SE should not be trusted.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#' 
#' @section IFAR Chapter: 5-Age-Length Key.
#'
#' @seealso  See \code{\link{alkIndivAge}} and related functions for a completely different methodology.  See \code{\link{alkAgeDist}} for a related method of determining the proportion of fish at each age.  See the \pkg{ALKr} package.
#'
#' @references Ogle, D.H.  2015.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Bettoli, P. W. and Miranda, L. E.  2001. A cautionary note about estimating mean length at age with subsampled data. North American Journal of Fisheries Management, 21:425-428.
#'  
#' Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages
#'
#' @keywords manip
#'
#' @examples
#' ## Get data with length measurements and some assigned ages
#' data(WR79)
#'
#' ## Example -- Even breaks for length categories
#' WR1 <- WR79
#' # add length intervals (width=5)
#' WR1$LCat <- lencat(WR1$len,w=5)
#' # get number of fish in each length interval in the entire sample
#' len.n <- xtabs(~LCat,data=WR1)
#' # isolate aged sample
#' WR1.age <- subset(WR1, !is.na(age))
#' # create age-length key
#' raw <- xtabs(~LCat+age,data=WR1.age)
#' ( WR1.key <- prop.table(raw, margin=1) )
#' 
#' ## use age-length key to estimate mean length-at-age of all fish
#' # Bettoli-Miranda method
#' alkMeanVar(WR1.key,len~LCat+age,WR1.age,len.n)
#' 
#' # Quinn-Deriso method
#' alkMeanVar(WR1.key,len~LCat+age,WR1.age,len.n,method="QuinnDeriso")
#' 
#' @export
alkMeanVar <- function(key,formula,data,len.n,method=c("BettoliMiranda","QuinnDeriso")) {
  ## Some checks
  method <- match.arg(method)
  key <- iCheckALK(key)
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=2)
  if (!tmp$metExpNumR) stop("'formula' must have a LHS with only one variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop ("LHS of 'formula' must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'formula' must have two and only two variables on the RHS.",call.=FALSE)
  if (length(len.n)!=nrow(key)) stop("'len.n' and the 'key' have different numbers of length intervals.",call.=FALSE)
  ## Main calculations (in internal functions)
  switch(method,
         BettoliMiranda= { res=iALKMean.BM(key,formula,data,N_i=len.n) },
         QuinnDeriso=    { res=iALKMean.QD(key,formula,data,N_i=len.n) }
  )
  res
}


##############################################################
## Internal function for Bettoli-Miranda method 
##   equations 3 and 4b
##############################################################
iALKMean.BM <- function(key,formula,data,N_i) {
  ## Compute means by length and age (suppress warnings)
  options(warn=-1)
  mn_ij <- sumTable(formula,data,FUN=mean)
  options(warn=0)
  ## See if key has a row that sums to zero, remove that row from
  ##   the key, N_i, and mn_ij
  key.goodrows <- which(rowSums(key,na.rm=TRUE)!=0)
  if (length(key.goodrows)>0) {
    key <- key[key.goodrows,]
    N_i <- N_i[key.goodrows]
    mn_ij <- mn_ij[key.goodrows,]
  }
  ## Get overall number by length and age (can set check.margin=FALSE
  ##   because already checked the dimensions ... see ?sweep)
  N_ij <- sweep(key,MARGIN=1,FUN="*",STATS=N_i,check.margin=FALSE)
  ## Get overall number by age
  N_j <- colSums(N_ij)
  ## Number of ages prsent
  A <- ncol(key)
  ## Prepare result vectors
  mn_j <- var_j <- numeric(A)
  ## Loop through the ages
  for (j in 1:A) {
    ## These are the formulas in Bettoli and Miranda (2001)
    mn_j[j] <- sum(N_ij[,j]*mn_ij[,j],na.rm=TRUE)/N_j[j] 
    var_j[j] <- sum(N_ij[,j]*((mn_ij[,j]-mn_j[j])^2),na.rm=TRUE)/(N_j[j]-1) 
    ## These are the equivalent formulas in IFSWR (would need to
    ##   define N <- sum(N_i) and p_ij <- N_ij/N before loop.)  
    #mn_j[j] <- sum(p_ij[,j]*mn_ij[,j],na.rm=TRUE)/N_j[j]*N 
    #var_j[j] <- sum(p_ij[,j]*((mn_ij[,j]-mn_j[j])^2),na.rm=TRUE)/(N_j[j]-1)*N
  }
  res <- data.frame(age=as.numeric(colnames(key)),mean=mn_j,sd=sqrt(var_j))
  rownames(res) <- NULL
  ## return the result
  res  
}

##############################################################
## Internal function for Quinn-Deriso method 
##   equations 8.15a and 8.15b
##############################################################
iALKMean.QD <- function(key,formula,data,N_i) {
  ## Compute ns, means, and variances by length and age (suppress warnings)
  options(warn=-1)
  n_ij <- sumTable(formula,data,FUN=length)
  mn_ij <- sumTable(formula,data,FUN=mean)
  # Not sure from Q&D if this should be divided by sqrt(n) or not
  var_ij <- sumTable(formula,data,FUN=stats::var)/sqrt(sumTable(formula,data,FUN=length))
  #var_ij <- sumTable(formula,data,FUN=stats::var)/sqrt(sumTable(formula,data,FUN=length))
  options(warn=0)
  ## See if key has a row that sums to zero, remove that row from
  ##   the key, N_i, n_ij, mn_ij, and var_ij
  key.goodrows <- which(rowSums(key,na.rm=TRUE)!=0)
  if (length(key.goodrows)>0) {
    key <- key[key.goodrows,]
    N_i <- N_i[key.goodrows]
    n_ij <- n_ij[key.goodrows,]
    mn_ij <- mn_ij[key.goodrows,]
    var_ij <- var_ij[key.goodrows,]
  }
  ## Get the number in aged sample in each length
  n_i <- colSums(n_ij,na.rm=TRUE)
  ## total number of fish sampled
  N <- sum(N_i)
  ## proportion of total fish sampled by length interval
  l_i <- N_i/N
  ## Number of ages prsent
  A <- ncol(key)
  ## Prepare result vectors
  mn_j <- var_j <- numeric(A)
  ## Loop through ages
  for (j in 1:A) {
    # top of page 305 from Quinn and Deriso (1999)
    p_jgi <- key[,j]
    # equation 8.14a from Quinn and Deriso (1999) ... correct from ALKAgeDist
    p_ij <- l_i*p_jgi
    p_j <- sum(p_ij)
    # RHS equivalency of 8.14b and 8.14c from Quinn and Deriso (1999) ... correct from ALKAgeDist
    var_p_ij <- ((l_i^2)*p_jgi*(1-p_jgi))/(n_i[j]-1) + (l_i*((p_jgi-p_j)^2))/N
    # equation 8.15a from Quinn and Deriso (1999)
    mn_j[j] <- sum(p_ij*mn_ij[,j],na.rm=TRUE)/p_j
    # 8.15b from Quinn and Deriso (1999)
    var_j[j] <- sum((p_ij^2)*var_ij[,j] + ((mn_ij[,j]-mn_j[j])^2)*var_p_ij,na.rm=TRUE)/(p_j^2)
  }
  ## Put it all together and return the result
  res <- data.frame(age=as.numeric(colnames(key)),mean=mn_j,se=sqrt(var_j))
  rownames(res) <- NULL
  message("The 'se' values should not be trusted!")
  res
}