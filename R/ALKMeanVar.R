#' @title Mean Values-at-age from an Age-Length Key
#' 
#' @description Uses the methods of Quinn and Deriso (1999) to compute the mean value-at-age (and the standard errors and coefficient of variation of those means) in a larger sample based on an age-length-key created from a subsample of ages through a two-stage random sampling design.  The mean values could be mean length-, weight-, or fecundity-at-age, for example.
#' 
#' @details The age-length key sent in \code{key} must be constructed with length intervals as rows and age values as columns.  XXX
#' 
#' @param key A numeric matrix that contains the age-length key.  See details.
#' @param formula A formula of the form \code{var~lencat+age} where \dQuote{var} generically represents the variable to be summarized (e.g., length, weight, fecundity), \dQuote{lencat} generically represents the variable that contains the length categories, and \dQuote{age} generically represents the variable that contain the assessed ages.
#' @param data A data.frame that minimally contains the length measurements, assessed ages, and the variable to be summarized (i.e., this should be the aged sample) as given in \code{formula}.
#' @param len.n A vector of sample sizes for each length interval in the \emph{complete sample} (i.e., all fish regardles of whether they were aged or not).
#' @param method A string that indicates which method of calculation should be used.  See details.
#' 
#' @return A data.frame with as many rows as ages present in \code{key} and the following three variables:
#' \itemize{
#'   \item age The ages.
#'   \item mean The mean value at each age.
#'   \item sd,se The SD if \code{method="BettoliMiranda"} or SE of the mean if \code{method="QuinnDeriso"} for the value at each age.
#'  } 
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @references
#' Bettoli,
#'  
#' Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages
#'
#' @seealso  See \code{\link{ageKey}} and related functions for a completely different methodology.  See \code{\link{ALKAgeDist}} for a related method of determining the proportion of fish at each age.
#' 
#' @section fishR vignette: none yet.
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
#' WR1.age <- Subset(WR1, !is.na(age))
#' # create age-length key
#' raw <- xtabs(~LCat+age,data=WR1.age)
#' ( WR1.key <- prop.table(raw, margin=1) )
#' 
#' ## use age-length key to estimate mean length-at-age of all fish
#' # Bettoli-Miranda method
#' ALKMeanVar(WR1.key,len~LCat+age,WR1.age,len.n)
#' 
#' # Quinn-Deriso method
#' ALKMeanVar(WR1.key,len~LCat+age,WR1.age,len.n,method="QuinnDeriso")
#' 
#' @export
#' 
ALKMeanVar <- function(key,formula,data,len.n,method=c("BettoliMiranda","QuinnDeriso")) {
  ## Some checks
  method <- match.arg(method)
  L <- nrow(key)
  if (length(len.n)!=L) stop("'len.n' and the 'key' have different numbers of length intervals.",call.=FALSE)
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
  ## Get overall number by length and age
  N_ij <- sweep(key,MARGIN=1,FUN="*",STATS=N_i)
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
  var_ij <- sumTable(formula,data,FUN=var)/sqrt(sumTable(formula,data,FUN=length))
  #var_ij <- sumTable(formula,data,FUN=var)/sqrt(sumTable(formula,data,FUN=length))
  options(warn=0)
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