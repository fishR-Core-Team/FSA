#' @title Mean Values-at-age from an Age-Length Key
#' 
#' @description Uses the methods of Quinn and Deriso (1999) to compute the mean value-at-age (and the standard errors and coefficient of variation of those means) in a larger sample based on an age-length-key created from a subsample of ages through a two-stage random sampling design.  The mean values could be mean length-, weight-, or fecundity-at-age, for example.
#' 
#' @details The age-length key sent in \code{key} must be constructed with length intervals as rows and age values as columns.  XXX
#' 
#' @param formula A formula of the form \code{var~length+age} where \dQuote{var} generically represents the variable to be summarized (e.g., length, weight, fecundity), \dQuote{length} generically represents the variable that contains the known length measurements, and \dQuote{age} generically represents the variable that contain the assessed ages.
#' @param data A data.frame that minimally contains the length measurements, assessed ages, and the variable to be summarized (i.e., this should be the aged sample) as given in \code{formula}.
#' @param key A numeric matrix that contains the age-length key.  See details.
#' @param lenA.n A vector of sample sizes for each length interval in the \emph{aged sample}.
#' @param len.n A vector of sample sizes for each length interval in the \emph{complete sample} (i.e., all fish regardles of whether they were aged or not).
#' 
#' @return A data.frame with as many rows as ages present in \code{key} and the following three variables:
#' \itemize{
#'   \item mean The mean value at each age.
#'   \item se The SE for the mean value at each age.
#'   \item cv The CV for the mean value at each age.
#'  } 
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @references Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages
#'
#' @seealso  See \code{\link{ageKey}} and related functions for a completely different methodology.  See \code{\link{ALKAgeDist}} for a related method of determining the proportion of fish at each age.
#' 
#' @section fishR vignette: none yet.
#'
#' @keywords manip
#'
#' @examples
#' ## None yet
#' 
#' @export
#' 
ALKMeanVar <- function(formula,data,key,lenA.n,len.n) {
  ## end internal function, start of main function
  ## Some checks
  num.ages <- ncol(key)
  num.lens <- nrow(key)
  if (length(lenA.n)!=num.lens) stop("'lenA.n' and the 'key' have different numbers of length intervals.",call.=FALSE)
  if (length(lenA.n)!=num.lens) stop("'lenN.n' and the 'key' have different numbers of length intervals.",call.=FALSE)
  
  ## Mean by length and age
  mns <- sumTable(formula,data,FUN=mean)
  ## Variance by length and age
  vars <- sumTable(formula,data,FUN=var)
  
  ## total number of fish sampled
  n <- sum(len.n)
  ## proportion of total fish sampled by length interval
  alpha_l <- len.n/n
  W_a <- var_W_a <- numeric(num.ages)
  for (i in 1:num.ages) {
    # top of page 305 from Quinn and Deriso (1999)
    theta_la <- key[,i]
    # extract "age" columns from the mean and variance matrices
    mnW_la <- mns[,i]
    varW_la <- vars[,i]
    # equation 8.14a from Quinn and Deriso (1999)
    r_la <- alpha_l*theta_la
    theta_a <- sum(r_la)
    # equation 8.15a from Quinn and Deriso (1999)
    W_a[i] <- sum(r_la*mnW_la,na.rm=TRUE)/theta_a
    # RHS equivalency of 8.14b and 8.14c from Quinn and Deriso (1999)
    var_r_la <- ((alpha_l^2)*theta_la*(1-theta_la))/(lenA.n[i]-1) + (alpha_l*((theta_la-theta_a)^2))/n
    # 8.15b from Quinn and Deriso (1999)
    var_W_a[i] <- sum((r_la^2)*varW_la + ((varW_la-W_a[i])^2)*var_r_la,na.rm=TRUE)/(theta_a^2)
  }
  res <- cbind(mean=W_a,SE=sqrt(var_W_a))
  res
}