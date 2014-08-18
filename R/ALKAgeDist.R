#' @title Proportions-at-age from an Age-Length Key
#' 
#' @description Uses the methods of Quinn and Deriso (1999) to compute the proportions-at-age (and the standard errors and coefficient of variation of those proportions) in a larger sample based on an age-length-key created from a subsample of ages through a two-stage random sampling design.
#' 
#' @details The age-length key sent in \code{key} must be constructed with length intervals as rows and age values as columns.  XXX
#' 
#' @param key A numeric matrix that contains the age-length key.  See details.
#' @param lenA.n A vector of sample sizes for each length interval in the \emph{aged sample}.
#' @param len.n A vector of sample sizes for each length interval in the \emph{complete sample} (i.e., all fish regardles of whether they were aged or not).
#' 
#' @return A data.frame with as many rows as ages present in \code{key} and the following three variables:
#' \itemize{
#'   \item prop The prortion of fish at each age.
#'   \item se The SE for the proportion of fish at each age.
#'   \item cv The CV for the proportion of fish at each age.
#'  } 
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @references Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages
#'
#' @seealso  See \code{\link{ageKey}} and related functions for a completely different methodology.  See \code{alkprop} from \pkg{fishmethods} for the exact same methodology but with different inputs.
#' 
#' @section fishR vignette: none yet.
#' 
#' @section Tests: The results from this function perfectly match the results in Table 8.4 (left) of Quinn and Deriso when using \code{data(SnapperHG2)} from \pkg{FSAdata}.  The results also perfectly match the results from using \code{alkprop} in \pkg{fishmethods}.
#'
#' @keywords manip
#'
#' @examples
#' ## None yet
#' 
#' @export
#' 
ALKAgeDist <- function(key,lenA.n,len.n) {
  ## Some checks
  num.ages <- ncol(key)
  num.lens <- nrow(key)
  if (length(lenA.n)!=num.lens) stop("'lenA.n' and the 'key' have different numbers of length intervals.",call.=FALSE)
  if (length(lenA.n)!=num.lens) stop("'lenN.n' and the 'key' have different numbers of length intervals.",call.=FALSE)
  
  ## total number of fish sampled
  n <- sum(len.n)
  ## proportion of total fish sampled by length interval
  alpha_l <- len.n/n
  ## Create a matrix of proportions-at-age with corresponding SE and CV.
  tmp <- t(apply(key,2,iALKAgeProp,alpha_l=alpha_l,lenA.n=lenA.n,n=n))
  res <- data.frame(as.numeric(rownames(tmp)),tmp)
  names(res) <- c("age","prop","se","cv")
  rownames(res) <- NULL
  ## return the result
  res
}

## ===========================================================
## An internal function that allows the use of apply()
## in ALKAgeDist() rather than using a for loop.  This computes
## the proportion at each age (theta_a) using 8.14a and the SE
## (sqrt of var.theta_a) of each proportion using 8.14b
## (note that only a single sum was used here) of Quinn and
## Deriso (1999).  The CV (SE/prop) is also returned.
## ===========================================================
iALKAgeProp <- function(theta_la,alpha_l,lenA.n,n) {
  theta_a <- sum(theta_la*alpha_l)
  var.theta_a <- sum(alpha_l^2*theta_la*(1-theta_la)/(lenA.n-1) + alpha_l*((theta_la-theta_a)^2)/n)
  c(theta_a,sqrt(var.theta_a),sqrt(var.theta_a)/theta_a)
}
