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
#'   \item age The ages.
#'   \item prop The prortion of fish at each age.
#'   \item se The SE for the proportion of fish at each age.
#'  } 
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @references 
#' Lai, H.-L.  1987.  Optimum allocation for estimating age composition using age-length key. Fishery Bulletin, 85:179-185.
#' 
#' Lai, H.-L.  1993.  Optimum sampling design for using the age-length key to estimate age composition of a fish population. Fishery Bulletin, 92:382-388.
#' 
#' Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages
#'
#' @seealso  See \code{\link{alkIndivAge}} and related functions for a completely different methodology.  See \code{alkprop} from \pkg{fishmethods} for the exact same methodology but with different inputs.
#' 
#' @section fishR vignette: none yet.
#' 
#' @section Tests: The results from this function perfectly match the results in Table 8.4 (left) of Quinn and Deriso when using \code{data(SnapperHG2)} from \pkg{FSAdata}.  The results also perfectly match the results from using \code{alkprop} in \pkg{fishmethods}.
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
#' WR1.age <- Subset(WR1, !is.na(age))
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
