#' @title Compute confidence intervals for PSD-X and PSD X-Y values.
#'
#' @description Compute confidence intervals for (traditional) PSD-X and (incremental) PSD X-Y values as requested by the user.
#'
#' @details Computes confidence intervals for (traditional) PSD-X and (incremental) PSD X-Y values. Two methods can be used as chosen with \code{method=}. If \code{method="binomial"} then the binomial distribution (via \code{binCI()}) is used. If \code{method="multinomial"} then the multinomial method described by Brenden et al. (2008) is used. This function is defined to compute one confidence interval so \code{method="binomial"} is the default. See examples and \code{\link{psdCalc}} for computing several simultaneous confidence intervals.
#' 
#' A table of proportions within each length category is given in \code{ptbl}. If \code{ptbl} has any values greater than 1 then it is assumed that a table of percentages was supplied and the entire table will be divided by 100 to continue. The proportions must sum to 1 (with some allowance for rounding).
#' 
#' A vector of length equal to the length of \code{ptbl} is given in \code{indvec} which contains zeros and ones to identify the linear combination of values in \code{ptbl} to use to construct the confidence intervals. For example, if \code{ptbl} has four proportions then \code{indvec=c(1,0,0,0)} would be used to construct a confidence interval for the population proportion in the first category. Alternatively, \code{indvec=c(0,0,1,1)} would be used to construct a confidence interval for the population proportion in the last two categories. This vector must not contain all zeros or all ones.
#'
#' @param indvec A numeric vector of 0s and 1s that identify the linear combination of proportions from \code{ptbl} that the user is interested in. See details.
#' @param ptbl A numeric vector or array that contains the proportion or percentage of all individuals in each length category. See details.
#' @param n A single numeric of the number of fish used to construct \code{ptbl}.
#' @param method A string that identifies the confidence interval method to use. See details.
#' @param bin.type A string that identifies the type of method to use for calculation of the confidence intervals when \R{method="binomial"}. See details of \code{\link{binCI}}.
#' @param conf.level A number that indicates the level of confidence to use for constructing confidence intervals (default is \code{0.95}).
#' @param label A single string that can be used to label the row of the output matrix.
#' @param digits A numeric that indicates the number of decimals to round the result to.
#'
#' @return A matrix with columns that contain the computed PSD-X or PSD X-Y value and the associated confidence interval. The confidence interval values were set to zero or 100 if the computed value was negative or greater than 100, respectively.
#' 
#' @section Testing: The multinomial results match the results given in Brendent et al. (2008).
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 6-Size Structure.
#'
#' @seealso See \code{\link{psdVal}}, \code{\link{psdPlot}}, \code{\link{psdAdd}}, \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{lencat}}, and \code{\link{rcumsum}} for related functionality.
#'
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR/}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Brenden, T.O., T. Wagner, and B.R. Murphy. 2008. Novel tools for analyzing proportional size distribution index data. North American Journal of Fisheries Management 28:1233-1242. [Was (is?) from http://qfc.fw.msu.edu/Publications/Publication\%20List/2008/Novel\%20Tools\%20for\%20Analyzing\%20Proportional\%20Size\%20Distribution_Brenden.pdf.]
#'
#' @keywords hplot
#'
#' @examples
#' ## similar to Brenden et al. (2008)
#' n <- 997
#' ipsd <- c(130,491,253,123)/n
#' 
#' ## single binomial
#' psdCI(c(0,0,1,1),ipsd,n=n)
#' psdCI(c(1,0,0,0),ipsd,n=n,label="PSD S-Q")
#' 
#' ## single multinomial
#' psdCI(c(0,0,1,1),ipsd,n=n,method="multinomial")
#' psdCI(c(1,0,0,0),ipsd,n=n,method="multinomial",label="PSD S-Q")
#' 
#' ## multiple multinomials (but see psdCalc())
#' lbls <- c("PSD S-Q","PSD Q-P","PSD P-M","PSD M-T","PSD","PSD-P")
#' imat <- matrix(c(1,0,0,0,
#'                  0,1,0,0,
#'                  0,0,1,0,
#'                  0,0,0,1,
#'                  0,1,1,1,
#'                  0,0,1,1),nrow=6,byrow=TRUE)
#' rownames(imat) <- lbls
#' imat
#' 
#' mcis <- t(apply(imat,MARGIN=1,FUN=psdCI,ptbl=ipsd,n=n,method="multinomial"))
#' colnames(mcis) <- c("Estimate","95% LCI","95% UCI")
#' mcis
#'
#' ## Multiple "Bonferroni-corrected" (for six comparisons) binomial method
#' bcis <- t(apply(imat,MARGIN=1,FUN=psdCI,ptbl=ipsd,n=n,conf.level=1-0.05/6))
#' colnames(bcis) <- c("Estimate","95% LCI","95% UCI")
#' bcis
#' 
#' @export
psdCI <- function(indvec,ptbl,n,method=c("binomial","multinomial"),
                  bin.type=c("wilson","exact","asymptotic"),
                  conf.level=0.95,label=NULL,digits=1) {
  ## Perform some check (and potential manipluations)
  method <- match.arg(method)
  bin.type <- match.arg(bin.type)
  ptbl <- iCheckPtbl(ptbl)
  indvec <- iCheckIndvec(indvec,ptbl)
  ## process through internal functions
  switch(method,
         multinomial= { res <- iPSDCImultinom(indvec,ptbl,n,conf.level) },
         binomial=    { res <- iPSDCIbinom(indvec,ptbl,n,conf.level,bin.type) },
         )
  ## add names to result and then return
  res <- matrix(res,nrow=1)
  colnames(res) <- c("Estimate",iCILabel(conf.level))
  if (!is.null(label)) rownames(res) <- label
  ## multiply by 100 to make a PSD
  round(res*100,digits)
}

# ==============================================================================
# INTERNAL function to check the table of proportions to make sure that it is
#   proportions and that they sum to 1. If not obviously proportions then change
#   to proportions and return.
# ==============================================================================
iCheckPtbl <- function(ptbl) {
  ## make sure table is proportions
  if (any(ptbl>1)) {
    WARN("'ptbl' not a table of proportions; attempted to convert\n to proportions to continue.")
    ptbl <- ptbl/sum(ptbl)
  }
  ## make sure table sums to one (within rounding)
  if (sum(ptbl)<0.99 | sum(ptbl)>1.01)
    STOP("'ptbl' does not sum to 1 (within rounding)")
  ptbl
}

# ==============================================================================
# INTERNAL function to check the vector of indicator values. The vector will be
#   returned as a column vector.
# ==============================================================================
iCheckIndvec <- function(indvec,ptbl) {
  ## check that table and indvec are same size
  k <- length(ptbl)
  if (length(indvec)!=k) STOP("Length of 'ptbl' and 'indvec' must be the same.")
  ## make sure that indvec is not all zeros or ones
  tmp <- sum(indvec)
  if (tmp==0) STOP("'indvec' cannot be all zeros.")
  if (tmp==k) STOP("'indvec' cannot be all ones.")
  ## convert indvec to a column matrix for matrix multiplication below
  matrix(indvec,ncol=1)
}

# ==============================================================================
# INTERNAL function to compute the binomial CI.
# ==============================================================================
iPSDCIbinom <- function(indvec,ptbl,n,conf.level,type) {
  ## get psd val asked for (drop gets rid of matrix result)
  psd <- drop(t(indvec) %*% ptbl)
  if (psd==0 | psd==1) {
    res <- c(psd,NA,NA)
  } else {
    res <- c(psd,binCI(n*psd,n,conf.level,type))
    ## replace negative numbers with 0, values >1 with 1
    res[res<0] <- 0
    res[res>1] <- 1
  }
  res
}

# ==============================================================================
# INTERNAL function to compute the multinomial CI.
# ==============================================================================
iPSDCImultinom <- function(indvec,ptbl,n,conf.level) {
  ## check if sample size is >20 (see Brenden et al. 2008), warn if not
  tmp <- drop(t(indvec) %*% (n*ptbl))
  if (tmp<20 & tmp>0) WARN("Category sample size (",tmp,
                           ") <20, CI coverage may be lower than ",
                           100*conf.level,"%.")
  ## create covariance matrix ... from hints at
  ##    https://stackoverflow.com/questions/19960605/r-multinomial-distribution-variance/
  cov <- -outer(ptbl,ptbl)
  diag(cov) <- ptbl*(1-ptbl)
  ## get psd val asked for (drop gets rid of matrix result)
  psd <- drop(t(indvec) %*% ptbl)
  if (psd==0 | psd==1) {
    res <- c(psd,NA,NA)
  } else {
    ## get SE
    se <- drop(sqrt((t(indvec) %*% cov %*% indvec)/n))
    ## get critical chi-square value
    # get number of non-zero values in ptbl for df
    df <- length(ptbl[ptbl>0])-1
    cv <- sqrt(stats::qchisq(conf.level,df))
    ## put together
    res <- c(psd,psd + c(-1,1)*cv*se)
    ## replace negative numbers with 0, values >1 with 1
    res[res<0] <- 0
    res[res>1] <- 1
  }
  res
}
