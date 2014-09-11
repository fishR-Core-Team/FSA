#' @title Compute confidence intervals for PSD-X and PSD X-Y values.
#'
#' @description Compute confidence intervals for PSD-X and PSD X-Y values as requested by the user.
#'
#' @details Computes confidence intervals for PSD-X and PSD X-Y values.  Three methods can be used as chosen with \code{method=}.  If \code{method="multinom"} then the multinomial method described by Brenden et al. (2008) is used.  If \code{method="Gustafson"} then the approximate binomial method of Gustafson (1988) is used.  If \code{method="binomial"} then the binomial distribution (via \R{binCI()}) is used.
#' 
#' A table of proportions within each length category is given in \code{tbl}.  If \code{tbl} has any values greater than 1 then it is assumed that a table of percentages was suplied and the entire table will divided by 100 to continue.  The proportions must sum to 1 (with some allowance for rounding).
#' 
#' A vector of length equal to the length of \code{tbl} is given in \code{indvec} which contains zeroes and ones to identify the linear combination of values in \code{tbl} to use to construct the confidence intervals.  For example, if \code{tbl} has four proportions then \code{indvec=c(1,0,0,0)} would be used to construct a confidence interval for the population proportion in the first category.  Alternatively, \code{indvec=c(0,0,1,1)} would be used to construct a confidence interval for the population proportion in the last two categories.  This vector must not contain all zeroes or all ones.
#'
#' @aliases psdCI
#'
#' @param indvec A numeric vector of 0s and 1s that identify the linear combination of proprtions from \code{tbl} that the user is interested in.  See details.
#' @param tbl A numeric vector or array that contains the proportion of all individuals in each length category.  See details.
#' @param n A single numeric of the number of fish used to construct \code{tbl}.
#' @param method A character that identifies the confidence interval method to use.  See details.
#' @param conf.level A number that indicates the level of confidence to use for constructing confidence intervals (default is \code{0.95}).
#' @param label A single character that can be used to label the row of the output matrix.
#' @param digits A numeric that indicates the number of decimals to round the result to.
#'
#' @return A matrix with columns that contain the computed PSD-X or PSD X-Y value and the associated confidence interval.  The confidence interval values were set to zero or 100 if the computed value was negative or greater than 100, respectively.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{psdVal}}, \code{\link{psdPlot}}, \code{\link{psdDataPrep}}, \code{\link{PSDlit}}, \code{\link{tictactoe}}, \code{\link{tictactoeAdd}}, \code{\link{lencat}}, and \code{\link{rcumsum}}.
#'
#' @section fishR vignette: none
#'
#' @references
#' Brenden, T.O., T. Wagner, and B.R. Murphy.  2008.  \href{http://qfc.fw.msu.edu/Publications/Publication\%20List/2008/Novel\%20Tools\%20for\%20Analyzing\%20Proportional\%20Size\%20Distribution_Brenden.pdf}{Novel tools for analyzing proportional size distribution index data.}  North American Journal of Fisheries Management 28:1233-1242.
#'
#' @keywords hplot
#'
#' @examples
#' ## from Brenden et al. (2008)
#' ipsd <- c(0.130,0.491,0.253,0.123)
#' n <- 445
#' psdCI(c(0,0,1,1),ipsd,n=n)
#' psdCI(c(1,0,0,0),ipsd,n=n,label="PSD S-Q")
#' 
#' lbls <- c("PSD-P","PSD S-Q")
#' imat <- matrix(c(0,0,1,1,
#'                  1,0,0,0),nrow=2,byrow=TRUE)
#' rownames(imat) <- lbls
#' imat
#' cis <- t(apply(imat,MARGIN=1,FUN=psdCI,tbl=ipsd,n=n))
#' colnames(cis) <- c("Estimate","95% LCI","95% UCI")
#' cis
#' 
#' @export
#' 
psdCI <- function(indvec,tbl,n,method=c("multinom","binom","Gustafson"),conf.level=0.95,label=NULL,digits=1) {
  method <- match.arg(method)
  ## make sure table is proportions
  if (any(tbl>1)) tbl <- tbl/sum(tbl)
  ## make sure table sumes to one (within rounding)
  if (sum(tbl)<0.99) stop("'tbl' does not sum to 1 (within rounding)",call.=FALSE)
  ## check that table and indvec are same size
  k <- length(tbl)
  if (length(indvec)!=k) stop("Length of 'tbl' and 'indvec' must be the same.",call.=FALSE)
  ## make sure that indvec is not all zeroes or ones
  tmp <- sum(indvec)
  if (tmp==0) stop("'indvec' cannot be all zeroes.",call.=FALSE)
  if (tmp==k) stop("'indvec' cannot be all ones.",call.=FALSE)
  ## convert indvec to a column matrix for matrix multiplication below
  indvec <- matrix(indvec,ncol=1)
  ## process through internal functions
  switch(method,
         multinom=  {res <- zPSDCImultinom(indvec,tbl,n,k,conf.level)},
         binom=     { },
         Gustafson= { }
         )
  ## add names to result and then return
  res <- matrix(res,nrow=1)
  colnames(res) <- c("Estimate",iCILabel(conf.level))
  if (!is.null(label)) rownames(res) <- label
  ## multiply by 100 to make a PSD
  round(res*100,digits)
}

zPSDCImultinom <- function(indvec,tbl,n,k,conf.level) {
  ## check if sample size is >20 (see Brenden et al. 2008), warn if not
  tmp <- drop(t(indvec) %*% (n*tbl))
  if (tmp<20 & tmp>0) warning("Category sample size (",tmp,") <20, CI coverage may be lower than ",100*conf.level,"%.",call.=FALSE)
  ## create covariance matrix ... from hints at
  ##    http://stackoverflow.com/questions/19960605/r-multinomial-distribution-variance
  cov <- -outer(tbl,tbl)
  diag(cov) <- tbl*(1-tbl)
  ## get psd val asked for (drop gets rid of matrix result)
  psd <- drop(t(indvec) %*% tbl)
  if (psd==0 | psd==1) {
    res <- c(psd,NA,NA)
  } else {
    ## get SE
    se <- drop(sqrt((t(indvec) %*% cov %*% indvec)/n))
    ## get critical chi-square value
    cv <- sqrt(qchisq(conf.level,k-1))
    ## put together
    res <- c(psd,psd + c(-1,1)*cv*se)
    ## replace negative numbers with 0, values >1 with 1
    res[res<0] <- 0
    res[res>1] <- 1
  }
  res
}
