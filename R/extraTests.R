#' @name extraTests
#' 
#' @title Likelihood ratio and extra sum-of-squares tests.
#' 
#' @description Likelihood ratio and extra sum-of-squares tests with multiple \code{lm} or \code{nls} models nested within one common model.  This function is most useful when the nested functions are all at the same level; otherise use \code{anova()} or \code{lrtest()} which are more flexible.
#' 
#' @details \code{\link{anova}} and \code{\link[lmtest]{lrtest}} (from \pkg{lmtest}) provide simple methods for conducting extra sum-of-squares or likelihood ratio tests when one model is nested within another model or when there are several layers of simple models all sequentially nested within each other.  However, to compare several models that are nested at the same level with one common more complex model, then \code{anova()} and \code{lrtest()} must be repeated for each comparison.  This repetition can be eliminated with \code{lapply()} but then the output is voluminous.  This function is designed to remove the repetitiveness and to provide output that is compact and easy to read.
#' 
#' @note This function is experimental at this point.  It seems to work fine for \code{lm} and \code{nls} models.  An error will be thrown by \code{extraSS} for other model classes, but \code{lrt} will not (but it has not been thoroughly tests for other models).
#' 
#' @param sim The results of one \code{lm} or \code{nls} model, for example, that is a nested subset of the model in \code{com=}.
#' @param \dots More model results that are nested subsets of the model in \code{com=}.
#' @param com The results of one \code{lm} or \code{nls} model, for example, that the models in \code{sim=} and \code{\dots} are a subset of.
#' @param x An object from \code{lrt()} or \code{extraSS()}.
#'
#' @return The main function returns a matrix with as many rows as model comparisons and columns of the following types:
#'  \itemize{
#'    \item \code{DfO} The error degrees-of-freedom from the subset (more simple) model.
#'    \item \code{RSSO}, \code{logLikO} The residual sum-of-squares (from \code{extraSS}) or log-likelihood (from \code{lrt}) from the subset (more simple) model.
#'    \item \code{DfA} The error degrees-of-freedom from the \code{com=} model.
#'    \item \code{RSSA}, \code{logLikA} The residual sum-of-squares (from \code{extraSS}) or log-likelihood (from \code{lrt}) from the \code{com=} model.
#'    \item \code{Df} The difference in error degrees-of-freedom between the two models.
#'    \item \code{SS}, \code{logLik} The difference in residual sum-of-squares (from \code{extraSS}) or log-likelihood (from \code{lrt}) between the two models.
#'    \item \code{F}, \code{Chisq} The corresponding F- (from \code{extraSS}) or Chi-square (from \code{lrt}) test statistic.
#'    \item \code{Pr(>F)}, \code{Pr(>Chisq)} The corresponding p-value.
#'  }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords htest
#'
#' @examples
#' ## Example data
#' df <- data.frame(x=c(1,2,3,4,5,6,7,8,9,10),
#'                  y=c(4,6,5,7,9,8,7,12,16,22),
#'                  z=as.factor(rep(c("A","B"),each=5)),
#'                  w=as.factor(rep(c("A","B"),times=5)))
#' df$x2 <- df$x^2
#' 
#' ## Linear (lm()) models
#' #  ... regression
#' fit.0 <- lm(y~1,data=df)
#' fit.1 <- lm(y~x,data=df)
#' fit.2 <- lm(y~x2+x,data=df)
#' extraSS(fit.0,fit.1, com=fit.2)
#' lrt(fit.0,fit.1, com=fit.2)
#' 
#' #  ... dummy variable regression
#' fit.2b <- lm(y~x*z,data=df)
#' extraSS(fit.0,fit.1, com=fit.2b)
#' lrt(fit.0,fit.1, com=fit.2b)
#' 
#' #  ... ANOVAs
#' fit.1 <- lm(y~w,data=df)
#' fit.2 <- lm(y~w*z,data=df)
#' extraSS(fit.0,fit.1, com=fit.2)
#' lrt(fit.0,fit.1, com=fit.2)
#' 
#' 
#' ## Non-linear (nls()) models
#' fit.0 = nls(y~c,data=df,start=list(c=10))
#' fit.1 = nls(y~a*x+c,data=df,start=list(a=1,c=1))
#' fit.2 = nls(y~b*x2+a*x+c,data=df,start=list(a=-1,b=0.3,c=10))
#' extraSS(fit.0,fit.1, com=fit.2)
#' lrt(fit.0,fit.1, com=fit.2)
#' 
#' ## General least-squares (gls()) models
#' \dontrun{
#'   require(nlme)
#'   fit.0 <- gls(y~1,data=df,method="ML")
#'   fit.1 <- gls(y~x,data=df,method="ML")
#'   fit.2 <- gls(y~x2+x,data=df,method="ML")
#'   lrt(fit.0,fit.1, com=fit.2)
#'   ## will return an error ... does not work with gls() models
#'   # extraSS(fit.0,fit.1, com=fit.2)
#' }
#' 
NULL

#' @rdname extraTests
#' @export
lrt <- function(sim,...,com) {
  ## Check if models are of the same class (if so, get class)
  mdl.class <- iSameModelClass(list(sim,...,com))
  ## Make a list of the simple models and determine how many
  sim <- list(sim,...)
  n.sim <- length(sim)
  ## run lrtest for each sim and com pair
  tmp <- lapply(sim,lmtest::lrtest.default,com)
  ## prepare a matrix to received the anova results  
  res <- matrix(NA,nrow=n.sim,ncol=5+2)
  ## extract results from all lrtests and put in results matrix
  for (i in 1:n.sim) {
    res[i,] <- c(unlist(tmp[[i]][1,c("#Df","LogLik")]),
                 unlist(tmp[[i]][2,c("#Df","LogLik")]),
                 unlist(tmp[[i]][2,c("Df","Chisq","Pr(>Chisq)")]))
  }
  ## Df from lrtest are not error Df.  Thus, subtract each from n
  # get n from number of residuals in a model
  n <- length(residuals(com))
  res[,1] <- n-res[,1]+1
  res[,3] <- n-res[,3]+1
  ## Compute difference in log-likelihoods and shoehorn into the results matrix
  tmp <- res[,2]-res[,4]
  res <- cbind(matrix(res[,1:5],nrow=n.sim),
               matrix(tmp,nrow=n.sim),
               matrix(res[,6:7],nrow=n.sim))
  ## give better names to the columns and rows of the results matrix  
  colnames(res) <- c("DfO","logLikO","DfA","logLikA","Df","logLik","Chisq","Pr(>Chisq)")
  rownames(res) <- paste(1:n.sim,"vA",sep="")
  ## provide a heading and a class to use in the print method
  com.hdg <- paste("Model A: ",deparse(formula(com)),sep="")
  sim.hdg <- paste("Model ",1:n.sim,": ",lapply(sim,formula),sep="",collapse="\n")
  attr(res,"heading") <- paste(sim.hdg,com.hdg,sep="\n")
  ## provide a heading and a class to use in the print method
  class(res) <- "extraTest"
  ## return the result
  res
}

#' @rdname extraTests
#' @export
extraSS <- function(sim,...,com) {
  ## Check if models are of the same class (if so, get class)
  mdl.class <- iSameModelClass(list(sim,...,com))
  ## Send error if models are not lm() or nls()
  if (!mdl.class %in% c("lm","nls")) stop("'extraSS' only works with 'lm' or 'nls' models.",call.=FALSE)
  ## Make a list of the simple models and determine how many
  sim <- list(sim,...)
  n.sim <- length(sim)
  ## run anova for each sim and com pair
  tmp <- lapply(sim,anova,com)
  ## prepare a matrix to received the anova results
  res <- matrix(NA,nrow=n.sim,ncol=6+2)
  ## extract results from all anovas and put in results matrix
  # anova results are labeled differently depending on model type
  switch(mdl.class,
         lm={ lbls1 <- c("Res.Df","RSS")
              lbls2 <- c("Df","Sum of Sq","F","Pr(>F)") },
         nls={ lbls1 <- c("Res.Df","Res.Sum Sq")
               lbls2 <- c("Df","Sum Sq","F value","Pr(>F)") })
  # now extract the results 
  for (i in 1:n.sim) {
    res[i,] <- c(unlist(tmp[[i]][1,lbls1]),
                 unlist(tmp[[i]][2,lbls1]),
                 unlist(tmp[[i]][2,lbls2]))
  }
  ## give better names to the columns and rows of the results matrix
  colnames(res) <- c("DfO","RSSO","DfA","RSSA","Df","SS","F","Pr(>F)")
  rownames(res) <- paste(1:n.sim,"vA",sep="")
  com.hdg <- paste("Model A: ",deparse(formula(com)),sep="")
  sim.hdg <- paste("Model ",1:n.sim,": ",lapply(sim,formula),sep="",collapse="\n")
  ## provide a heading and a class to use in the print method
  attr(res,"heading") <- paste(sim.hdg,com.hdg,sep="\n")
  class(res) <- "extraTest"
  ## return the result
  res
}

#' @rdname extraTests
#' @export
print.extraTest <- function(x,...) {
  cat(attr(x,"heading"),"\n\n")
  nms <- names(x)
  printCoefmat(x,cs.ind=c(2,4,6),tst.ind=7,zap.ind=6,has.Pvalue=TRUE,...)
}

# ============================================================
# Internal fuction -- determine if all models are of the same
#   class (or type) ... i.e., all lm, nls, etc.  If not then
#   stop with an error.  If so, then return the class type.
# ============================================================
iSameModelClass <- function(mdllist) {
  # an internal function that will collapse multiple classes
  # into one string.  This allows this method to identify if
  # models have the same class OR classes.
  classp <- function(x) paste(class(x),collapse="")
  # end internal function, start of main function
  tmp <- unique(unlist(lapply(mdllist,classp)))
  if (length(tmp)>1) stop("All supplied models are not of the same class.",call.=FALSE)
  tmp
}