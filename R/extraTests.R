#' @title Likelihood ratio and extra sum-of-squares tests.
#' 
#' @description Likelihood ratio and extra sum-of-squares tests with multiple models nested at the same level below one commond model.
#' 
#' @details \code{anova()} and \code{lrtest()} (from \pkg{lmtest}) provide simple methods for conduction extra sum-of-squares or likelihood ratio tests when one model is nested within another model or when there are several layers of single models all nested within each other.  However, to compare several models that are nested at the same level with one common more complex model then \code{anova()} and \code{lrtest()} need to be repeated for each comparison.  This repetion can be eliminated with \code{lapply()} but then the output is voluminous.  This function is designed to remove the reptitiveness and to provide output that compact and easy to read.
#' 
#' @note This is experimental at this point.  It seems to work fine for \code{nls} models but has not been tested thoroughly or with other types of models.
#' 
#' @param sim The results of one \code{lm} or \code{nls} model, for example, that is a nested subsets of the model in \code{com=}.
#' @param \dots More model results that are nested subsets of the model in \code{com=}.
#' @param com The results of one \code{lm} or \code{nls} model, for example, that the models in \code{sim=} and \code{\dots} are a subset of.
#' @param x An object from \code{lrt()} or \code{extraSS()}.
#'
#' @return The main function returns a matrix with as many rows as model comparisons and columns of the following types:
#'  \itemize{
#'    \item \code{DfO} The degrees-of-freedom from the subset (more simple) model.
#'    \item \code{RSSO}, \code{logLikO} The residual sum-of-squares (from \code{extraSS}) or log-likelihood (from \code{lrt}) from the subset (more simple) model.
#'    \item \code{DfA} The degrees-of-freedom from the \code{com=} model.
#'    \item \code{RSSA}, \code{logLikA} The residual sum-of-squares (from \code{extraSS}) or log-likelihood (from \code{lrt}) from the \code{com=} model.
#'    \item \code{Df} The difference in degrees-of-freedom between the two models.
#'    \item \code{SS}, \code{logLik} The difference in residual sum-of-squares (from \code{extraSS}) or log-likelihood (from \code{lrt}) between the two models.
#'    \item \code{F}, \code{Chisq} The corresponding F- (from \code{extraSS}) or Chi-square (from \code{lrt}) test statistic.
#'    \item \code{Pr(>F)"}, \code{Pr(>Chisq)"} The corresponding p-value.
#'  }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords htest
#'
#' @examples
#' ## Nothing yet:
#' 
#' @rdname extraTests
#' @export
lrt <- function(sim,...,com) {
  sim <- list(sim,...)
  n.sim <- length(sim)
  res <- matrix(NA,nrow=n.sim,ncol=5+2)
  tmp <- lapply(sim,lrtest,com)
  for (i in 1:n.sim) {
    res[i,] <- c(unlist(tmp[[i]][1,c("#Df","LogLik")]),
                 unlist(tmp[[i]][2,c("#Df","LogLik")]),
                 unlist(tmp[[i]][2,c("Df","Chisq","Pr(>Chisq)")]))
  }
  tmp <- res[,2]-res[,4]
  res <- cbind(matrix(res[,1:5],nrow=n.sim),
               matrix(tmp,nrow=n.sim),
               matrix(res[,6:7],nrow=n.sim))
  colnames(res) <- c("DfO","logLikO","DfA","logLikA","Df","logLik","Chisq","Pr(>Chisq)")
  rownames(res) <- paste(1:n.sim,"vA",sep="")
  tmp <- 
    com.hdg <- paste("Model A: ",deparse(formula(com)),sep="")
  sim.hdg <- paste("Model ",1:n.sim,": ",lapply(sim,formula),sep="",collapse="\n")
  attr(res,"heading") <- paste(sim.hdg,com.hdg,sep="\n")
  class(res) <- "extraTest"
  res
}

#' @rdname extraTests
#' @export
extraSS <- function(sim,...,com) {
  sim <- list(sim,...)
  n.sim <- length(sim)
  res <- matrix(NA,nrow=n.sim,ncol=6+2)
  tmp <- lapply(sim,anova,com)
  for (i in 1:n.sim) {
    res[i,] <- c(unlist(tmp[[i]][1,c("Res.Df","Res.Sum Sq")]),
                 unlist(tmp[[i]][2,c("Res.Df","Res.Sum Sq")]),
                 unlist(tmp[[i]][2,c("Df","Sum Sq","F value","Pr(>F)")]))
  }
  colnames(res) <- c("DfO","RSSO","DfA","RSSA","Df","SS","F","Pr(>F)")
  rownames(res) <- paste(1:n.sim,"vA",sep="")
  com.hdg <- paste("Model A: ",deparse(formula(com)),sep="")
  sim.hdg <- paste("Model ",1:n.sim,": ",lapply(sim,formula),sep="",collapse="\n")
  attr(res,"heading") <- paste(sim.hdg,com.hdg,sep="\n")
  class(res) <- "extraTest"
  res
}

#' @rdname extraTests
#' @method print extraTest
#' @export
print.extraTest <- function(x,...) {
  cat(attr(x,"heading"),"\n\n")
  nms <- names(x)
  printCoefmat(x,cs.ind=c(2,4,6),tst.ind=7,zap.ind=6,has.Pvalue=TRUE,...)
}
