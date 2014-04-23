#'Performs a hypothesis test that a linear model parameter is equal to a specific value.
#'
#'Performs a hypothesis test that a linear model parameter is equal to a specific value.
#'seful for testing that a parameter is equal to a value other than 0.
#'
#'The \dQuote{direction} of the alternative hypothesis is identified by a
#'string in the \code{alt} argument.  
#'
#'If the \code{lm} object is from a simple linear regression with an intercept
#'then \code{term=1} will use the intercept and \code{term=2} will use the
#'slope in the hypothesis test.
#'
#'@param lmobj A \code{lm} object.
#'@param term A single numeric that indicates which term in the model to use in the
#'hypothesis test.
#'@param bo The null hypothesized parameter value.
#'@param alt A string that identifies the \dQuote{direction} of the alternative
#'hypothesis.  The strings may be \code{"less"} for a \dQuote{less than} alternative,
#'\code{"greater"} for a \dQuote{greater than} alternative, or \code{"two.sided"}
#' (DEFAULT) for a \dQuote{not equals} alternative.
#'@return A matrix that contains the term number, hypothesized value, parameter 
#'estimate, standard error of the parameter estimate, t test statistic, and 
#'corresponding p-value.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{htest.nlsBoot}}.
#'@keywords htest
#'@examples
#'data(Mirex)
#'# Simple linear regression test HA:slope!=0.1
#'lm1 <- lm(mirex~weight, data=Mirex)
#'hoCoef(lm1,2,0.1)
#'
#'@export
hoCoef <- function(lmobj,term=2,bo=0,alt=c("two.sided","less","greater")) {
  alt <- match.arg(alt)
  est <- summary(lmobj)$coefficients[term,"Estimate"]
  se <- summary(lmobj)$coefficients[term,"Std. Error"]
  t <- (est-bo)/se
  df <- lmobj$df.residual
  switch(alt,
    less=,l=,L=,lt=,LT=,Lt=p.value<-pt(t,df,lower.tail=TRUE),
    greater=,g=,G=,gt=,GT=,Gt=p.value<-pt(t,df,lower.tail=FALSE),
    two.sided=,t=,T=,ne=,NE=,Ne=p.value<-2*min(pt(t,df,lower.tail=TRUE),pt(t,df,lower.tail=FALSE))
  )
  res <- cbind(term,bo,est,se,t,p.value)
  colnames(res) <- c("term","Ho Value","Estimate","Std. Error","T","p value")
  rownames(res) <- ""
  res
}
