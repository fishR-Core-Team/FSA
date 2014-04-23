#'Methods to assess length-bias in a proposed standard weight equation.
#'
#'The Willis and empirical quantiles (EmpQ) methods to assess length-bias in a
#'proposed standard weight equation.
#'
#'The main function can be used to assess length-bias in a proposed standard
#'weight equation using either the method of Willis et al. (1991) (i.e.,
#'\code{type="Willis"}) or the empricial quantiles method of Gerow et al.
#'(2004) (i.e., \code{type="EmpQ"}).  The Willis method begins by regressing
#'the relative weight computed from the candidate standard weight equation
#'(supplied in \code{object}) for each individual in a population in the
#'\code{df} data frame against length.  This is repeated for each population in
#'\code{df}.  The number of positive and negative slopes from this regression
#'that are statistically significant are counted and a chi-square test is used
#'to determine if there is a statistically equal number of each.  If there is a
#'statistically equal number of positive and negative significant slopes then
#'the standard weight equation is said not to exhibit a length bias.
#'
#'The EmpQ method is performed by (1) computing the mean actual weight per
#'\code{w}-mm length category for each population, (2) computing the given
#'quartile (default is third) of mean actual weight per length category across
#'all populations, (3) standardizing the quartile mean weights by dividing each
#'by the standard weight for the midpoint of the length categories using the
#'proposed standard weight equation, and (4) regressing the standardized
#'quartile mean weights against the length category midpoints.  The regression
#'can either be quadratic (i.e., \code{quadratic=TRUE}) as proposed by Gerow et
#'al. (2004) or n-weighted (i.e., \code{weighted=TRUE}).  In addition, length
#'categories with fewer than \code{ncutoff} are eliminated (see
#'\code{cutoff.tail} description above).  A slope of zero for the relationship
#'between standardized quartile mean weights and length category midpoints
#'indicates that no length-based biases exist with the proposed standard weight
#'equation.
#'
#'Types of quantile calculation methods are discussed in the details of \code{quantile}.
#'
#'@aliases wsValidate print.willis summary.willis anova.empq coef.empq
#'summary.empq predict.empq plot.empq fitPlot.empq
#'@param object An object of class \code{RLP} or \code{EMP} returned from
#'calling \code{rlp()} or \code{emp()} in the main function and an object of
#'class class \code{empq} or \code{willis} (saved from the \code{wsValidate})
#'in the generic functions.
#'@param df A data frame that contains the length-weight data for each population.
#'@param pops A string or numeric that indicates which column in \code{df} contains
#'the variable that identifies the different populations.
#'@param len A string or numeric that indicates which column in \code{df} contains
#'the variable with the length data.
#'@param wt A string or numeric that indicates which column in \code{df} contains
#'the variable with the weight data.
#'@param min A number that indicates the midpoint value of the smallest X-mm length category.
#'@param max A number that indicates the midpoint value of the largest X-mm length category.
#'@param w A number that indicates the widths for which to create length categories.
#'@param type A string that indicates which type of bias detection method should be used.
#'@param n.cutoff A numeric that indicates the minimum sample size in a length category
#'that should be included in the EmpQ regression.  Ignored if \code{type="Willis"}.
#'@param cutoff.tail A logical that indicates if all length categories larger than
#'the lowest length category with a sample size below \code{n.cutoff} should be
#'excluded \code{=TRUE} or just those length categories with sample sizes lower
#'than \code{n.cutoff}.  Ignored if \code{type="Willis"}.
#'@param qtype Type of quantile method to use.  See details.  Ignored if \code{use.means=TRUE}.
#'@param probs A number that indicates the probability of the quantile.  Must be
#'between 0 and 1.  Ignored if \code{use.means=TRUE}.
#'@param use.means A logical that indicates whether mean mean weight rather than a
#'quantile mean weight should be used in the EmpQ method.
#'@param quadratic A logical that indicates whether a quadratic regression should
#'be fit in the EmpQ method.  Ignored if \code{type="Willis"}.
#'@param weighted A logical that indicates whether the regression in the EmpQ
#'method should be weighted by the number of populations present in each length
#'category.  Ignored if \code{type="Willis"}.
#'@param alpha A numeric that indicates the rejection criterion to be used in the
#'Willis method.  Ignored if \code{type="EmpQ"}.
#'@param x An object saved from the \code{wsValidate} call (i.e., of class
#'\code{empq} or \code{willis}).
#'@param pch A single numeric that indicates what plotting characther codes should
#'be used for the points in plot or fitPlot.
#'@param col.pt A string used to indicate the color of the plotted points.
#'@param xlab A label for the x-axis of plot or fitPlot.
#'@param ylab A label for the y-axis of plot or fitPlot.
#'@param col.mdl A string that indicates the type of color to use for the standard
#'length-weight regression line.
#'@param lwd.mdl A numeric that indicates the width of the line to use for the
#'standard length-weight regression line.
#'@param lty.mdl A numeric that indicates the type of line to use for the standard
#'length-weight regression line.
#'@param main A label for the main title of fitPlot.
#'@param \dots Additional arguments for methods.
#'@return If \code{type="Willis"} then a list is returned with the following three items.
#'\itemize{
#'\item \code{res.ind} is a data frame that contains the results of the
#'individual regressions.
#'\item \code{res.tbl}) is the table summarizing the number of positive and
#'negative significant slopes.
#'\item \code{res.test}) contains the results for the chi-square test.
#'}
#'
#'If \code{type="EmpQ"} then a list is returned with the following five items:
#'\itemize{
#'\item \code{n.by.pop}) is a table of the number of populations represented in
#'each length category.
#'\item \code{regdata}) is a dataframe used for the EmpQ regression.
#'\item \code{quadratic}) is a logical that indicates whether the quadratic regression
#'was used.
#'\item \code{weighted}) is a logical that indicates whether a weighted regression
#'was used.
#'\item \code{lm.v} is the EmpQ regression model results.
#'}
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{rlp}}, \code{\link{emp}}, \code{\link{FroeseWs}}, 
#'\code{quantile} in \pkg{stats}
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}
#'@references Gerow, K.G., W.A. Hubert, R.C. Anderson-Sprecher.  2004.  An
#'alternative approach to detection of length-related biases in standard weight
#'equations.  North American Journal of Fisheries Management 24:903-910.
#'
#'Willis, D.W., C.S. Guy, and B.R. Murphy.  1991.  Development and evaluation
#'of the standard weight (Ws) equation for yellow perch.  North American
#'Journal of Fisheries Management, 11:374-380.
#'@keywords manip
#'@examples
#'## See examples in rlp(), emp(), and FroeseWs()
#'
#'@rdname wsValidate
#'@export wsValidate
wsValidate <- function(object,df,pops,len,wt,min,max,w=10,type=c("EmpQ","Willis"),n.cutoff=3,cutoff.tail=TRUE,qtype=8,probs=0.75,use.means=FALSE,quadratic=TRUE,weighted=FALSE,alpha=0.05) {
  ## Internal functions
  compute.Ws <- function(object,vals) {
    if (!("FroeseWs" %in% class(object))) {
      ndf <- data.frame(log10(vals))
      names(ndf) <- names(object$Ws$model)[2]
      Ws <- 10^(predict(object,ndf))
    } else Ws <- object$gm.a*vals^object$mn.b
    Ws  
  }
  
  EmpQ <- function(object,df,pops,len,wt,min,w,n.cutoff,cutoff.tail,qtype,probs,quadratic,weighted) {
    # adds lwr bnd len cat to df -- not factor for midpoint correction next
    df <- lencatOLD(df,len,startcat=min-w/2,w=w,as.fact=FALSE)
    # converts lower bound of category to a midpoint value
    df$LCat <- df$LCat+w/2
    # finds n and mean for each popn and length category
    df1 <- aggregate(as.formula(paste(wt,"~",pops,"+LCat",sep="")),data=df,FUN=function(x) cbind(length(x),mean(x)))
    # puts results into a useful data frame ... a hack
    df1 <- data.frame(as.matrix(df1))
    names(df1)[3:4] <- c("n","mn.W")
    # creates a factored version of LCat -- for summary tables
    df1$fLCat <- factor(df1$LCat)
   # find table of populations in each length category, remove those lower than ncutoff considering cutoff.tail
    p.n.tbl <- table(df1$fLCat)               # number of population means in each length category
    p.n.low <- which(p.n.tbl < n.cutoff)
    p.n.adeq <- which(p.n.tbl >= n.cutoff)
    if (cutoff.tail & length(p.n.low)>0) {
      p.n.low <- p.n.low[1]:p.n.low[length(p.n.low)]
      p.n.adeq <- p.n.adeq[1]:(p.n.low[1]-1)
    }
    names(p.n.tbl)[which(names(p.n.tbl) %in% names(p.n.tbl)[p.n.low])] <- paste(names(p.n.tbl)[p.n.low],"*",sep="")  # asterisk those not used
    df2 <- subset(df1,fLCat %in% names(p.n.tbl)[p.n.adeq])                            # make new data frame with length categories deleted
    df2 <- drop.levels(df2,reorder=FALSE)
   # find the desired quantile (or mean) of mean W in each length category
    if (use.means) Wq <- tapply(df2$mn.W,df2$fLCat,mean)
    else Wq <- tapply(df2$mn.W,df2$fLCat,quantile,probs=probs,type=qtype)
    regdf <- data.frame(midpt=as.numeric(names(Wq)),wq=Wq,n=p.n.tbl[names(Wq)])
    regdf$Ws <- compute.Ws(object,regdf$midpt)                                        # add computed Ws & stndrdzd quantile (or mean) mean weight to df
    regdf$Wr <- (regdf$wq/regdf$Ws)*100
   # Add regression of standardized quartile (or mean) mean weight on midpoint
    ifelse(weighted,lm.v <- lm(Wr~midpt,weights=n,data=regdf),lm.v <- lm(Wr~midpt,data=regdf)) 
    if (quadratic) lm.v <- update(lm.v,.~.+I(midpt^2))
   # return lots of parts
    list(n.by.pop=p.n.tbl,lm.v=lm.v,regdata=regdf,quadratic=quadratic,weighted=weighted,probs=probs)
  } ## end of internal EmpQ function
   
  Willis <- function(object,df,pops,len,wt,alpha) {
   # modify database with standard weight and relative weight
    df$Ws <- compute.Ws(object,df[,len])                                              # Add reg of standardized quartile (or mean) mean weight on midpoint
    df$Wr <- (df[,wt]/df$Ws)*100
   # loop through regressions of Wr on length by "fishery" keeping track of sign of significant relationships.  
    reg.nums <- sort(unique(df[,pops]))
    pop <- len.slope <- len.slope.p <- numeric(length(reg.nums))
    sig.slope <- logical(length(reg.nums))
    sign.slope <- character(length(reg.nums))
    for (i in 1:length(reg.nums)) {
      pop[i] <- reg.nums[i]
      df1 <- df[df[,pops]==reg.nums[i],]
      lm1 <- lm(df1$Wr~df1[,len])
      len.slope[i] <- coef(lm1)[2]
      len.slope.p[i] <- summary(lm1)$coefficients[2,"Pr(>|t|)"]
    }
    sig.slope <- len.slope.p < alpha
    sign.slope[sig.slope] <- sign(len.slope[sig.slope])
    sign.slope <- factor(sign.slope,levels=c("-1","","1"))
    res.ind <- data.frame(pop,slope=len.slope,p.value=len.slope.p,significant=sig.slope,sign=sign.slope)
   # binomial test
    res.tbl <- table(sign.slope,exclude="")
    rownames(res.tbl) <- c("Negative","Positive")
    res.test <- binom.test(res.tbl[1],sum(res.tbl))
   # return results
    list(res.ind=res.ind,res.tbl=res.tbl,res.test=res.test)
  }  ## end of internal Willis function 
  
 ## start of main function 
  fLCat <- n <- NULL  # attempting to get by bindings warning in RCMD CHECK
  type <- match.arg(type)
 # modify database by limiting to lengths within modeling end points
  df <- df[df[,len]>=min-w/2 & df[,len]<max+w/2,]
  if (type=="EmpQ") {
    res <- EmpQ(object,df,pops,len,wt,min,w,n.cutoff,cutoff.tail,qtype,probs,quadratic,weighted)
    class(res) <- c("empq")
  } else {
    res <- Willis(object,df,pops,len,wt,alpha)
    class(res) <- c("willis")
  }
  res
}

#'@rdname wsValidate
#'@method print willis
#'@S3method print willis
print.willis <- function(x,...) {
  cat("Individual Regression Results.\n")
  print(x$res.ind)
  cat("\nSummary Table of Significant Slopes.\n")
  print(x$res.tbl)
}

#'@rdname wsValidate
#'@method summary willis
#'@S3method summary willis
summary.willis <- function(object,...) {
  cat("\nSummary Table of Significant Slopes.\n")
  print(object$res.tbl)
  cat("\n")
  print(object$res.test)
}

#'@rdname wsValidate
#'@method summary empq
#'@S3method summary empq
summary.empq <- function(object,...) {
  cat("\nNumber of Populations in Each Length Category.\n")
  print(object$n.by.pop)
  cat("\n")
  summary(object$lm.v,...)
}

#'@rdname wsValidate
#'@method anova empq
#'@S3method anova empq
anova.empq <- function(object,...) {
  anova(object$lm.v,...)
}

#'@rdname wsValidate
#'@method coef empq
#'@S3method coef empq
coef.empq <- function(object,...) {
  coef(object$lm.v,...)
}

#'@rdname wsValidate
#'@method predict empq
#'@S3method predict empq
predict.empq <- function(object,...) {
  predict(object$lm.v,...)
}

#'@rdname wsValidate
#'@method plot empq
#'@S3method plot empq
plot.empq <- function(x,pch=16,col.pt="black",xlab="Midpoint Length Category",ylab=paste("Standardized",100*x$probs,"Percentile Mean Weight"),...) {
  plot(Wr~midpt,data=x$regdata,xlab=xlab,ylab=ylab,pch=pch,col=col.pt,...)
}

#'@rdname wsValidate
#'@method fitPlot empq
#'@S3method fitPlot empq
fitPlot.empq <- function(object,pch=16,col.pt="black",col.mdl="red",lwd.mdl=3,lty.mdl=1,xlab="Midpoint Length Category",ylab=paste("Standardized",100*object$probs,"Percentile Mean Weight"),main="EmpQ Method",...) {
  plot(object,pch=pch,col.pt=col.pt,xlab=xlab,ylab=ylab,...)
  if (!object$quadratic) abline(object$lm.v,col=col.mdl,lwd=lwd.mdl,lty=lty.mdl)
    else {
      x <- seq(min(object$regdata$midpt),max(object$regdata$midpt),length.out=500)
      y <- predict(object$lm.v,data.frame(midpt=x))
      lines(y~x,col=col.mdl,lwd=lwd.mdl,lty=lty.mdl)
    }
}
