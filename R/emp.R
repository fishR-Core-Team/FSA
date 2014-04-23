#'Computes the standard weight equation using the emperical-percentile method.
#'
#'Computes the standard weight equation using the empirical-percentile method
#'when given populations of length-weight data.
#'
#'The main function follows the steps of the empirical percentile method
#'detailed in Gerow et al. (2005).  In general, the mean log10 weight for each
#'population within all length categories is computed, length categories with
#'fewer than \code{ncutoff} are eliminated (see \code{cutoff.tail} description
#'above), the third quartile of mean log10 weights for the remaining categories
#'are found, and a n-weighted regression (quadratic regression if
#'\code{quadratic=TRUE}) is then fit to the third quartile of mean log10
#'weights and the length category midpoint value.
#'
#'Gerow et al. (2005) suggest using a quantile definition that is basically the
#'same as \code{qtype=9}.  Types of quantile calculation methods are discussed
#'in the details of of \code{quantile}.
#'
#'The \code{plot}, \code{coef}, and \code{summary} methods are used to
#'construct a plot (see below), extract the coefficients of the standard weight
#'equation, and find summary results of the \code{lm} object returned by the
#'main function.  The \code{plot} method plots the mean log10 weights versus
#'length category midpoint for each population represented in the data frame
#'with the resultant standard weight equation superimposed in black.
#'
#'If the \code{col.pop} argument is set equal to one of these palettes --
#'\dQuote{rich}, \dQuote{cm}, \dQuote{default}, \dQuote{grey}, \dQuote{gray},
#'\dQuote{heat}, \dQuote{jet}, \dQuote{rainbow}, \dQuote{topo}, or
#'\dQuote{terrain} -- and the \code{order.pop=TRUE} then the populations
#'plotted should form a general color gradient from smallest to largest weight
#'in the initial length category.  This will make it easier to identify
#'populations that \dQuote{cross over} other populations.
#'
#'@aliases emp coef.emp summary.emp predict.emp anova.emplm plot.emplm
#'fitPlot.emplm residPlot.emplm fitPlot.emprq
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
#'@param n.cutoff A numeric that indicates the minimum sample size in a length
#'category that should be included in the regression.
#'@param cutoff.tail A logical that indicates if all length categories larger than
#'the lowest length category with a sample size below \code{n.cutoff} should be
#'excluded \code{=TRUE} or just those length categories with sample sizes lower
#'than \code{n.cutoff}.
#'@param qtype Type of quantile method to use.  See details.
#'@param probs A number that indicates the probability of the quantile.  Must be
#'between 0 and 1.
#'@param method A string that indicates whether a linear regression (\code{lm}
#'or quantile regression (\code{rq}) should be used to construct the standard
#'weight equation.  See details.
#'@param quadratic A logical that indicates whether a quadratic regression should
#'be fit (\code{=TRUE} or not.
#'@param x,object An object saved from the \code{emp} call (i.e., of class \code{emp}).
#'@param pch A single numeric that indicates what plotting characther codes should
#'be used for the points in plot or fitPlot.
#'@param col.pop A string that indicates the type of color or palette to use for
#'the population of length-weight regression lines.  See details.
#'@param col.Ws A string that indicates the type of color to use for the standard
#'length-weight regression line.
#'@param lwd.Ws A numeric that indicates the width of the line to use for the
#'standard length-weight regression line.
#'@param lty.Ws A numeric that indicates the type of line to use for the standard
#'length-weight regression line.
#'@param jitterX A logical that indicates whether the x values in plot should be jittered.
#'@param jitter.factor A numeric that indicates the relative magnitude of the
#'jittering in x (sent to \code{factor} argument in \code{jitter()}.
#'@param col.pt A string used to indicate the color of the plotted points.
#'@param xlab A label for the x-axis of fitPlot.
#'@param ylab A label for the y-axis of fitPlot.
#'@param main A label for the main title of fitPlot.
#'@param \dots Additional arguments for methods.
#'@return A list is returned with the following seven items:
#'\itemize{
#'\item \code{pop.by.len} is a table of the number of populations represented
#'in each length category.
#'\item \code{ind.by.len} is a table of the number of individual fish in each
#'length category.
#'\item \code{ind.by.len.pop} is a table of the number of individual fish in each
#'length category for each population.
#'\item \code{mnlwt.by.len.pop} is a table of the mean log10 weight for each
#'length category for each population.
#'\item \code{regdata} is a dataframe used for the Ws regression (i.e., third
#'quartiles of mean log10 weight and length category midpoints).
#'\item \code{quadratic} is a logical that indicates whether the quadratic
#'regression was used.
#'\item \code{Ws} is the Ws regression model results.
#'}
#'@seealso \code{\link{rlp}}, \code{\link{FroeseWs}}, \code{\link{wsValidate}},
#'\code{quantile} in \pkg{stats}
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/RelativeWeight.pdf}
#'@references Gerow, K.G., R.C. Anderson-Sprecher, and W.A. Hubert.  2005.  A
#'new method to compute standard-weight equations that reduces length-related
#'bias.  North American Journal of Fisheries Management 25:1288-1300.
#'@keywords manip hplot
#'@examples
#'## Walleye Ws equation for comparison to Gerow's Excel Tool
#'# Gerow's results were -- -4.624269, 2.477718, and 0.1461490 for the intercept, linear term,
#'#    and quadratic term for 75th percentile Ws equation
#'# Gerow's results were -- -4.950281, 2.698470, and 0.1052352 for the intercept, linear term,
#'#    and quadratic term for 50th percentile Ws equation
#'data(WalleyeGerowLW)
#'# compare to Ws75 results
#'wae1 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE)
#'coef(wae1)
#'# compare to Ws75 results -- uses same quantile type?
#'wae2 <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,qtype=9,cutoff.tail=FALSE)
#'coef(wae2)
#'# compare to Ws50 results; note use of all length categories
#'wae1a <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,n.cutoff=1,probs=0.5)
#'coef(wae1a)
#'# compare to Ws50 results -- uses same quantile type?
#'wae2a <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,qtype=9,n.cutoff=1,probs=0.5)
#'coef(wae2a)
#'
#'# It appears that a difference from Gerow's work is that the quantiles computed
#'#   here are different than his quantiles.  This can be seen by comparing the
#'#   regdata results with the results in his 'summarized' worksheet.  From
#'#   Gerow et al. (2005) it appears that he used 'qtype=9'; however, 'qtype=5'
#'#   provides the closest values to his Excel worksheet.
#'wae1$regdata
#'
#'## use quantile regression method
#'wae1rq <- emp(WalleyeGerowLW,"popn","len","wt",min=155,max=955,cutoff.tail=FALSE,method="rq") 
#'coef(wae1rq)
#'
#'@rdname emp
#'@export emp
emp <- function(df,pops,len,wt,min,max,w=10,n.cutoff=3,cutoff.tail=TRUE,qtype=8,
                probs=0.75,method=c("lm","rq"),quadratic=TRUE) {
   fLCat <- n <- NULL  # attempting to get by bindings warning in RCMD CHECK
   method <- match.arg(method)
  # create data frame with mean logW, midpoints, and n to be used for EmP method
   df$logwt <- log10(df[,wt])
   # adds lwr bnd len cat to df -- not factor for midpoint correction next
   df <- lencatOLD(df,len,startcat=min-w/2,w=w,as.fact=FALSE)
   # converts lower bound of category to a midpoint value
   df$LCat <- df$LCat+w/2
   # finds n and mean for each popn and length category
   df1 <- aggregate(as.formula(paste("logwt~",pops,"+LCat",sep="")),data=df,FUN=function(x) cbind(length(x),mean(x)))
   # puts results into a useful data frame ... a hack
   df1 <- data.frame(as.matrix(df1))
   names(df1)[3:4] <- c("n","mn.logW")
   # creates a factored version of LCat -- for summary tables
   df1$fLCat <- factor(df1$LCat)
   # add log midpt for quantile regression method 
   df1$logmidpt <- log10(df1$LCat) 
  # find table of populations in each length category, remove those lower than ncutoff considering cutoff.tail
   p.n.tbl <- table(df1$fLCat)                      # number of population means in each length category
   p.n.low <- which(p.n.tbl < n.cutoff)             # which length categories had n below or equal to n.cutoff
   p.n.adeq <- which(p.n.tbl >= n.cutoff)           # which length categories had n above n.cutoff
   if (cutoff.tail & length(p.n.low)>0) {           # if cutoff entire tail then find the tail
     p.n.low <- p.n.low[1]:p.n.low[length(p.n.low)]
     p.n.adeq <- p.n.adeq[1]:(p.n.low[1]-1)
   }
   names(p.n.tbl)[which(names(p.n.tbl) %in% names(p.n.tbl)[p.n.low])] <- paste(names(p.n.tbl)[p.n.low],"*",sep="")  # asterisk those not used
   # make new data frame with length categories deleted
   df2 <- subset(df1,fLCat %in% names(p.n.tbl)[p.n.adeq])
   df2 <- drop.levels(df2,reorder=FALSE)

  # find the given quantile of mean logW in each length category
   logWq <- tapply(df2$mn.logW,df2$fLCat,quantile,probs=probs,type=qtype)
   regdf <- data.frame(midpt=as.numeric(names(logWq)),Wq=10^(logWq),logmidpt=log10(as.numeric(names(logWq))),logwq=logWq,n=p.n.tbl[names(logWq)])

  # fit the regression to define the Ws equation
   if (method=="lm") Ws <- lm(logwq~logmidpt,data=regdf,weights=n)
     else Ws <- rq(mn.logW~logmidpt,tau=probs,data=df1)
   if (quadratic) Ws <- update(Ws,.~.+I(logmidpt^2)) 

  # return lots of parts
  ifelse(method=="lm",retdf <- regdf,retdf <- df1)
  res <- list(pop.by.len=p.n.tbl,ind.by.len=with(df1,tapply(n,fLCat,sum,na.rm=TRUE)),sumdata=df2,regdata=retdf,quadratic=quadratic,probs=probs,Ws=Ws)
  ifelse(method=="lm", class(res) <- c("emplm","emp"), class(res) <- c("emprq","emp"))
  res
} 

#'@rdname emp
#'@method coef emp
#'@S3method coef emp
coef.emp <- function(object,...) {
  coef(object$Ws,...)
}

#'@rdname emp
#'@method summary emp
#'@S3method summary emp
summary.emp <- function(object,...) {
  summary(object$Ws,...)
}

#'@rdname emp
#'@method predict emp
#'@S3method predict emp
predict.emp <- function(object,...) {
  predict(object$Ws,...)
}

#'@rdname emp
#'@method anova emplm
#'@S3method anova emplm
anova.emplm <- function(object,...) {
  anova(object$Ws,...)
}

#'@rdname emp
#'@method plot emplm
#'@S3method plot emplm
plot.emplm <- function(x,pch=16,col.pop="rich",col.Ws="black",lwd.Ws=3,lty.Ws=1,
                       jitterX=TRUE,jitter.factor=3,...) {
  old.par <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.75,0));   on.exit(par(old.par))
  object <- x
  pops <- factor(unique(object$sumdata$regrnum))
  if (col.pop %in% paletteChoices()) col.pop <- chooseColors(col.pop,length(pops))
  ifelse(jitterX,x <- jitter(object$sumdata$logmidpt,factor=jitter.factor),x <- object$sumdata$logmidpt) 
  plot(object$sumdata$mn.logW~x,pch=pch,col=col.pop,xlab="log10(Length (mm))",ylab="mean log10(Weight (g))")
  if (!object$quadratic) abline(object$Ws,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
    else {
      x <- seq(min(object$regdata$logmidpt),max(object$regdata$logmidpt),length.out=500)
      y <- predict(object$Ws,data.frame(logmidpt=x))
      lines(y~x,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
    }
}

#'@rdname emp
#'@method fitPlot emplm
#'@S3method fitPlot emplm
fitPlot.emplm <- function(object,pch=16,col.pt="black",col.Ws="red",lwd.Ws=3,lty.Ws=1,
                 xlab="log10(midpt Length)",
                 ylab=paste(100*object$probs,"Percentile of mean log10(Weight)"),
                 main="EMP Equation Fit",...) {
  ifelse("emplm" %in% class(object),df <- object$regdata, df <- object$rawdata)
  plot(object$regdata$logwq~object$regdata$logmidpt,pch=pch,col=col.pt,xlab=xlab,ylab=ylab,main=main,...)
  if (!object$quadratic) abline(object$Ws,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
    else {
      x <- seq(min(object$regdata$logmidpt),max(object$regdata$logmidpt),length.out=500)
      y <- predict(object$Ws,data.frame(logmidpt=x))
      lines(y~x,col=col.Ws,lwd=lwd.Ws,lty=lty.Ws)
    }
}

#'@rdname emp
#'@method residPlot emplm
#'@S3method residPlot emplm
residPlot.emplm <- function(object,...) {
  residPlot(object$Ws)
}

#'@rdname emp
#'@method fitPlot emprq
#'@S3method fitPlot emprq
fitPlot.emprq <- function(object,pch=16,col.pop="rich",col.Ws="black",lwd.Ws=3,lty.Ws=1,
                          jitterX=TRUE,jitter.factor=3,...) {
  plot.emplm(object,pch=pch,col.pop=col.pop,col.Ws=col.Ws,lwd.Ws=lwd.Ws,lty.Ws=lty.Ws,jitterX=jitterX,jitter.factor=jitter.factor,...)
}
